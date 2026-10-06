/*
 * LWS features used by the HTTP adapter (libwebsockets.so.21, runtime >= 4.3.0;
 * verified with 4.5.8):
 *
 * - LWS_ROLE_H1, LWS_WITH_CLIENT, and LWS_WITH_SERVER: HTTP client requests and
 *   server transactions, header APIs, writable callbacks, and bounded bodies.
 * - LWS_ROLE_H2 when available: cleartext prior knowledge, TLS ALPN, concurrent
 *   child streams, flow control, and LWS-owned SETTINGS / GOAWAY handling.
 * - LWS_WITH_EXTERNAL_POLL: descriptor callbacks, lws_service_fd, nonblocking
 *   lws_service_tsi(-1), forced-service adjustment, and lws_cancel_service.
 *   One Scheme reactor owns service; callbacks copy events and never call Scheme.
 * - Optional LWS_WITH_TLS with OpenSSL: caller-provided client SSL_CTX and
 *   server vhost credentials. HTTP proxy settings use LWS's HTTP client API.
 *
 * This adapter configures no WebSocket extensions or LWS compression features;
 * HTTP content decoding is handled by Chezpp's body layer. It uses the poll
 * backend, not libuv / libev / libevent. Enabled builds link directly to LWS.
 * Capability checks live in lws_loader.c and use the dependency header feature macros.
 */

#include "../build-config.h"
#include "lws_http.h"

#include "lws_loader.h"
#include "../openssl_loader.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

extern void *chezpp_net_tls_context_native(uptr handle);
extern int chezpp_net_tls_context_verifies_peer(uptr handle);

/*
 * Callback ordering follows the backend's original libwebsockets 4.5.x audit:
 * protocol bind establishes H2 readiness, then
 * ESTABLISHED_CLIENT_HTTP publishes status and headers; RECEIVE_CLIENT_HTTP
 * and RECEIVE_CLIENT_HTTP_READ publish body bytes; CLIENT_HTTP_WRITEABLE is
 * the only request-body pull point; COMPLETED_CLIENT_HTTP publishes completion;
 * CLOSED_CLIENT_HTTP and CLIENT_CONNECTION_ERROR publish terminal failure;
 * CLIENT_HTTP_DROP_PROTOCOL detaches the WSI; explicit release frees the stream. Events are copied
 * while holding the context lock and drained FIFO by the serialized reactor.
 */

static struct lws_context *lws_http_lifetime_context;
static pthread_mutex_t lws_http_lifetime_mutex = PTHREAD_MUTEX_INITIALIZER;

static int ensure_lws_http_lifetime_context(void) {
  struct lws_context_creation_info information;
  pthread_mutex_lock(&lws_http_lifetime_mutex);
  if (lws_http_lifetime_context == NULL) {
    memset(&information, 0, sizeof(information));
    information.port = CONTEXT_PORT_NO_LISTEN;
#if defined(LWS_WITH_TLS)
    information.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
#endif
    lws_http_lifetime_context = lws_create_context(&information);
  }
  pthread_mutex_unlock(&lws_http_lifetime_mutex);
  return lws_http_lifetime_context != NULL;
}

__attribute__((destructor)) static void release_lws_http_lifetime_context(void) {
  pthread_mutex_lock(&lws_http_lifetime_mutex);

  if (lws_http_lifetime_context != NULL) {
    lws_context_destroy(lws_http_lifetime_context);
    lws_http_lifetime_context = NULL;
  }
  pthread_mutex_unlock(&lws_http_lifetime_mutex);
}
static uint64_t next_context_identity;
static uint64_t next_server_identity;

static lws_http_context *context_from_handle(uintptr_t handle) {
  return (lws_http_context *)handle;
}

static uint64_t fresh_identity(uint64_t *counter) {
  return __atomic_add_fetch(counter, 1, __ATOMIC_RELAXED);
}

static int set_nonblocking_close_on_exec(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  int fd_flags = fcntl(fd, F_GETFD, 0);
  if (flags < 0 || fd_flags < 0) return 0;
  if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) return 0;
  return fcntl(fd, F_SETFD, fd_flags | FD_CLOEXEC) == 0;
}

static const char *event_tag_name(lws_http_event_tag tag) {
  switch (tag) {
    case LWS_HTTP_EVENT_POLL_ADD:
      return "poll-add";
    case LWS_HTTP_EVENT_POLL_CHANGE:
      return "poll-change";
    case LWS_HTTP_EVENT_POLL_DELETE:
      return "poll-delete";
    case LWS_HTTP_EVENT_CONNECTED:
      return "connected";
    case LWS_HTTP_EVENT_HEADERS:
      return "headers";
    case LWS_HTTP_EVENT_READABLE:
      return "readable";
    case LWS_HTTP_EVENT_WRITABLE:
      return "writable";
    case LWS_HTTP_EVENT_COMPLETE:
      return "complete";
    case LWS_HTTP_EVENT_CLOSED:
      return "closed";
    case LWS_HTTP_EVENT_FAILED:
      return "failed";
    case LWS_HTTP_EVENT_RESET:
      return "reset";
    case LWS_HTTP_EVENT_GOAWAY:
      return "goaway";
    default:
      return "failed";
  }
}

static const char *protocol_name(lws_http_protocol protocol) {
  switch (protocol) {
    case LWS_HTTP_PROTOCOL_HTTP1:
      return "http1";
    case LWS_HTTP_PROTOCOL_HTTP2:
      return "http2";
    default:
      return "unknown";
  }
}

static const char *terminal_scope_name(lws_http_terminal_scope scope) {
  switch (scope) {
    case LWS_HTTP_TERMINAL_SCOPE_STREAM:
      return "stream";
    case LWS_HTTP_TERMINAL_SCOPE_CONNECTION:
      return "connection";
    default:
      return "none";
  }
}

static lws_http_event *event_acquire_locked(lws_http_context *context) {
  lws_http_event *event = context->event_free;
  if (event == NULL) {
    context->event_misses++;
    context->event_exhaustions++;
    return NULL;
  }
  context->event_free = event->next;
  event->next = NULL;
  context->event_in_use++;
  if (context->event_in_use > context->event_high_water)
    context->event_high_water = context->event_in_use;
  return event;
}

static void event_release_locked(lws_http_context *context,
                                 lws_http_event *event) {
  if (event->payload_length != 0)
    memset(event->payload, 0, event->payload_length);
  event->payload_length = 0;
  event->next = context->event_free;
  context->event_free = event;
  context->event_in_use--;
}

static int queue_event_metadata_locked(
    lws_http_context *context, lws_http_event_tag tag, uint64_t connection_id,
    uint64_t stream_id, uint64_t generation, int status, const void *payload,
    size_t payload_length, lws_http_protocol protocol, int reusable,
    uint32_t peer_h2_capacity, int peer_h2_capacity_known,
    lws_http_terminal_scope terminal_scope) {
  lws_http_event *event;
  if (payload_length > context->payload_capacity) {
    context->event_misses++;
    context->event_exhaustions++;
    return 0;
  }
  if (tag == LWS_HTTP_EVENT_READABLE &&
      context->queued_body_bytes + payload_length > context->body_byte_limit) {
    context->event_misses++;
    return 0;
  }
  event = event_acquire_locked(context);
  if (event == NULL) return 0;
  event->tag = tag;
  event->context_id = context->identity;
  event->connection_id = connection_id;
  event->stream_id = stream_id;
  event->generation = generation;
  event->status = status;
  event->protocol = protocol;
  event->reusable = reusable;
  event->peer_h2_capacity = peer_h2_capacity;
  event->peer_h2_capacity_known = peer_h2_capacity_known;
  event->terminal_scope = terminal_scope;
  event->payload_length = payload_length;
  if (payload_length != 0) memcpy(event->payload, payload, payload_length);
  if (context->event_tail == NULL)
    context->event_head = event;
  else
    context->event_tail->next = event;
  context->event_tail = event;
  if (tag == LWS_HTTP_EVENT_READABLE)
    context->queued_body_bytes += payload_length;
  return 1;
}

static int queue_event_locked(lws_http_context *context,
                              lws_http_event_tag tag, uint64_t connection_id,
                              uint64_t stream_id, uint64_t generation,
                              int status, const void *payload,
                              size_t payload_length) {
  return queue_event_metadata_locked(
      context, tag, connection_id, stream_id, generation, status, payload,
      payload_length, LWS_HTTP_PROTOCOL_UNKNOWN, 0, 0, 0,
      LWS_HTTP_TERMINAL_SCOPE_NONE);
}

static int terminal_event_tag(lws_http_event_tag tag) {
  return tag == LWS_HTTP_EVENT_COMPLETE || tag == LWS_HTTP_EVENT_CLOSED ||
         tag == LWS_HTTP_EVENT_FAILED || tag == LWS_HTTP_EVENT_RESET ||
         tag == LWS_HTTP_EVENT_GOAWAY;
}

static int queue_terminal_locked(lws_http_context *context,
                                 lws_http_stream *stream,
                                 lws_http_event_tag tag, int status,
                                 const void *payload, size_t payload_length,
                                 lws_http_protocol protocol, int reusable,
                                 uint32_t peer_h2_capacity,
                                 int peer_h2_capacity_known,
                                 lws_http_terminal_scope terminal_scope) {
  int queued;
  if (!terminal_event_tag(tag) || stream->terminal || stream->terminal_pending ||
      payload_length > context->payload_capacity)
    return 0;
  if (stream->pending_body_bytes != 0) {
    if (payload_length != 0)
      memcpy(stream->terminal_payload, payload, payload_length);
    stream->terminal_tag = tag;
    stream->terminal_status = status;
    stream->terminal_protocol = protocol;
    stream->terminal_reusable = reusable;
    stream->terminal_peer_h2_capacity = peer_h2_capacity;
    stream->terminal_peer_h2_capacity_known = peer_h2_capacity_known;
    stream->terminal_scope = terminal_scope;
    stream->terminal_payload_length = payload_length;
    stream->terminal_pending = 1;
    return 1;
  }
  queued = queue_event_metadata_locked(
      context, tag, stream->connection->identity, stream->identity,
      stream->generation, status, payload, payload_length, protocol, reusable,
      peer_h2_capacity, peer_h2_capacity_known, terminal_scope);
  if (queued) stream->terminal = 1;
  return queued;
}

static int flush_terminal_locked(lws_http_context *context,
                                 lws_http_stream *stream) {
  int queued;
  if (!stream->terminal_pending || stream->pending_body_bytes != 0) return 0;
  queued = queue_event_metadata_locked(
      context, stream->terminal_tag, stream->connection->identity,
      stream->identity, stream->generation, stream->terminal_status,
      stream->terminal_payload, stream->terminal_payload_length,
      stream->terminal_protocol, stream->terminal_reusable,
      stream->terminal_peer_h2_capacity,
      stream->terminal_peer_h2_capacity_known, stream->terminal_scope);
  if (!queued) return 0;
  if (stream->terminal_payload_length != 0)
    memset(stream->terminal_payload, 0, stream->terminal_payload_length);
  stream->terminal_payload_length = 0;
  stream->terminal_pending = 0;
  stream->terminal = 1;
  return 1;
}

static lws_http_connection *connection_find_locked(lws_http_context *context,
                                                    uint64_t identity) {
  size_t index;
  for (index = 0; index < context->connection_capacity; index++) {
    lws_http_connection *connection = &context->connections[index];
    if (connection->active && connection->identity == identity)
      return connection;
  }
  return NULL;
}

static lws_http_connection *connection_acquire_locked(
    lws_http_context *context, uint64_t identity, uint64_t generation) {
  lws_http_connection *connection = connection_find_locked(context, identity);
  if (connection != NULL) {
    if (generation < connection->generation) return NULL;
    return connection;
  }
  connection = context->connection_free;
  if (connection == NULL) return NULL;
  context->connection_free = connection->next_free;
  memset(connection, 0, sizeof(*connection));
  connection->identity = identity;
  connection->generation = generation;
  connection->active = 1;
  context->connection_in_use++;
  context->live_handle_count++;
  if (context->connection_in_use > context->connection_high_water)
    context->connection_high_water = context->connection_in_use;
  return connection;
}

static void connection_release_locked(lws_http_context *context,
                                      lws_http_connection *connection) {
  if (connection == NULL || !connection->active || connection->active_streams)
    return;
  memset(connection, 0, sizeof(*connection));
  connection->next_free = context->connection_free;
  context->connection_free = connection;
  context->connection_in_use--;
  context->live_handle_count--;
}

static lws_http_stream *stream_find_locked(lws_http_context *context,
                                           uint64_t connection_id,
                                           uint64_t stream_id) {
  size_t index;
  for (index = 0; index < context->stream_capacity; index++) {
    lws_http_stream *stream = &context->streams[index];
    if (stream->active && stream->identity == stream_id &&
        stream->connection != NULL &&
        stream->connection->identity == connection_id)
      return stream;
  }
  return NULL;
}

static void stream_release_locked(lws_http_context *context,
                                  lws_http_stream *stream) {
  lws_http_connection *connection;
  uint64_t connection_identity;
  uint64_t stream_identity;
  uint64_t generation;
  if (stream == NULL || !stream->active) return;
  connection = stream->connection;
  connection_identity = connection == NULL ? 0 : connection->identity;
  stream_identity = stream->identity;
  generation = stream->generation;
  if (stream->pending_body_bytes <= context->queued_body_bytes)
    context->queued_body_bytes -= stream->pending_body_bytes;
  else
    context->queued_body_bytes = 0;
  memset(stream->outbound, 0, context->payload_capacity + LWS_PRE);
  memset(stream->headers, 0, context->payload_capacity);
  memset(stream->terminal_payload, 0, context->payload_capacity);
  memset(stream->address, 0, sizeof(stream->address));
  memset(stream->host, 0, sizeof(stream->host));
  memset(stream->path, 0, sizeof(stream->path));
  memset(stream->method, 0, sizeof(stream->method));
  stream->connection = NULL;
  stream->connection_identity = connection_identity;
  stream->identity = stream_identity;
  stream->generation = generation;
  stream->wsi = NULL;
  stream->outbound_length = 0;
  stream->submitted_body_length = 0;
  stream->headers_length = 0;
  stream->terminal_payload_length = 0;
  stream->pending_body_bytes = 0;
  stream->outbound_final = 0;
  stream->has_request_body = 0;
  stream->active = 0;
  stream->terminal = 0;
  stream->terminal_pending = 0;
  stream->terminal_tag = 0;
  stream->terminal_status = 0;
  stream->terminal_protocol = LWS_HTTP_PROTOCOL_UNKNOWN;
  stream->terminal_reusable = 0;
  stream->terminal_peer_h2_capacity = 0;
  stream->terminal_peer_h2_capacity_known = 0;
  stream->terminal_scope = LWS_HTTP_TERMINAL_SCOPE_NONE;
  stream->failure_pending = 0;
  stream->failure_status = 0;
  stream->server_stream = 0;
  stream->server_request_complete = 0;
  stream->server_body_remaining = SIZE_MAX;
  stream->response_status = 0;
  stream->response_headers_sent = 0;
  stream->observed_protocol = LWS_HTTP_PROTOCOL_UNKNOWN;
  stream->reusable = 0;
  stream->peer_h2_capacity = 0;
  stream->peer_h2_capacity_known = 0;
  stream->next_free = context->stream_free;
  context->stream_free = stream;
  context->stream_in_use--;
  context->live_handle_count--;
  if (connection != NULL && connection->active_streams)
    connection->active_streams--;
  connection_release_locked(context, connection);
}

static void stream_remove_free_locked(lws_http_context *context,
                                      lws_http_stream *stream) {
  lws_http_stream *current = context->stream_free;
  lws_http_stream *previous = NULL;
  while (current != NULL) {
    if (current == stream) {
      if (previous == NULL)
        context->stream_free = current->next_free;
      else
        previous->next_free = current->next_free;
      return;
    }
    previous = current;
    current = current->next_free;
  }
}

static lws_http_stream *stream_acquire_locked(lws_http_context *context,
                                              uint64_t connection_id,
                                              uint64_t stream_id,
                                              uint64_t generation) {
  lws_http_stream *stream =
      stream_find_locked(context, connection_id, stream_id);
  lws_http_connection *connection;
  size_t index;
  if (stream != NULL) {
    if (stream->generation != generation) return NULL;
    return stream->terminal ? NULL : stream;
  }
  stream = NULL;
  for (index = 0; index < context->stream_capacity; index++) {
    lws_http_stream *candidate = &context->streams[index];
    if (!candidate->active && candidate->identity == stream_id &&
        candidate->connection_identity == connection_id &&
        (candidate->identity != 0 || candidate->generation != 0)) {
      if (generation <= candidate->generation) return NULL;
      stream = candidate;
      break;
    }
  }
  connection = connection_acquire_locked(context, connection_id, generation);
  if (connection == NULL) return NULL;
  if (context->stream_free == NULL) {
    connection_release_locked(context, connection);
    return NULL;
  }
  if (stream == NULL) {
    stream = context->stream_free;
    context->stream_free = stream->next_free;
  } else {
    stream_remove_free_locked(context, stream);
  }
  memset(stream, 0, sizeof(*stream));
  stream->outbound = context->stream_payloads +
                     (size_t)(stream - context->streams) *
                         (context->payload_capacity + LWS_PRE);
  stream->headers = context->stream_headers +
                    (size_t)(stream - context->streams) *
                        context->payload_capacity;
  stream->terminal_payload = context->stream_terminal_payloads +
                             (size_t)(stream - context->streams) *
                                 context->payload_capacity;
  stream->connection = connection;
  stream->connection_identity = connection_id;
  stream->identity = stream_id;
  stream->generation = generation;
  stream->active = 1;
  connection->active_streams++;
  context->stream_in_use++;
  context->live_handle_count++;
  if (context->stream_in_use > context->stream_high_water)
    context->stream_high_water = context->stream_in_use;
  return stream;
}

static lws_poll_entry *poll_find_locked(lws_http_context *context, int fd) {
  size_t index;
  for (index = 0; index < context->poll_capacity; index++) {
    if (context->poll_entries[index].active &&
        context->poll_entries[index].fd == fd)
      return &context->poll_entries[index];
  }
  return NULL;
}

static int poll_update_locked(lws_http_context *context,
                              lws_http_event_tag tag, int fd, int events,
                              int publish) {
  lws_poll_entry *entry = poll_find_locked(context, fd);
  if (tag == LWS_HTTP_EVENT_POLL_DELETE) {
    if (entry == NULL) return 0;
    entry->active = 0;
    entry->next_free = context->poll_free;
    context->poll_free = entry;
    context->poll_in_use--;
  } else if (entry == NULL) {
    if (tag != LWS_HTTP_EVENT_POLL_ADD || context->poll_free == NULL) return 0;
    entry = context->poll_free;
    context->poll_free = entry->next_free;
    entry->fd = fd;
    entry->events = (short)events;
    entry->active = 1;
    context->poll_in_use++;
    if (context->poll_in_use > context->poll_high_water)
      context->poll_high_water = context->poll_in_use;
  } else {
    entry->events = (short)events;
  }
  return !publish || queue_event_locked(context, tag, 0, 0, 0, events, NULL, 0);
}

static lws_http_context *callback_context(struct lws *wsi) {
  struct lws_context *lws_context;
  if (wsi == NULL)
    return NULL;
  lws_context = lws_get_context(wsi);
  return lws_context == NULL
             ? NULL
             : (lws_http_context *)lws_context_user(lws_context);
}

static lws_http_stream *callback_stream(struct lws *wsi, void *user) {
  lws_http_stream *stream = NULL;
  if (wsi != NULL)
    stream = (lws_http_stream *)lws_get_opaque_user_data(wsi);
  if (stream == NULL && user != NULL) stream = *(lws_http_stream **)user;
  return stream;
}

static lws_http_protocol observe_protocol(struct lws *wsi) {
  if (wsi == NULL) return LWS_HTTP_PROTOCOL_UNKNOWN;
#if (defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2))
  return lws_get_network_wsi(wsi) == wsi ? LWS_HTTP_PROTOCOL_HTTP1
                                       : LWS_HTTP_PROTOCOL_HTTP2;
#else
  return LWS_HTTP_PROTOCOL_HTTP1;
#endif
}

static int observe_http1_reusable(struct lws *wsi) {
  char connection[32];
  int copied;
    copied = lws_hdr_copy(wsi, connection, sizeof(connection), WSI_TOKEN_CONNECTION);
  return copied <= 0 || strcasecmp(connection, "close") != 0;
}

static lws_http_terminal_scope stream_terminal_scope(
    lws_http_stream *stream, lws_http_event_tag tag) {
  if (stream->observed_protocol != LWS_HTTP_PROTOCOL_HTTP2)
    return LWS_HTTP_TERMINAL_SCOPE_STREAM;
  return tag == LWS_HTTP_EVENT_GOAWAY || tag == LWS_HTTP_EVENT_FAILED
             ? LWS_HTTP_TERMINAL_SCOPE_CONNECTION
             : LWS_HTTP_TERMINAL_SCOPE_STREAM;
}

static int callback_queue_stream(lws_http_context *context,
                                 lws_http_stream *stream,
                                 lws_http_event_tag tag, int status,
                                 const void *payload, size_t length) {
  int queued;
  if (context == NULL || stream == NULL || !stream->active || stream->terminal)
    return 0;
  pthread_mutex_lock(&context->lock);
  /* A close callback may follow completion before Scheme acknowledges its final body chunk. */
  if (terminal_event_tag(tag) && stream->terminal_pending) {
    pthread_mutex_unlock(&context->lock);
    return 1;
  }
  queued = terminal_event_tag(tag)
               ? queue_terminal_locked(context, stream, tag, status, payload,
                                       length, stream->observed_protocol,
                                       tag == LWS_HTTP_EVENT_COMPLETE
                                           ? stream->reusable
                                           : 0,
                                       stream->peer_h2_capacity,
                                       stream->peer_h2_capacity_known,
                                       stream_terminal_scope(stream, tag))
               : queue_event_metadata_locked(
                     context, tag, stream->connection->identity,
                     stream->identity, stream->generation, status, payload,
                     length, stream->observed_protocol, 0,
                     stream->peer_h2_capacity,
                     stream->peer_h2_capacity_known,
                     LWS_HTTP_TERMINAL_SCOPE_NONE);
  if (!queued && tag != LWS_HTTP_EVENT_FAILED) {
    stream->terminal = 1;
    stream->failure_pending = 1;
    stream->failure_status = ENOBUFS;
  }
  pthread_mutex_unlock(&context->lock);
  return queued;
}

typedef struct custom_header_copy_state {
  lws_http_context *context;
  struct lws *wsi;
  size_t used;
  int overflow;
} custom_header_copy_state;

static void copy_custom_header_name(const char *name, int name_length,
                                    void *opaque) {
  custom_header_copy_state *state = (custom_header_copy_state *)opaque;

  size_t clean_length;
  size_t remaining;
  int copied;
  if (state == NULL || name == NULL || name_length <= 0)
    return;
  clean_length = (size_t)name_length;
  if (clean_length != 0 && name[clean_length - 1] == ':') clean_length--;
  if (state->used + clean_length + 2 > state->context->payload_capacity) {
    state->overflow = 1;
    return;
  }
  remaining = state->context->payload_capacity - state->used - clean_length - 1;
  copied = lws_hdr_custom_copy(state->wsi,
                   (char *)state->context->drain_buffer + state->used +
                       clean_length + 1,
                   (int)remaining, name, name_length);
  if (copied < 0) {
    state->overflow = 1;
    return;
  }
  memcpy(state->context->drain_buffer + state->used, name, clean_length);
  state->context->drain_buffer[state->used + clean_length] = 0;
  state->used += clean_length + 1 + (size_t)copied + 1;
}

static size_t copy_http_headers(lws_http_context *context,
                                struct lws *wsi, size_t used) {
  size_t index;
  for (index = 0; index < WSI_TOKEN_COUNT; index++) {
    const unsigned char *name = lws_token_to_string((enum lws_token_indexes)index);
    size_t name_length;
    int fragment = 0;
    if (name == NULL || name[0] == ':') continue;
    name_length = strlen((const char *)name);
    if (name_length == 0 || name[name_length - 1] != ':') continue;
    name_length--;
    for (;;) {
      size_t remaining;
      int copied;
      if (used + name_length + 2 > context->payload_capacity) {
        return SIZE_MAX;
      }
      remaining = context->payload_capacity - used - name_length - 1;
      copied = lws_hdr_copy_fragment(
          wsi, (char *)context->drain_buffer + used + name_length + 1,
          (int)remaining, (enum lws_token_indexes)index, fragment);
      if (copied == -1) break;
      if (copied < -1) return SIZE_MAX;
      {
        unsigned char *value = context->drain_buffer + used + name_length + 1;
        size_t begin = 0;
        size_t finish = (size_t)copied;
        /* LWS may include separator whitespace in repeated field fragments. */
        while (begin < finish && (value[begin] == ' ' || value[begin] == '\t')) begin++;
        while (finish > begin && (value[finish - 1] == ' ' || value[finish - 1] == '\t'))
          finish--;
        copied = (int)(finish - begin);
        memmove(value, value + begin, (size_t)copied);
        value[copied] = 0;
      }
      memcpy(context->drain_buffer + used, name, name_length);
      context->drain_buffer[used + name_length] = 0;
      used += name_length + 1 + (size_t)copied + 1;
      fragment++;
    }
  }
  {
    custom_header_copy_state state = {context, wsi, used, 0};
    (void)lws_hdr_custom_name_foreach(wsi, copy_custom_header_name, &state);
    used = state.used;
    if (state.overflow) return SIZE_MAX;
  }
  return used;
}

static size_t copy_server_request(lws_http_context *context, struct lws *wsi,
                                  const void *path, size_t path_length) {
  int method_length;
  size_t used;
  if (path == NULL || path_length + 3 > context->payload_capacity)
    return SIZE_MAX;
  method_length = lws_hdr_copy(wsi, (char *)context->drain_buffer,
                          (int)context->payload_capacity,
                          WSI_TOKEN_HTTP_COLON_METHOD);
  if (method_length <= 0) {
    if (lws_hdr_total_length(wsi, WSI_TOKEN_POST_URI) > 0) {
      method_length = 4;
      memcpy(context->drain_buffer, "POST", 4);
    } else {
      method_length = 3;
      memcpy(context->drain_buffer, "GET", 3);
    }
  }
  used = (size_t)method_length;
  if (used + path_length + 2 > context->payload_capacity) return SIZE_MAX;
  context->drain_buffer[used++] = 0;
  memcpy(context->drain_buffer + used, path, path_length);
  used += path_length;
  context->drain_buffer[used++] = 0;
  return copy_http_headers(context, wsi, used);
}

static int append_request_headers(lws_http_stream *stream, struct lws *wsi,
                                  unsigned char **cursor,
                                  unsigned char *end, int *length_present) {
  size_t offset = 0;
    while (offset < stream->headers_length) {
    const unsigned char *name = stream->headers + offset;
    size_t name_length = strnlen((const char *)name,
                                 stream->headers_length - offset);
    const unsigned char *value;
    size_t value_length;
    if (offset + name_length >= stream->headers_length) return -1;
    offset += name_length + 1;
    value = stream->headers + offset;
    value_length = strnlen((const char *)value,
                           stream->headers_length - offset);
    if (offset + value_length >= stream->headers_length) return -1;
    if (!stream->server_stream && (strcasecmp((const char *)name, "host") == 0 ||
        strcasecmp((const char *)name, "connection") == 0 ||
        (!stream->has_request_body &&
         strcasecmp((const char *)name, "content-length") == 0))) {
      offset += value_length + 1;
      continue;
    }
    if (strcasecmp((const char *)name, "content-length") == 0) {
      if (length_present != NULL) *length_present = 1;
      if (lws_add_http_header_by_token(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH, value,
                       (int)value_length, cursor, end) != 0)
        return -1;
      offset += value_length + 1;
      continue;
    }
    {
      /* Header construction precedes terminal delivery; reuse its bounded scratch buffer. */
      unsigned char *colon_name = stream->terminal_payload;
      int result;
      memcpy(colon_name, name, name_length);
      colon_name[name_length] = ':';
      colon_name[name_length + 1] = 0;
      result = lws_add_http_header_by_name(wsi, colon_name, value, (int)value_length, cursor, end);
      memset(colon_name, 0, name_length + 2);
      if (result != 0) return -1;
    }
    offset += value_length + 1;
  }
  return 0;
}

static int initial_body_is_complete(lws_http_stream *stream,
                                    size_t initial_body_length) {
  size_t offset = 0;
  while (offset < stream->headers_length) {
    const char *name = (const char *)stream->headers + offset;
    size_t name_length =
        strnlen(name, stream->headers_length - offset);
    const char *value;
    size_t value_length;
    unsigned long long expected;
    char *end;
    if (offset + name_length >= stream->headers_length) return 0;
    offset += name_length + 1;
    value = (const char *)stream->headers + offset;
    value_length = strnlen(value, stream->headers_length - offset);
    if (offset + value_length >= stream->headers_length) return 0;
    if (name_length == 14 && strcasecmp(name, "content-length") == 0) {
      errno = 0;
      expected = strtoull(value, &end, 10);
      return errno == 0 && end == value + value_length &&
             expected == initial_body_length;
    }
    offset += value_length + 1;
  }
  return 0;
}

static int lws_http_callback(struct lws *wsi,
                             enum lws_callback_reasons reason, void *user,
                             void *input, size_t length) {
  lws_http_context *context = callback_context(wsi);
  lws_http_stream *stream = callback_stream(wsi, user);
  switch (reason) {
#if defined(LWS_WITH_TLS)
    case LWS_CALLBACK_OPENSSL_LOAD_EXTRA_SERVER_VERIFY_CERTS:
#if defined(LWS_WITH_TLS) && CHEZPP_WITH_OPENSSL && !defined(LWS_WITH_MBEDTLS)
      if (context != NULL && context->server_tls_context_handle != 0) {
        SSL_CTX *source = chezpp_net_tls_context_native(context->server_tls_context_handle);
        SSL_CTX *destination = user;
        X509 *certificate = SSL_CTX_get0_certificate(source);
        EVP_PKEY *key = SSL_CTX_get0_privatekey(source);
        if (certificate == NULL || key == NULL ||
            SSL_CTX_use_certificate(destination, certificate) != 1 ||
            SSL_CTX_use_PrivateKey(destination, key) != 1)
          return -1;
      }
#endif
      return 0;
#endif
    case LWS_CALLBACK_ADD_POLL_FD:
    case LWS_CALLBACK_CHANGE_MODE_POLL_FD:
    case LWS_CALLBACK_DEL_POLL_FD: {
      struct lws_pollargs *arguments = (struct lws_pollargs *)input;
      lws_http_event_tag tag = reason == LWS_CALLBACK_ADD_POLL_FD
                                   ? LWS_HTTP_EVENT_POLL_ADD
                                   : reason == LWS_CALLBACK_CHANGE_MODE_POLL_FD
                                         ? LWS_HTTP_EVENT_POLL_CHANGE
                                         : LWS_HTTP_EVENT_POLL_DELETE;
      if (context == NULL || arguments == NULL) return 0;
      pthread_mutex_lock(&context->lock);
      (void)poll_update_locked(context, tag, arguments->fd, arguments->events,
                               !context->initializing);
      pthread_mutex_unlock(&context->lock);
      return 0;
    }
    case LWS_CALLBACK_CLIENT_HTTP_BIND_PROTOCOL:
      {
        lws_http_protocol protocol = observe_protocol(wsi);
        if (context != NULL && stream != NULL &&
            protocol == LWS_HTTP_PROTOCOL_HTTP2 && !stream->h2_ready) {
          stream->observed_protocol = protocol;
          stream->h2_ready = 1;
          (void)callback_queue_stream(context, stream,
                                      LWS_HTTP_EVENT_CONNECTED,
                                      CHEZPP_LWS_HTTP_STATUS_H2_READY,
                                      NULL, 0);
        }
      }
      return 0;
    case LWS_CALLBACK_CLIENT_APPEND_HANDSHAKE_HEADER: {
      unsigned char **cursor = (unsigned char **)input;

      if (stream == NULL || cursor == NULL || *cursor == NULL) return -1;
      if (append_request_headers(stream, wsi, cursor, *cursor + length, NULL) != 0)
        return -1;
      if (!stream->has_request_body) return 0;

      lws_client_http_body_pending(wsi, 1);
      {
        int result = lws_callback_on_writable(wsi);
        if (result < 0) return -1;
        return 0;
      }
    }
    case LWS_CALLBACK_ESTABLISHED_CLIENT_HTTP: {
      int status = 0;
      size_t headers_length;

      status = lws_http_client_http_response(wsi);
      stream->observed_protocol = observe_protocol(wsi);
      stream->reusable =
          stream->observed_protocol == LWS_HTTP_PROTOCOL_HTTP1
              ? observe_http1_reusable(wsi)
              : 0;
      headers_length = copy_http_headers(context, wsi, 0);
      if (headers_length == SIZE_MAX) {
        (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_FAILED,
                                    ENOBUFS, NULL, 0);
        return -1;
      }
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_CONNECTED,
                                  status, NULL, 0);
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_HEADERS,
                                  status, context->drain_buffer,
                                  headers_length);
      return 0;
    }
    case LWS_CALLBACK_RECEIVE_CLIENT_HTTP: {
      char *buffer;
      int available;
      int result;
      if (context == NULL || stream == NULL || stream->terminal ||
          stream->pending_body_bytes)
        return 0;

          buffer = (char *)context->drain_buffer + LWS_PRE;
      available = (int)context->payload_capacity;
      result = lws_http_client_read(wsi, &buffer, &available);
      if (result >= 0 && available > 0 && buffer != NULL) {
        if (!callback_queue_stream(context, stream, LWS_HTTP_EVENT_READABLE,
                                   0, buffer, (size_t)available))
          return -1;
        pthread_mutex_lock(&context->lock);
        stream->pending_body_bytes += (size_t)available;
        pthread_mutex_unlock(&context->lock);
      }
      /* A positive lws_http_client_read result is progress metadata, not a
       * callback failure.  Returning it from the callback makes LWS abort the
       * stream (the raw chunked path surfaced this as native status 103).
       * Only negative results should propagate as callback errors. */
      return result < 0 ? result : 0;
    }
    case LWS_CALLBACK_RECEIVE_CLIENT_HTTP_READ:
      if (context == NULL || stream == NULL) return 0;
      pthread_mutex_lock(&context->lock);
      if (!queue_event_metadata_locked(
              context, LWS_HTTP_EVENT_READABLE,
              stream->connection->identity, stream->identity,
              stream->generation, 0, input, length,
              stream->observed_protocol, 0, stream->peer_h2_capacity,
              stream->peer_h2_capacity_known,
              LWS_HTTP_TERMINAL_SCOPE_NONE)) {
        pthread_mutex_unlock(&context->lock);
        return 0;
      }
      stream->pending_body_bytes += length;
      pthread_mutex_unlock(&context->lock);
      {
        lws_rx_flow_control(wsi, 0);
      }
      return 0;
    case LWS_CALLBACK_CLIENT_HTTP_WRITEABLE: {
      lws_http_protocol protocol;
      size_t outbound_length;
      int final_chunk;
      int written = 0;
      if (context == NULL || stream == NULL) return 0;
      protocol = observe_protocol(wsi);
      if (protocol == LWS_HTTP_PROTOCOL_HTTP2 && !stream->h2_ready) {
        stream->observed_protocol = protocol;
        stream->h2_ready = 1;
        (void)callback_queue_stream(context, stream,
                                    LWS_HTTP_EVENT_CONNECTED,
                                    CHEZPP_LWS_HTTP_STATUS_H2_READY,
                                    NULL, 0);
      }
      if (!stream->has_request_body) return 0;
      pthread_mutex_lock(&context->lock);
      outbound_length = stream->outbound_length;
      final_chunk = stream->outbound_final;
      pthread_mutex_unlock(&context->lock);
      if (outbound_length != 0) {
                written = lws_write(wsi, stream->outbound + LWS_PRE, outbound_length,
                           final_chunk ? LWS_WRITE_HTTP_FINAL : LWS_WRITE_HTTP);
        if (written < 0) return -1;
        pthread_mutex_lock(&context->lock);
        memset(stream->outbound + LWS_PRE, 0, stream->outbound_length);
        stream->outbound_length = 0;
        pthread_mutex_unlock(&context->lock);
      }

      if (final_chunk) lws_client_http_body_pending(wsi, 0);
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE,
                                  written, NULL, 0);
      return 0;
    }
    case LWS_CALLBACK_COMPLETED_CLIENT_HTTP:
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_COMPLETE, 0,
                                  NULL, 0);
      /* Client completion is owned by LWS; the server completion helper can close its network WSI. */
      return 0;
    case LWS_CALLBACK_CLOSED_CLIENT_HTTP:
    case LWS_CALLBACK_CLOSED_HTTP:
      if (stream != NULL) stream->reusable = 0;
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_CLOSED, 0,
                                  NULL, 0);
      return 0;
    case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
      if (input != NULL && length == 0)
        length = strnlen((const char *)input,
                         context == NULL ? 0 : context->payload_capacity);
      if (stream != NULL) stream->reusable = 0;
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_FAILED,
                                  ECONNABORTED, input, length);
      return 0;
    case LWS_CALLBACK_HTTP:
      if (context == NULL) return 0;
      if (stream == NULL || stream->terminal) {
        uint64_t identity = fresh_identity(&next_server_identity);
        pthread_mutex_lock(&context->lock);
        stream = stream_acquire_locked(context, identity, identity, 1);
        if (stream != NULL) {
          stream->wsi = wsi;
          stream->connection->wsi = wsi;
          stream->server_stream = 1;
          if (user != NULL) *(lws_http_stream **)user = stream;
        }
        pthread_mutex_unlock(&context->lock);
      }
      {
        int has_body = (lws_hdr_total_length(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH) > 0 ||
                        lws_hdr_total_length(wsi, WSI_TOKEN_HTTP_TRANSFER_ENCODING) > 0);
        if (stream != NULL) stream->server_request_complete = !has_body;
        if (stream != NULL) {
          char content_length[32];
          stream->server_body_remaining = SIZE_MAX;
          if (lws_hdr_total_length(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH) > 0) {
            char *end;
            unsigned long long count;
            if (lws_hdr_copy(wsi, content_length, sizeof(content_length),
                        WSI_TOKEN_HTTP_CONTENT_LENGTH) <= 0)
              return -1;
            errno = 0;
            count = strtoull(content_length, &end, 10);
            if (errno != 0 || *end != '\0' || count > SIZE_MAX) return -1;
            stream->server_body_remaining = (size_t)count;
          }
        }
        /* LWS 4.5's HTTP/1 server parser forwards transfer-coded bodies verbatim and
         * cannot signal their completion. Fail before exposing an unfinishable request. */
        if (observe_protocol(wsi) != LWS_HTTP_PROTOCOL_HTTP2 &&
            lws_hdr_total_length(wsi, WSI_TOKEN_HTTP_TRANSFER_ENCODING) > 0)
          return -1;
        size_t request_length = copy_server_request(context, wsi, input, length);
        if (request_length == SIZE_MAX ||
            !callback_queue_stream(context, stream, LWS_HTTP_EVENT_HEADERS, has_body,
                                   context->drain_buffer, request_length))
          return -1;
        if (!has_body && observe_protocol(wsi) == LWS_HTTP_PROTOCOL_HTTP1) {
          lws_rx_flow_control(wsi, 0);
        }
      }
      return 0;
    case LWS_CALLBACK_HTTP_BODY:
      if (context == NULL || stream == NULL) return 0;
      /* Reject LWS versions that include a pipelined request in the preceding body. */
      if (stream->server_body_remaining != SIZE_MAX) {
        if (length > stream->server_body_remaining) return -1;
        stream->server_body_remaining -= length;
      }
      {
      int queued;

      pthread_mutex_lock(&context->lock);
      queued = queue_event_locked(context, LWS_HTTP_EVENT_READABLE,
                             stream->connection->identity, stream->identity,
                             stream->generation, 0, input, length);
      if (queued) {
        stream->pending_body_bytes += length;
      }
      pthread_mutex_unlock(&context->lock);
      if (!queued) return -1;
      lws_rx_flow_control(wsi, 0);
      return 0;
      }
    case LWS_CALLBACK_HTTP_BODY_COMPLETION:
      if (context != NULL && stream != NULL && stream->server_stream) {
        stream->server_request_complete = 1;
        (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_HEADERS, -1,
                                    NULL, 0);
        if (observe_protocol(wsi) == LWS_HTTP_PROTOCOL_HTTP1) {
          lws_rx_flow_control(wsi, 0);
        }
      }
      return 0;
    case LWS_CALLBACK_HTTP_WRITEABLE:
      if (context != NULL && stream != NULL && stream->outbound_length <=
                                                   context->payload_capacity) {
        int written = 0;
        if (!stream->response_headers_sent) {
          unsigned char *start = context->drain_buffer + LWS_PRE;
          unsigned char *cursor = start;
          unsigned char *end = context->drain_buffer + LWS_PRE +
                               context->payload_capacity;
          int length_present = 0;

          if (lws_add_http_header_status(wsi, (unsigned int)stream->response_status, &cursor,
                        end) != 0 ||
              append_request_headers(stream, wsi, &cursor, end, &length_present) != 0)
            return -1;
          if (!length_present && stream->outbound_final) {
            char body_length[32];
            int digits = snprintf(body_length, sizeof(body_length), "%zu", stream->outbound_length);
            if (lws_add_http_header_by_token(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH,
                             (unsigned char *)body_length, digits, &cursor, end) != 0)
              return -1;
          }
          if (lws_finalize_write_http_header(wsi, start, &cursor, end) != 0) return -1;
          stream->response_headers_sent = 1;
        }
        if (stream->outbound_length != 0) {
                    written = lws_write(wsi, stream->outbound + LWS_PRE,
                             stream->outbound_length,
                             stream->outbound_final ? LWS_WRITE_HTTP_FINAL
                                                    : LWS_WRITE_HTTP);
          if (written < 0) return -1;
          memset(stream->outbound + LWS_PRE, 0, stream->outbound_length);
          stream->outbound_length = 0;
        }
        if (stream->outbound_final) {
          (void)callback_queue_stream(context, stream,
                                      LWS_HTTP_EVENT_COMPLETE, written, NULL,
                                      0);
          /* Completion may immediately dispatch the next pipelined request. Detach the
           * old request first so its cleanup cannot erase that request's user pointer. */
          pthread_mutex_lock(&context->lock);
          stream->wsi = NULL;
          if (user != NULL) *(lws_http_stream **)user = NULL;
          stream_release_locked(context, stream);
          pthread_mutex_unlock(&context->lock);
          if (lws_http_transaction_completed(wsi) != 0) return -1;
          {
            lws_rx_flow_control(wsi, 1);
          }
          return 0;
        }
        (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE,
                                    written, NULL, 0);
        if (stream->server_request_complete &&
            observe_protocol(wsi) == LWS_HTTP_PROTOCOL_HTTP1) {
          lws_rx_flow_control(wsi, 0);
        }
        return 0;
      }
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE, 0,
                                  NULL, 0);
      return 0;
    case LWS_CALLBACK_CLIENT_HTTP_DROP_PROTOCOL:
    case LWS_CALLBACK_HTTP_DROP_PROTOCOL:
      if (context != NULL && stream != NULL) {
        /* LWS may detach an H2 child without a separate CLOSED_CLIENT_HTTP callback. */
        (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_CLOSED,
                                    0, NULL, 0);
        pthread_mutex_lock(&context->lock);
        stream->wsi = NULL;
        if (!stream->terminal_pending) stream->terminal = 1;
        if (user != NULL) *(lws_http_stream **)user = NULL;
        pthread_mutex_unlock(&context->lock);
        {
          lws_set_opaque_user_data(wsi, NULL);
        }
      }
      return 0;
    default:
      return 0;
  }
}

static int initialize_pools(lws_http_context *context) {
  size_t index;
  size_t stream_slot_size = context->payload_capacity + LWS_PRE;
  context->poll_capacity = context->event_capacity < 16 ? 16 : context->event_capacity;
  context->connection_capacity = context->event_capacity;
  context->stream_capacity = context->event_capacity;
  context->signal_capacity = context->event_capacity;
  context->events = calloc(context->event_capacity, sizeof(*context->events));
  context->event_payloads =
      calloc(context->event_capacity, context->payload_capacity);
  context->poll_entries =
      calloc(context->poll_capacity, sizeof(*context->poll_entries));
  context->connections =
      calloc(context->connection_capacity, sizeof(*context->connections));
  context->streams = calloc(context->stream_capacity, sizeof(*context->streams));
  context->stream_payloads = calloc(context->stream_capacity, stream_slot_size);
  context->stream_headers =
      calloc(context->stream_capacity, context->payload_capacity);
  context->stream_terminal_payloads =
      calloc(context->stream_capacity, context->payload_capacity);
  context->signals = calloc(context->signal_capacity, sizeof(*context->signals));
  context->drain_buffer = calloc(1, stream_slot_size);
  if (context->events == NULL || context->event_payloads == NULL ||
      context->poll_entries == NULL || context->connections == NULL ||
      context->streams == NULL || context->stream_payloads == NULL ||
      context->stream_headers == NULL ||
      context->stream_terminal_payloads == NULL || context->signals == NULL ||
      context->drain_buffer == NULL)
    return 0;
  for (index = 0; index < context->event_capacity; index++) {
    context->events[index].payload =
        context->event_payloads + index * context->payload_capacity;
    context->events[index].next = context->event_free;
    context->event_free = &context->events[index];
  }
  for (index = 0; index < context->poll_capacity; index++) {
    context->poll_entries[index].next_free = context->poll_free;
    context->poll_free = &context->poll_entries[index];
  }
  for (index = 0; index < context->connection_capacity; index++) {
    context->connections[index].next_free = context->connection_free;
    context->connection_free = &context->connections[index];
  }
  for (index = 0; index < context->stream_capacity; index++) {
    context->streams[index].outbound =
        context->stream_payloads + index * stream_slot_size;
    context->streams[index].next_free = context->stream_free;
    context->stream_free = &context->streams[index];
  }
  for (index = 0; index < context->signal_capacity; index++) {
    context->signals[index].pipe[0] = -1;
    context->signals[index].pipe[1] = -1;
  }
  for (index = 0; index < context->signal_capacity; index++) {
    lws_http_signal *signal = &context->signals[index];
    signal->context = context;
    if (pipe(signal->pipe) != 0 ||
        !set_nonblocking_close_on_exec(signal->pipe[0]) ||
        !set_nonblocking_close_on_exec(signal->pipe[1]))
      return 0;
    signal->next_free = context->signal_free;
    context->signal_free = signal;
  }
  context->body_byte_limit = context->event_capacity * context->payload_capacity;
  return 1;
}

static void free_context_storage(lws_http_context *context) {
  size_t index;
  if (context->signals != NULL) {
    for (index = 0; index < context->signal_capacity; index++) {
      if (context->signals[index].pipe[0] >= 0)
        close(context->signals[index].pipe[0]);
      if (context->signals[index].pipe[1] >= 0)
        close(context->signals[index].pipe[1]);
    }
  }
  free(context->drain_buffer);
  free(context->signals);
  free(context->stream_terminal_payloads);
  free(context->stream_headers);
  free(context->stream_payloads);
  free(context->streams);
  free(context->connections);
  free(context->poll_entries);
  free(context->event_payloads);
  free(context->events);
  free(context);
}

static uintptr_t context_open(size_t event_capacity, size_t payload_capacity,
                              uintptr_t tls_context_handle,
                              const char *proxy_address, int proxy_port,
                              const char *interface_name, int listen_port) {
  lws_http_context *context;
  struct lws_context_creation_info information;

  if (event_capacity == 0 || payload_capacity == 0 ||
      event_capacity > UINT_MAX || payload_capacity > INT_MAX - LWS_PRE ||
      event_capacity > SIZE_MAX / payload_capacity ||
      !chezpp_lws_available())
    return 0;

  lws_set_log_level(0, NULL);
  if (!ensure_lws_http_lifetime_context()) return 0;
  context = calloc(1, sizeof(*context));
  if (context == NULL) return 0;
  context->identity = fresh_identity(&next_context_identity);
  context->event_capacity = event_capacity;
  context->payload_capacity = payload_capacity;
  context->tls_verify_peer = 1;
  context->wakeup_pipe[0] = -1;
  context->wakeup_pipe[1] = -1;
  if (pthread_mutex_init(&context->lock, NULL) != 0) {
    free(context);
    return 0;
  }
  if (!initialize_pools(context) || pipe(context->wakeup_pipe) != 0 ||
      !set_nonblocking_close_on_exec(context->wakeup_pipe[0]) ||
      !set_nonblocking_close_on_exec(context->wakeup_pipe[1])) {
    if (context->wakeup_pipe[0] >= 0) close(context->wakeup_pipe[0]);
    if (context->wakeup_pipe[1] >= 0) close(context->wakeup_pipe[1]);
    pthread_mutex_destroy(&context->lock);
    free_context_storage(context);
    return 0;
  }
  memset(context->protocols, 0, sizeof(context->protocols));
  context->protocols[0].name = "chezpp-http";
  context->protocols[0].callback = lws_http_callback;
  context->protocols[0].per_session_data_size = sizeof(lws_http_stream *);
  context->protocols[0].rx_buffer_size = payload_capacity;
  memset(&information, 0, sizeof(information));
  information.port = listen_port;
  information.iface = interface_name;
  information.protocols = context->protocols;
  information.user = context;
#if defined(LWS_WITH_TLS)
  information.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
#endif
#if (defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2))
  information.alpn = "h2,http/1.1";
#else
  information.alpn = "http/1.1";
#endif
  if (proxy_address != NULL && proxy_address[0] != '\0') {
    information.http_proxy_address = proxy_address;
    information.http_proxy_port = (unsigned int)proxy_port;
  }
#if defined(LWS_WITH_TLS) && CHEZPP_WITH_OPENSSL && !defined(LWS_WITH_MBEDTLS)
  if (tls_context_handle != 0) {
    if (listen_port >= 0) {
      information.options |= LWS_SERVER_OPTION_CREATE_VHOST_SSL_CTX;
      context->server_tls_context_handle = tls_context_handle;
    } else {
      information.provided_client_ssl_ctx =
          (SSL_CTX *)chezpp_net_tls_context_native((uptr)tls_context_handle);
    }
    context->tls_verify_peer =
        chezpp_net_tls_context_verifies_peer((uptr)tls_context_handle);
  }
#else
  (void)tls_context_handle;
#endif
  information.fd_limit_per_thread = (unsigned int)context->poll_capacity;
  context->initializing = 1;
  context->lws = lws_create_context(&information);
  context->initializing = 0;
  if (context->lws == NULL) {
    close(context->wakeup_pipe[0]);
    close(context->wakeup_pipe[1]);
    pthread_mutex_destroy(&context->lock);
    free_context_storage(context);
    return 0;
  }
  return (uintptr_t)context;
}

uintptr_t chezpp_lws_http_context_open(size_t event_capacity,
                                       size_t payload_capacity,
                                       uintptr_t tls_context_handle,
                                       const char *proxy_address,
                                       int proxy_port) {
  return context_open(event_capacity, payload_capacity, tls_context_handle,
                      proxy_address, proxy_port, NULL, CONTEXT_PORT_NO_LISTEN);
}

uintptr_t chezpp_lws_http_server_context_open(size_t event_capacity,
                                              size_t payload_capacity,
                                              const char *interface_name,
                                              int port,
                                              uintptr_t tls_context_handle) {
  if (interface_name == NULL || port <= 0 || port > 65535) return 0;
  return context_open(event_capacity, payload_capacity, tls_context_handle,
                      NULL, 0, interface_name, port);
}

void chezpp_lws_http_context_close(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);

  if (context == NULL) return;
  context->closing = 1;

  if (context->lws != NULL) lws_context_destroy(context->lws);
  context->lws = NULL;
  close(context->wakeup_pipe[0]);
  close(context->wakeup_pipe[1]);
  pthread_mutex_destroy(&context->lock);
  free_context_storage(context);
}

int chezpp_lws_http_context_wakeup_fd(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  return context == NULL ? -1 : context->wakeup_pipe[0];
}

ptr chezpp_lws_http_context_poll_snapshot(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  ptr snapshot;
  size_t index;
  size_t output_index = 0;
  if (context == NULL) return Sfalse;
  pthread_mutex_lock(&context->lock);
  snapshot = Smake_vector((iptr)context->poll_in_use, Sfalse);
  for (index = 0; index < context->poll_capacity; index++) {
    lws_poll_entry *entry = &context->poll_entries[index];
    if (entry->active) {
      ptr item = Smake_vector(2, Sfalse);
      Svector_set(item, 0, Sinteger(entry->fd));
      Svector_set(item, 1, Sinteger(entry->events));
      Svector_set(snapshot, output_index++, item);
    }
  }
  pthread_mutex_unlock(&context->lock);
  return snapshot;
}

int chezpp_lws_http_context_service_fd(uintptr_t context_handle, int fd,
                                       int revents) {
  lws_http_context *context = context_from_handle(context_handle);

  struct lws_pollfd poll_descriptor;
  if (context == NULL || context->lws == NULL) return -1;
  if (fd == context->wakeup_pipe[0]) {
    unsigned char bytes[64];
    while (read(fd, bytes, sizeof(bytes)) > 0) {
    }
    return 0;
  }
  if (fd < 0) {
    return lws_service_tsi(context->lws, -1, 0);
  }

  memset(&poll_descriptor, 0, sizeof(poll_descriptor));
  poll_descriptor.fd = fd;
  pthread_mutex_lock(&context->lock);
  {
    lws_poll_entry *entry = poll_find_locked(context, fd);
    poll_descriptor.events = entry == NULL ? 0 : entry->events;
  }
  pthread_mutex_unlock(&context->lock);
  poll_descriptor.revents = (short)revents;
  if (revents & POLLOUT) {
    size_t index;
    /* LWS 4.5's H1 role suppresses POLLOUT while RX is paused. Unpause only for
     * the observed write-ready event, keeping buffered pipelined input deferred. */
    for (index = 0; index < context->stream_capacity; index++) {
        lws_http_stream *stream = &context->streams[index];
        if (stream->active && stream->server_stream && stream->server_request_complete &&
            stream->wsi != NULL && lws_get_socket_fd(stream->wsi) == fd &&
            observe_protocol(stream->wsi) == LWS_HTTP_PROTOCOL_HTTP1) {
          lws_rx_flow_control(stream->wsi, 1);
          poll_descriptor.revents &= (short)~POLLIN;
          break;
        }
      }
  }
  return lws_service_fd(context->lws, &poll_descriptor);
}

static ptr event_to_scheme_locked(lws_http_context *context,
                                  lws_http_event *event) {
  ptr result = Smake_vector(8, Sfalse);
  ptr payload = Smake_bytevector((iptr)event->payload_length, 0);
  ptr metadata = Smake_vector(4, Sfalse);
  if (event->payload_length != 0)
    memcpy(Sbytevector_data(payload), event->payload, event->payload_length);
  Svector_set(result, 0, Sstring_to_symbol(event_tag_name(event->tag)));
  Svector_set(result, 1, Sunsigned64(event->context_id));
  Svector_set(result, 2, Sunsigned64(event->connection_id));
  Svector_set(result, 3, Sunsigned64(event->stream_id));
  Svector_set(result, 4, Sunsigned64(event->generation));
  Svector_set(result, 5, Sinteger(event->status));
  Svector_set(result, 6, payload);
  Svector_set(metadata, 0,
              Sstring_to_symbol(protocol_name(event->protocol)));
  Svector_set(metadata, 1, event->reusable ? Strue : Sfalse);
  if (event->peer_h2_capacity_known)
    Svector_set(metadata, 2, Sunsigned32(event->peer_h2_capacity));
  Svector_set(metadata, 3,
              Sstring_to_symbol(terminal_scope_name(event->terminal_scope)));
  Svector_set(result, 7, metadata);
  event_release_locked(context, event);
  return result;
}

ptr chezpp_lws_http_context_next_event(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_event *event;
  ptr result;
  if (context == NULL) return Sfalse;
  pthread_mutex_lock(&context->lock);
  event = context->event_head;
  if (event == NULL) {
    pthread_mutex_unlock(&context->lock);
    return Sfalse;
  }
  context->event_head = event->next;
  if (context->event_head == NULL) context->event_tail = NULL;
  result = event_to_scheme_locked(context, event);
  {
    size_t index;
    for (index = 0; index < context->stream_capacity; index++) {
      lws_http_stream *stream = &context->streams[index];
      if (stream->active && stream->failure_pending &&
          queue_event_locked(context, LWS_HTTP_EVENT_FAILED,
                             stream->connection->identity, stream->identity,
                             stream->generation, stream->failure_status,
                             NULL, 0)) {
        stream->failure_pending = 0;
        break;
      }
    }
  }
  pthread_mutex_unlock(&context->lock);
  return result;
}

int chezpp_lws_http_context_timeout_ms(uintptr_t context_handle,
                                       int maximum_timeout_ms) {
  lws_http_context *context = context_from_handle(context_handle);

  int adjustment;
  if (context == NULL || context->lws == NULL || maximum_timeout_ms < 0)
    return -1;

  adjustment = lws_service_adjust_timeout(context->lws, maximum_timeout_ms, 0);
  if (adjustment < 0) return -1;
  return adjustment == 0 ? 0 : maximum_timeout_ms;
}

int chezpp_lws_http_context_wakeup(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);

  unsigned char byte = 1;
  ssize_t written;
  if (context == NULL || context->lws == NULL) return 0;
  written = write(context->wakeup_pipe[1], &byte, 1);
  if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK) return 0;

  lws_cancel_service(context->lws);
  return 1;
}

ptr chezpp_lws_http_context_pool_metrics(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  ptr metrics;
  if (context == NULL) return Sfalse;
  pthread_mutex_lock(&context->lock);
  metrics = Smake_vector(17, Sfalse);
  Svector_set(metrics, 0, Sunsigned64(context->event_capacity));
  Svector_set(metrics, 1, Sunsigned64(context->event_in_use));
  Svector_set(metrics, 2, Sunsigned64(context->event_high_water));
  Svector_set(metrics, 3, Sunsigned64(context->event_misses));
  Svector_set(metrics, 4, Sunsigned64(context->event_exhaustions));
  Svector_set(metrics, 5, Sunsigned64(context->payload_capacity));
  Svector_set(metrics, 6, Sunsigned64(context->queued_body_bytes));
  Svector_set(metrics, 7, Sunsigned64(context->poll_in_use));
  Svector_set(metrics, 8, Sunsigned64(context->live_handle_count));
  Svector_set(metrics, 9, Sunsigned64(context->poll_high_water));
  Svector_set(metrics, 10, Sunsigned64(context->connection_high_water));
  Svector_set(metrics, 11, Sunsigned64(context->stream_high_water));
  Svector_set(metrics, 12, Sunsigned64(context->body_byte_limit));
  Svector_set(metrics, 13, Sunsigned64(context->signal_capacity));
  Svector_set(metrics, 14, Sunsigned64(context->signal_in_use));
  Svector_set(metrics, 15, Sunsigned64(context->signal_high_water));
  Svector_set(metrics, 16, Sunsigned64(context->signal_misses));
  pthread_mutex_unlock(&context->lock);
  return metrics;
}

uintptr_t chezpp_lws_http_signal_open(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_signal *signal;
  if (context == NULL) return 0;
  pthread_mutex_lock(&context->lock);
  signal = context->signal_free;
  if (signal == NULL) {
    context->signal_misses++;
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  context->signal_free = signal->next_free;
  signal->next_free = NULL;
  signal->active = 1;
  context->signal_in_use++;
  if (context->signal_in_use > context->signal_high_water)
    context->signal_high_water = context->signal_in_use;
  pthread_mutex_unlock(&context->lock);
  return (uintptr_t)signal;
}

int chezpp_lws_http_signal_fd(uintptr_t signal_handle) {
  lws_http_signal *signal = (lws_http_signal *)signal_handle;
  return signal == NULL || !signal->active ? -1 : signal->pipe[0];
}

int chezpp_lws_http_signal_notify(uintptr_t signal_handle) {
  lws_http_signal *signal = (lws_http_signal *)signal_handle;
  unsigned char byte = 1;
  ssize_t written;
  if (signal == NULL || !signal->active) return 0;
  written = write(signal->pipe[1], &byte, 1);
  return written == 1 || (written < 0 && (errno == EAGAIN || errno == EWOULDBLOCK));
}

void chezpp_lws_http_signal_drain(uintptr_t signal_handle) {
  lws_http_signal *signal = (lws_http_signal *)signal_handle;
  unsigned char bytes[64];
  if (signal == NULL || !signal->active) return;
  while (read(signal->pipe[0], bytes, sizeof(bytes)) > 0) {
  }
}

void chezpp_lws_http_signal_close(uintptr_t signal_handle) {
  lws_http_signal *signal = (lws_http_signal *)signal_handle;
  lws_http_context *context;
  if (signal == NULL || !signal->active || signal->context == NULL) return;
  context = signal->context;
  chezpp_lws_http_signal_drain(signal_handle);
  pthread_mutex_lock(&context->lock);
  if (signal->active) {
    signal->active = 0;
    signal->next_free = context->signal_free;
    context->signal_free = signal;
    context->signal_in_use--;
  }
  pthread_mutex_unlock(&context->lock);
}

static int copy_string(char *destination, size_t capacity, const char *source) {
  size_t length;
  if (source == NULL) return 0;
  length = strlen(source);
  if (length >= capacity) return 0;
  memcpy(destination, source, length + 1);
  return 1;
}

int chezpp_lws_http_client_start(uintptr_t context_handle,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, const char *address,
                                 int port, int tls, const char *method,
                                 const char *host, const char *path,
                                 ptr headers, ptr initial_body, int has_body,
                                 const char *alpn) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;

  struct lws_client_connect_info information;
  size_t headers_length;
  size_t initial_body_length;
  if (context == NULL || context->lws == NULL || port <= 0 || port > 65535 ||
      !Sbytevectorp(headers) || !Sbytevectorp(initial_body))
    return 0;
#if !(defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2))
  if (alpn != NULL && strcmp(alpn, "h2") == 0) return 0;
#endif
#if !defined(LWS_WITH_TLS)
  if (tls) return 0;
#endif
  headers_length = (size_t)Sbytevector_length(headers);
  initial_body_length = (size_t)Sbytevector_length(initial_body);
  if (headers_length > context->payload_capacity ||
      initial_body_length > context->payload_capacity)
    return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_acquire_locked(context, connection_id, stream_id, generation);
  if (stream == NULL || !copy_string(stream->address, sizeof(stream->address), address) ||
      !copy_string(stream->method, sizeof(stream->method), method) ||
      !copy_string(stream->host, sizeof(stream->host), host) ||
      !copy_string(stream->path, sizeof(stream->path), path)) {
    if (stream != NULL) stream_release_locked(context, stream);
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  if (headers_length != 0)
    memcpy(stream->headers, Sbytevector_data(headers), headers_length);
  if (initial_body_length != 0)
    memcpy(stream->outbound + LWS_PRE, Sbytevector_data(initial_body),
           initial_body_length);
  stream->headers_length = headers_length;
  stream->outbound_length = initial_body_length;
  stream->submitted_body_length = initial_body_length;
  stream->outbound_final =
      initial_body_is_complete(stream, initial_body_length);
  stream->has_request_body = has_body != 0;
  stream->h2 = alpn != NULL && strcmp(alpn, "h2") == 0;
  pthread_mutex_unlock(&context->lock);
  memset(&information, 0, sizeof(information));
  information.context = context->lws;
  information.address = stream->address;
  information.port = port;
  information.ssl_connection = LCCSCF_HTTP_NO_FOLLOW_REDIRECT;
#if (defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2))
  if (alpn != NULL && strcmp(alpn, "h2") == 0) {
    information.ssl_connection |= LCCSCF_PIPELINE | LCCSCF_H2_QUIRK_OVERFLOWS_TXCR |
                                  LCCSCF_H2_QUIRK_NGHTTP2_END_STREAM;
    if (!tls) information.ssl_connection |= LCCSCF_H2_PRIOR_KNOWLEDGE;
  }
#endif
#if defined(LWS_WITH_TLS)
  if (tls) {
    information.ssl_connection |= LCCSCF_USE_SSL;
    if (!context->tls_verify_peer)
      information.ssl_connection |=
          LCCSCF_ALLOW_SELFSIGNED |
          LCCSCF_SKIP_SERVER_CERT_HOSTNAME_CHECK |
          LCCSCF_ALLOW_EXPIRED | LCCSCF_ALLOW_INSECURE;
  }
#endif
  information.path = stream->path;
  information.host = stream->host;
  information.origin = stream->host;
  information.method = stream->method;
  information.protocol = context->protocols[0].name;
  information.local_protocol_name = context->protocols[0].name;
  information.alpn = alpn != NULL && alpn[0] != '\0' ? alpn : "http/1.1";
  information.opaque_user_data = stream;
  information.pwsi = &stream->wsi;

  if (lws_client_connect_via_info(&information) == NULL) {
    pthread_mutex_lock(&context->lock);
    (void)queue_event_locked(context, LWS_HTTP_EVENT_FAILED, connection_id,
                             stream_id, generation, ECONNREFUSED, NULL, 0);
    stream->terminal = 1;
    stream_release_locked(context, stream);
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  pthread_mutex_lock(&context->lock);
  stream->connection->wsi = stream->wsi;
  pthread_mutex_unlock(&context->lock);
  if (stream->h2) {
    (void)lws_callback_on_writable(stream->wsi);
  }
  return 1;
}

int chezpp_lws_http_client_acquire(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int accepted = 0;
  if (context == NULL || context->closing) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_acquire_locked(context, connection_id, stream_id, generation);
  if (stream != NULL && !stream->server_stream) accepted = 1;
  pthread_mutex_unlock(&context->lock);
  return accepted;
}

int chezpp_lws_http_client_release(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int released = 0;
  if (context == NULL) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream != NULL && stream->generation == generation && stream->terminal &&
      stream->wsi == NULL && stream->pending_body_bytes == 0) {
    stream_release_locked(context, stream);
    released = 1;
  }
  pthread_mutex_unlock(&context->lock);
  return released;
}

int chezpp_lws_http_client_body_submit(uintptr_t context_handle,
                                       uint64_t connection_id,
                                       uint64_t stream_id, uint64_t generation,
                                       ptr payload, int final_chunk) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;

  size_t length;
  if (context == NULL) return 0;
  length = (size_t)Sbytevector_length(payload);
  if (length > context->payload_capacity) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation || stream->terminal ||
      stream->outbound_length != 0) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  if (length != 0)
    memcpy(stream->outbound + LWS_PRE, Sbytevector_data(payload), length);
  stream->outbound_length = length;
  stream->submitted_body_length += length;
  stream->outbound_final = final_chunk != 0 ||
                          initial_body_is_complete(stream, stream->submitted_body_length);
  pthread_mutex_unlock(&context->lock);
  {
    if (stream->wsi != NULL)
      lws_client_http_body_pending(stream->wsi, final_chunk && length == 0 ? 0 : 1);
  }
  if (final_chunk && length == 0) return 1;

  {
    int result = stream->wsi == NULL
                     ? -1
                     : lws_callback_on_writable(stream->wsi);
    return result >= 0;
  }
}

int chezpp_lws_http_client_body_drain(uintptr_t context_handle,
                                      uint64_t connection_id,
                                      uint64_t stream_id,
                                      uint64_t generation) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;

  char *buffer;
  int available;
  if (context == NULL) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation || stream->terminal ||
      stream->pending_body_bytes != 0 || stream->wsi == NULL) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  pthread_mutex_unlock(&context->lock);

  buffer = (char *)context->drain_buffer + LWS_PRE;
  available = (int)context->payload_capacity;
  return lws_http_client_read(stream->wsi, &buffer, &available) == 0;
}

ptr chezpp_lws_http_server_request_dequeue(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_event *event;
  lws_http_event *previous = NULL;
  ptr result;
  if (context == NULL) return Sfalse;
  pthread_mutex_lock(&context->lock);
  for (event = context->event_head; event != NULL; event = event->next) {
    if (event->tag == LWS_HTTP_EVENT_HEADERS) break;
    previous = event;
  }
  if (event == NULL) {
    pthread_mutex_unlock(&context->lock);
    return Sfalse;
  }
  if (previous == NULL)
    context->event_head = event->next;
  else
    previous->next = event->next;
  if (context->event_tail == event) context->event_tail = previous;
  result = event_to_scheme_locked(context, event);
  pthread_mutex_unlock(&context->lock);
  return result;
}

int chezpp_lws_http_server_response_submit(
    uintptr_t context_handle, uint64_t connection_id, uint64_t stream_id,
    uint64_t generation, int status, ptr headers, ptr payload, int final_chunk) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int result;
  if (context == NULL || status < 100 || status > 999 ||
      (size_t)Sbytevector_length(headers) > context->payload_capacity) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation ||
      !stream->server_stream ||
      (stream->response_status != 0 && stream->response_status != status)) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  stream->response_status = status;
  if (!stream->response_headers_sent) {
    stream->headers_length = (size_t)Sbytevector_length(headers);
    memcpy(stream->headers, Sbytevector_data(headers), stream->headers_length);
  }
  pthread_mutex_unlock(&context->lock);
  result = chezpp_lws_http_client_body_submit(
      context_handle, connection_id, stream_id, generation, payload,
      final_chunk);
  if (result && final_chunk && Sbytevector_length(payload) == 0) {
    if (stream->wsi == NULL || lws_callback_on_writable(stream->wsi) < 0)
      return 0;
  }
  return result;
}

int chezpp_lws_http_stream_cancel(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, int status) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;

  struct lws *wsi;
  int result;
  if (context == NULL) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation || stream->terminal) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  result = queue_event_locked(context, LWS_HTTP_EVENT_RESET, connection_id,
                              stream_id, generation, status, NULL, 0);
  stream->terminal = 1;
  wsi = stream->wsi;
  pthread_mutex_unlock(&context->lock);
  /* LWS owns the H2 stream WSI and emits RST_STREAM while closing it. */

  if (wsi != NULL)
    lws_set_timeout(wsi, PENDING_TIMEOUT_AWAITING_SERVER_RESPONSE,
                   LWS_TO_KILL_ASYNC);
  (void)chezpp_lws_http_context_wakeup(context_handle);
  return result;
}

int chezpp_lws_http_body_consumed(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, size_t byte_count) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int resume;
  struct lws *resume_wsi;
  if (context == NULL) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation ||
      byte_count > stream->pending_body_bytes) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  stream->pending_body_bytes -= byte_count;
  if (byte_count <= context->queued_body_bytes)
    context->queued_body_bytes -= byte_count;
  else
    context->queued_body_bytes = 0;
  resume = stream->pending_body_bytes == 0;
  resume_wsi = resume && !(stream->server_stream && stream->server_request_complete)
                   ? stream->wsi : NULL;
  if (resume && stream->terminal_pending && !flush_terminal_locked(context, stream)) {
    stream->terminal_pending = 0;
    stream->terminal = 1;
    stream->failure_pending = 1;
    stream->failure_status = ENOBUFS;
  }
  pthread_mutex_unlock(&context->lock);
  if (resume_wsi != NULL) {
    lws_rx_flow_control(resume_wsi, 1);
  }
  if (resume)
    (void)chezpp_lws_http_context_wakeup(context_handle);
  return 1;
}

int chezpp_lws_http_inject_event(uintptr_t context_handle, int tag,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, int status, ptr payload,
                                 int protocol, int reusable,
                                 uint32_t peer_h2_capacity,
                                 int peer_h2_capacity_known,
                                 int terminal_scope) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  lws_http_event_tag event_tag = (lws_http_event_tag)tag;
  size_t length;
  int result;
  int terminal;
  if (context == NULL || event_tag < LWS_HTTP_EVENT_CONNECTED ||
      event_tag > LWS_HTTP_EVENT_GOAWAY)
    return 0;
  length = (size_t)Sbytevector_length(payload);
  pthread_mutex_lock(&context->lock);
  stream = stream_acquire_locked(context, connection_id, stream_id, generation);
  if (stream == NULL ||
      (event_tag == LWS_HTTP_EVENT_READABLE &&
       stream->pending_body_bytes != 0)) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  stream->observed_protocol = (lws_http_protocol)protocol;
  stream->reusable = reusable != 0;
  stream->peer_h2_capacity = peer_h2_capacity;
  stream->peer_h2_capacity_known = peer_h2_capacity_known != 0;
  terminal = event_tag == LWS_HTTP_EVENT_COMPLETE ||
             event_tag == LWS_HTTP_EVENT_CLOSED ||
             event_tag == LWS_HTTP_EVENT_FAILED ||
             event_tag == LWS_HTTP_EVENT_RESET ||
             event_tag == LWS_HTTP_EVENT_GOAWAY;
  result = terminal
               ? queue_terminal_locked(
                     context, stream, event_tag, status,
                     Sbytevector_data(payload), length,
                     stream->observed_protocol, stream->reusable,
                     stream->peer_h2_capacity, stream->peer_h2_capacity_known,
                     (lws_http_terminal_scope)terminal_scope)
               : queue_event_metadata_locked(
                     context, event_tag, connection_id, stream_id, generation,
                     status, Sbytevector_data(payload), length,
                     stream->observed_protocol, stream->reusable,
                     stream->peer_h2_capacity, stream->peer_h2_capacity_known,
                     LWS_HTTP_TERMINAL_SCOPE_NONE);
  if (result && event_tag == LWS_HTTP_EVENT_READABLE)
    stream->pending_body_bytes += length;
  if (result && terminal && !stream->terminal_pending &&
      stream->pending_body_bytes == 0)
    stream_release_locked(context, stream);
  pthread_mutex_unlock(&context->lock);
  return result;
}

int chezpp_lws_http_inject_poll(uintptr_t context_handle, int operation, int fd,
                                int events) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_event_tag tag = operation == 1
                               ? LWS_HTTP_EVENT_POLL_ADD
                               : operation == 2 ? LWS_HTTP_EVENT_POLL_CHANGE
                                                : LWS_HTTP_EVENT_POLL_DELETE;
  int result;
  if (context == NULL || operation < 1 || operation > 3 || fd < 0) return 0;
  pthread_mutex_lock(&context->lock);
  result = poll_update_locked(context, tag, fd, events, 1);
  pthread_mutex_unlock(&context->lock);
  return result;
}
