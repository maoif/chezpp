#include "lws_http.h"

#include "lws_loader.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct lws_context *(*lws_create_context_fn)(
    const struct lws_context_creation_info *);
typedef void (*lws_context_destroy_fn)(struct lws_context *);
typedef struct lws *(*lws_client_connect_via_info_fn)(
    const struct lws_client_connect_info *);
typedef int (*lws_service_fd_fn)(struct lws_context *, struct lws_pollfd *);
typedef int (*lws_service_tsi_fn)(struct lws_context *, int, int);
typedef int (*lws_service_adjust_timeout_fn)(struct lws_context *, int, int);
typedef void (*lws_cancel_service_fn)(struct lws_context *);
typedef int (*lws_callback_on_writable_fn)(struct lws *);
typedef int (*lws_http_client_read_fn)(struct lws *, char **, int *);
typedef void (*lws_client_http_body_pending_fn)(struct lws *, int);
typedef struct lws_context *(*lws_get_context_fn)(const struct lws *);
typedef void *(*lws_context_user_fn)(struct lws_context *);
typedef void *(*lws_get_opaque_user_data_fn)(const struct lws *);
typedef int (*lws_http_client_http_response_fn)(struct lws *);
typedef int (*lws_hdr_copy_fn)(struct lws *, char *, int,
                               enum lws_token_indexes);
typedef int (*lws_write_fn)(struct lws *, unsigned char *, size_t,
                            enum lws_write_protocol);
typedef void (*lws_set_log_level_fn)(int, lws_log_emit_t);
typedef int (*lws_add_http_header_status_fn)(struct lws *, unsigned int,
                                             unsigned char **, unsigned char *);
typedef int (*lws_finalize_write_http_header_fn)(struct lws *, unsigned char *,
                                                  unsigned char **,
                                                  unsigned char *);
typedef int (*lws_http_transaction_completed_fn)(struct lws *);

static lws_get_context_fn dynamic_get_context;
static lws_context_user_fn dynamic_context_user;
static lws_get_opaque_user_data_fn dynamic_get_opaque_user_data;
static uint64_t next_context_identity;
static uint64_t next_server_identity;

static void *lws_function(const char *name) {
  return chezpp_lws_symbol(name);
}

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

static int queue_event_locked(lws_http_context *context,
                              lws_http_event_tag tag, uint64_t connection_id,
                              uint64_t stream_id, uint64_t generation,
                              int status, const void *payload,
                              size_t payload_length) {
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
  stream->pending_body_bytes = 0;
  stream->outbound_final = 0;
  stream->active = 0;
  stream->terminal = 0;
  stream->failure_pending = 0;
  stream->failure_status = 0;
  stream->server_stream = 0;
  stream->response_status = 0;
  stream->response_headers_sent = 0;
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
  if (wsi == NULL || dynamic_get_context == NULL ||
      dynamic_context_user == NULL)
    return NULL;
  lws_context = dynamic_get_context(wsi);
  return lws_context == NULL
             ? NULL
             : (lws_http_context *)dynamic_context_user(lws_context);
}

static lws_http_stream *callback_stream(struct lws *wsi, void *user) {
  lws_http_stream *stream = NULL;
  if (wsi != NULL && dynamic_get_opaque_user_data != NULL)
    stream = (lws_http_stream *)dynamic_get_opaque_user_data(wsi);
  if (stream == NULL && user != NULL) stream = *(lws_http_stream **)user;
  return stream;
}

static int callback_queue_stream(lws_http_context *context,
                                 lws_http_stream *stream,
                                 lws_http_event_tag tag, int status,
                                 const void *payload, size_t length) {
  int queued;
  if (context == NULL || stream == NULL || !stream->active || stream->terminal)
    return 0;
  pthread_mutex_lock(&context->lock);
  queued = queue_event_locked(context, tag, stream->connection->identity,
                              stream->identity, stream->generation, status,
                              payload, length);
  if (!queued && tag != LWS_HTTP_EVENT_FAILED) {
    stream->terminal = 1;
    stream->failure_pending = 1;
    stream->failure_status = ENOBUFS;
  }
  pthread_mutex_unlock(&context->lock);
  return queued;
}

static size_t copy_common_headers(lws_http_context *context, struct lws *wsi) {
  static const struct {
    enum lws_token_indexes token;
    const char *name;
  } headers[] = {{WSI_TOKEN_CONNECTION, "connection"},
                 {WSI_TOKEN_HTTP_CONTENT_LENGTH, "content-length"},
                 {WSI_TOKEN_HTTP_CONTENT_TYPE, "content-type"},
                 {WSI_TOKEN_HTTP_DATE, "date"},
                 {WSI_TOKEN_HTTP_CONTENT_ENCODING, "content-encoding"},
                 {WSI_TOKEN_HTTP_LOCATION, "location"},
                 {WSI_TOKEN_HTTP_SERVER, "server"},
                 {WSI_TOKEN_HTTP_SET_COOKIE, "set-cookie"},
                 {WSI_TOKEN_HTTP_TRANSFER_ENCODING, "transfer-encoding"}};
  lws_hdr_copy_fn copy_fn =
      (lws_hdr_copy_fn)lws_function("lws_hdr_copy");
  size_t index;
  size_t used = 0;
  if (copy_fn == NULL) return 0;
  for (index = 0; index < sizeof(headers) / sizeof(headers[0]); index++) {
    size_t name_length = strlen(headers[index].name);
    size_t remaining;
    int copied;
    if (used + name_length + 2 > context->payload_capacity) break;
    remaining = context->payload_capacity - used - name_length - 1;
    copied = copy_fn(wsi,
                     (char *)context->drain_buffer + used + name_length + 1,
                     (int)remaining, headers[index].token);
    if (copied <= 0) continue;
    memcpy(context->drain_buffer + used, headers[index].name, name_length);
    context->drain_buffer[used + name_length] = 0;
    used += name_length + 1 + (size_t)copied + 1;
  }
  return used;
}

static int lws_http_callback(struct lws *wsi,
                             enum lws_callback_reasons reason, void *user,
                             void *input, size_t length) {
  lws_http_context *context = callback_context(wsi);
  lws_http_stream *stream = callback_stream(wsi, user);
  switch (reason) {
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
      return 0;
    case LWS_CALLBACK_ESTABLISHED_CLIENT_HTTP: {
      int status = 0;
      size_t headers_length;
      lws_http_client_http_response_fn response_fn =
          (lws_http_client_http_response_fn)lws_function(
              "lws_http_client_http_response");
      if (response_fn != NULL) status = response_fn(wsi);
      headers_length = copy_common_headers(context, wsi);
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_CONNECTED,
                                  status, NULL, 0);
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_HEADERS,
                                  status, context->drain_buffer,
                                  headers_length);
      return 0;
    }
    case LWS_CALLBACK_RECEIVE_CLIENT_HTTP: {
      lws_http_client_read_fn read_fn;
      char *buffer;
      int available;
      if (context == NULL || stream == NULL || stream->pending_body_bytes)
        return 0;
      read_fn = (lws_http_client_read_fn)lws_function("lws_http_client_read");
      if (read_fn == NULL) return -1;
      buffer = (char *)context->drain_buffer + LWS_PRE;
      available = (int)context->payload_capacity;
      return read_fn(wsi, &buffer, &available);
    }
    case LWS_CALLBACK_RECEIVE_CLIENT_HTTP_READ:
      if (context == NULL || stream == NULL) return 0;
      pthread_mutex_lock(&context->lock);
      if (stream->pending_body_bytes != 0 ||
          !queue_event_locked(context, LWS_HTTP_EVENT_READABLE,
                              stream->connection->identity, stream->identity,
                              stream->generation, 0, input, length)) {
        pthread_mutex_unlock(&context->lock);
        return 0;
      }
      stream->pending_body_bytes += length;
      pthread_mutex_unlock(&context->lock);
      return 0;
    case LWS_CALLBACK_CLIENT_HTTP_WRITEABLE: {
      lws_write_fn write_fn;
      lws_client_http_body_pending_fn pending_fn;
      size_t outbound_length;
      int final_chunk;
      int written = 0;
      if (context == NULL || stream == NULL) return 0;
      pthread_mutex_lock(&context->lock);
      outbound_length = stream->outbound_length;
      final_chunk = stream->outbound_final;
      pthread_mutex_unlock(&context->lock);
      if (outbound_length != 0) {
        write_fn = (lws_write_fn)lws_function("lws_write");
        if (write_fn == NULL) return -1;
        written = write_fn(wsi, stream->outbound + LWS_PRE, outbound_length,
                           LWS_WRITE_HTTP);
        if (written < 0) return -1;
        pthread_mutex_lock(&context->lock);
        memset(stream->outbound + LWS_PRE, 0, stream->outbound_length);
        stream->outbound_length = 0;
        pthread_mutex_unlock(&context->lock);
      }
      if (final_chunk) {
        pending_fn = (lws_client_http_body_pending_fn)lws_function(
            "lws_client_http_body_pending");
        if (pending_fn != NULL) pending_fn(wsi, 0);
      }
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE,
                                  written, NULL, 0);
      return 0;
    }
    case LWS_CALLBACK_COMPLETED_CLIENT_HTTP:
    case LWS_CALLBACK_HTTP_BODY_COMPLETION:
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_COMPLETE, 0,
                                  NULL, 0);
      return 0;
    case LWS_CALLBACK_CLOSED_CLIENT_HTTP:
    case LWS_CALLBACK_CLOSED_HTTP:
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_CLOSED, 0,
                                  NULL, 0);
      return 0;
    case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
      if (input != NULL && length == 0)
        length = strnlen((const char *)input,
                         context == NULL ? 0 : context->payload_capacity);
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_FAILED,
                                  ECONNABORTED, input, length);
      return 0;
    case LWS_CALLBACK_HTTP:
      if (context == NULL) return 0;
      if (stream == NULL) {
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
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_HEADERS, 0,
                                  input, length);
      return 0;
    case LWS_CALLBACK_HTTP_BODY:
      if (context == NULL || stream == NULL) return 0;
      pthread_mutex_lock(&context->lock);
      if (stream->pending_body_bytes == 0 &&
          queue_event_locked(context, LWS_HTTP_EVENT_READABLE,
                             stream->connection->identity, stream->identity,
                             stream->generation, 0, input, length))
        stream->pending_body_bytes += length;
      pthread_mutex_unlock(&context->lock);
      return 0;
    case LWS_CALLBACK_HTTP_WRITEABLE:
      if (context != NULL && stream != NULL && stream->outbound_length <=
                                                   context->payload_capacity) {
        lws_write_fn write_fn = (lws_write_fn)lws_function("lws_write");
        lws_add_http_header_status_fn status_fn =
            (lws_add_http_header_status_fn)lws_function(
                "lws_add_http_header_status");
        lws_finalize_write_http_header_fn finish_headers_fn =
            (lws_finalize_write_http_header_fn)lws_function(
                "lws_finalize_write_http_header");
        lws_http_transaction_completed_fn completed_fn =
            (lws_http_transaction_completed_fn)lws_function(
                "lws_http_transaction_completed");
        int written = 0;
        if (!stream->response_headers_sent) {
          unsigned char *start = context->drain_buffer + LWS_PRE;
          unsigned char *cursor = start;
          unsigned char *end = context->drain_buffer + LWS_PRE +
                               context->payload_capacity;
          if (status_fn == NULL || finish_headers_fn == NULL ||
              status_fn(wsi, (unsigned int)stream->response_status, &cursor,
                        end) != 0 ||
              finish_headers_fn(wsi, start, &cursor, end) != 0)
            return -1;
          stream->response_headers_sent = 1;
        }
        if (stream->outbound_length != 0) {
          if (write_fn == NULL) return -1;
          written = write_fn(wsi, stream->outbound + LWS_PRE,
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
          if (completed_fn != NULL && completed_fn(wsi) != 0) return -1;
        }
        (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE,
                                    written, NULL, 0);
        return 0;
      }
      (void)callback_queue_stream(context, stream, LWS_HTTP_EVENT_WRITABLE, 0,
                                  NULL, 0);
      return 0;
    case LWS_CALLBACK_CLIENT_HTTP_DROP_PROTOCOL:
    case LWS_CALLBACK_HTTP_DROP_PROTOCOL:
      if (context != NULL && stream != NULL) {
        pthread_mutex_lock(&context->lock);
        stream_release_locked(context, stream);
        if (user != NULL) *(lws_http_stream **)user = NULL;
        pthread_mutex_unlock(&context->lock);
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
  context->events = calloc(context->event_capacity, sizeof(*context->events));
  context->event_payloads =
      calloc(context->event_capacity, context->payload_capacity);
  context->poll_entries =
      calloc(context->poll_capacity, sizeof(*context->poll_entries));
  context->connections =
      calloc(context->connection_capacity, sizeof(*context->connections));
  context->streams = calloc(context->stream_capacity, sizeof(*context->streams));
  context->stream_payloads = calloc(context->stream_capacity, stream_slot_size);
  context->drain_buffer = calloc(1, stream_slot_size);
  if (context->events == NULL || context->event_payloads == NULL ||
      context->poll_entries == NULL || context->connections == NULL ||
      context->streams == NULL || context->stream_payloads == NULL ||
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
  context->body_byte_limit = context->event_capacity * context->payload_capacity;
  return 1;
}

static void free_context_storage(lws_http_context *context) {
  free(context->drain_buffer);
  free(context->stream_payloads);
  free(context->streams);
  free(context->connections);
  free(context->poll_entries);
  free(context->event_payloads);
  free(context->events);
  free(context);
}

uintptr_t chezpp_lws_http_context_open(size_t event_capacity,
                                       size_t payload_capacity) {
  lws_http_context *context;
  struct lws_context_creation_info information;
  lws_create_context_fn create_fn;
  lws_set_log_level_fn log_fn;
  if (event_capacity == 0 || payload_capacity == 0 ||
      event_capacity > UINT_MAX || payload_capacity > INT_MAX - LWS_PRE ||
      event_capacity > SIZE_MAX / payload_capacity ||
      !chezpp_lws_ensure_loaded())
    return 0;
  create_fn = (lws_create_context_fn)lws_function("lws_create_context");
  dynamic_get_context =
      (lws_get_context_fn)lws_function("lws_get_context");
  dynamic_context_user =
      (lws_context_user_fn)lws_function("lws_context_user");
  dynamic_get_opaque_user_data = (lws_get_opaque_user_data_fn)lws_function(
      "lws_get_opaque_user_data");
  if (create_fn == NULL || dynamic_get_context == NULL ||
      dynamic_context_user == NULL || dynamic_get_opaque_user_data == NULL)
    return 0;
  context = calloc(1, sizeof(*context));
  if (context == NULL) return 0;
  context->identity = fresh_identity(&next_context_identity);
  context->event_capacity = event_capacity;
  context->payload_capacity = payload_capacity;
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
  information.port = CONTEXT_PORT_NO_LISTEN;
  information.protocols = context->protocols;
  information.user = context;
  information.fd_limit_per_thread = (unsigned int)context->poll_capacity;
  log_fn = (lws_set_log_level_fn)lws_function("lws_set_log_level");
  if (log_fn != NULL) log_fn(0, NULL);
  context->initializing = 1;
  context->lws = create_fn(&information);
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

void chezpp_lws_http_context_close(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_context_destroy_fn destroy_fn;
  if (context == NULL) return;
  context->closing = 1;
  destroy_fn = (lws_context_destroy_fn)lws_function("lws_context_destroy");
  if (context->lws != NULL && destroy_fn != NULL) destroy_fn(context->lws);
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
  lws_service_fd_fn service_fn;
  lws_service_tsi_fn service_tsi_fn;
  struct lws_pollfd poll_descriptor;
  if (context == NULL || context->lws == NULL) return -1;
  if (fd == context->wakeup_pipe[0]) {
    unsigned char bytes[64];
    while (read(fd, bytes, sizeof(bytes)) > 0) {
    }
    return 0;
  }
  if (fd < 0) {
    service_tsi_fn = (lws_service_tsi_fn)lws_function("lws_service_tsi");
    return service_tsi_fn == NULL ? -1 : service_tsi_fn(context->lws, -1, 0);
  }
  service_fn = (lws_service_fd_fn)lws_function("lws_service_fd");
  if (service_fn == NULL) return -1;
  memset(&poll_descriptor, 0, sizeof(poll_descriptor));
  poll_descriptor.fd = fd;
  poll_descriptor.events = 0;
  poll_descriptor.revents = (short)revents;
  return service_fn(context->lws, &poll_descriptor);
}

static ptr event_to_scheme_locked(lws_http_context *context,
                                  lws_http_event *event) {
  ptr result = Smake_vector(7, Sfalse);
  ptr payload = Smake_bytevector((iptr)event->payload_length, 0);
  if (event->payload_length != 0)
    memcpy(Sbytevector_data(payload), event->payload, event->payload_length);
  Svector_set(result, 0, Sstring_to_symbol(event_tag_name(event->tag)));
  Svector_set(result, 1, Sunsigned64(event->context_id));
  Svector_set(result, 2, Sunsigned64(event->connection_id));
  Svector_set(result, 3, Sunsigned64(event->stream_id));
  Svector_set(result, 4, Sunsigned64(event->generation));
  Svector_set(result, 5, Sinteger(event->status));
  Svector_set(result, 6, payload);
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
  lws_service_adjust_timeout_fn timeout_fn;
  int adjustment;
  if (context == NULL || context->lws == NULL || maximum_timeout_ms < 0)
    return -1;
  timeout_fn = (lws_service_adjust_timeout_fn)lws_function(
      "lws_service_adjust_timeout");
  if (timeout_fn == NULL) return -1;
  adjustment = timeout_fn(context->lws, maximum_timeout_ms, 0);
  if (adjustment < 0) return -1;
  return adjustment == 0 ? 0 : maximum_timeout_ms;
}

int chezpp_lws_http_context_wakeup(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_cancel_service_fn cancel_fn;
  unsigned char byte = 1;
  ssize_t written;
  if (context == NULL || context->lws == NULL) return 0;
  written = write(context->wakeup_pipe[1], &byte, 1);
  if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK) return 0;
  cancel_fn = (lws_cancel_service_fn)lws_function("lws_cancel_service");
  if (cancel_fn != NULL) cancel_fn(context->lws);
  return 1;
}

ptr chezpp_lws_http_context_pool_metrics(uintptr_t context_handle) {
  lws_http_context *context = context_from_handle(context_handle);
  ptr metrics;
  if (context == NULL) return Sfalse;
  pthread_mutex_lock(&context->lock);
  metrics = Smake_vector(13, Sfalse);
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
  pthread_mutex_unlock(&context->lock);
  return metrics;
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
                                 const char *host, const char *path) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  lws_client_connect_via_info_fn connect_fn;
  struct lws_client_connect_info information;
  if (context == NULL || context->lws == NULL || port <= 0 || port > 65535)
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
  pthread_mutex_unlock(&context->lock);
  memset(&information, 0, sizeof(information));
  information.context = context->lws;
  information.address = stream->address;
  information.port = port;
  information.ssl_connection = tls ? LCCSCF_USE_SSL : 0;
  information.path = stream->path;
  information.host = stream->host;
  information.origin = stream->host;
  information.method = stream->method;
  information.protocol = context->protocols[0].name;
  information.local_protocol_name = context->protocols[0].name;
  information.opaque_user_data = stream;
  information.pwsi = &stream->wsi;
  connect_fn = (lws_client_connect_via_info_fn)lws_function(
      "lws_client_connect_via_info");
  if (connect_fn == NULL || connect_fn(&information) == NULL) {
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
  return 1;
}

int chezpp_lws_http_client_body_submit(uintptr_t context_handle,
                                       uint64_t connection_id,
                                       uint64_t stream_id, uint64_t generation,
                                       ptr payload, int final_chunk) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  lws_callback_on_writable_fn writable_fn;
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
  stream->outbound_final = final_chunk != 0;
  pthread_mutex_unlock(&context->lock);
  writable_fn =
      (lws_callback_on_writable_fn)lws_function("lws_callback_on_writable");
  return writable_fn != NULL && stream->wsi != NULL &&
         writable_fn(stream->wsi) == 0;
}

int chezpp_lws_http_client_body_drain(uintptr_t context_handle,
                                      uint64_t connection_id,
                                      uint64_t stream_id,
                                      uint64_t generation) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  lws_http_client_read_fn read_fn;
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
  read_fn = (lws_http_client_read_fn)lws_function("lws_http_client_read");
  if (read_fn == NULL) return 0;
  buffer = (char *)context->drain_buffer + LWS_PRE;
  available = (int)context->payload_capacity;
  return read_fn(stream->wsi, &buffer, &available) == 0;
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
    uint64_t generation, int status, ptr payload, int final_chunk) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int result;
  if (context == NULL || status < 100 || status > 999) return 0;
  pthread_mutex_lock(&context->lock);
  stream = stream_find_locked(context, connection_id, stream_id);
  if (stream == NULL || stream->generation != generation ||
      !stream->server_stream ||
      (stream->response_status != 0 && stream->response_status != status)) {
    pthread_mutex_unlock(&context->lock);
    return 0;
  }
  stream->response_status = status;
  pthread_mutex_unlock(&context->lock);
  result = chezpp_lws_http_client_body_submit(
      context_handle, connection_id, stream_id, generation, payload,
      final_chunk);
  return result;
}

int chezpp_lws_http_stream_cancel(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, int status) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
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
  pthread_mutex_unlock(&context->lock);
  (void)chezpp_lws_http_context_wakeup(context_handle);
  return result;
}

int chezpp_lws_http_body_consumed(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, size_t byte_count) {
  lws_http_context *context = context_from_handle(context_handle);
  lws_http_stream *stream;
  int resume;
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
  if (resume && stream->terminal && stream->wsi == NULL)
    stream_release_locked(context, stream);
  pthread_mutex_unlock(&context->lock);
  if (resume)
    (void)chezpp_lws_http_context_wakeup(context_handle);
  return 1;
}

int chezpp_lws_http_inject_event(uintptr_t context_handle, int tag,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, int status, ptr payload) {
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
  result = queue_event_locked(context, event_tag, connection_id, stream_id,
                              generation, status, Sbytevector_data(payload),
                              length);
  if (result && event_tag == LWS_HTTP_EVENT_READABLE)
    stream->pending_body_bytes += length;
  terminal = event_tag == LWS_HTTP_EVENT_COMPLETE ||
             event_tag == LWS_HTTP_EVENT_CLOSED ||
             event_tag == LWS_HTTP_EVENT_FAILED ||
             event_tag == LWS_HTTP_EVENT_RESET ||
             event_tag == LWS_HTTP_EVENT_GOAWAY;
  if (result && terminal) {
    stream->terminal = 1;
    if (stream->pending_body_bytes == 0) stream_release_locked(context, stream);
  }
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
