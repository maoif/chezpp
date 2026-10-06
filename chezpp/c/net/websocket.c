/*
 * LWS features used by this adapter (libwebsockets.so.21, runtime >= 4.3.0;
 * verified with 4.5.8):
 *
 * - LWS_ROLE_H1 and LWS_ROLE_WS with LWS_WITH_CLIENT and LWS_WITH_SERVER:
 *   HTTP Upgrade, subprotocol negotiation, text / binary messages, continuation
 *   frames, ping / pong, and close status / reason handling.
 * - LWS_WITH_EXTERNAL_POLL: ADD / CHANGE_MODE / DEL_POLL_FD callbacks supply
 *   Scheme readiness snapshots. lws_service_tsi(context, -1, 0) makes one
 *   nonblocking service pass; lws_service_adjust_timeout detects forced work.
 *   LWS owns scheduled timeouts and buffered input; native callbacks never call
 *   Scheme. Context lifetimes and service are serialized by context_list_mutex.
 * - Optional LWS_WITH_TLS with the OpenSSL backend: WSS reuses caller-owned
 *   client SSL_CTX and copies server credentials through the vhost callback.
 *
 * No WebSocket extensions are configured or negotiated. In particular,
 * permessage-deflate and LWS extension callbacks are unsupported; a runtime
 * built with LWS_WITHOUT_EXTENSIONS is sufficient. HTTP/2 and alternative
 * event-loop backends (libuv / libev / libevent) are not used by this adapter.
 * All LWS symbols are dynamically loaded; libchezpp.so does not link to LWS.
 */

#include "../common.h"
#include "../optional_library.h"

#include <libwebsockets.h>
#include <poll.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdatomic.h>

typedef struct chezpp_ws_message chezpp_ws_message;
typedef struct chezpp_ws_send chezpp_ws_send;
typedef struct chezpp_ws_server chezpp_ws_server;
typedef struct chezpp_ws_connection chezpp_ws_connection;
typedef struct chezpp_ws_context_node chezpp_ws_context_node;
typedef struct chezpp_ws_poll_fd chezpp_ws_poll_fd;

struct chezpp_ws_poll_fd {
  chezpp_ws_poll_fd *next;
  struct lws *wsi;
  chezpp_ws_connection *connection;
  int fd;
  int events;
};

typedef struct {
  chezpp_ws_poll_fd *head;
  int wakeup_pipe[2];
  int forced_pending;
} chezpp_ws_poll_state;

struct chezpp_ws_message {
  chezpp_ws_message *next;
  int type;
  size_t len;
  unsigned char *data;
};

struct chezpp_ws_send {
  int type;
  int first;
  int final;
  size_t len;
  unsigned char *buf;
};

struct chezpp_ws_server {
  /* Must stay first: poll callbacks cast the context user to this field. */
  chezpp_ws_poll_state poll_state;
  struct lws_context *context;
  struct lws_protocols *protocols;
  size_t protocol_count;
  char *protocol_name;
  int port;
  int closed;
  uptr tls_context_handle;
  int live_count;
  char *error;
  chezpp_ws_connection *accept_head;
  chezpp_ws_connection *accept_tail;
};

struct chezpp_ws_connection {
  /* Must stay first: poll callbacks cast the context user to this field. */
  chezpp_ws_poll_state poll_state;
  struct lws_context *context;
  struct lws *wsi;
  chezpp_ws_server *server;
  chezpp_ws_connection *next_accept;
  struct lws_protocols protocols[2];
  char *protocol_name;
  int owns_context;
  int established;
  int closed;
  int failed;
  int client_side;
  int handed_out;
  int fragment_open;
  unsigned long pong_count;
  int close_code;
  char *close_reason;
  char *negotiated_protocol;
  int rx_type;
  size_t rx_len;
  size_t rx_cap;
  unsigned char *rx_buf;
  char *error;
  chezpp_ws_message *msg_head;
  chezpp_ws_message *msg_tail;
  chezpp_ws_send *pending_send;
};

struct chezpp_ws_context_node {
  chezpp_ws_context_node *next;
  struct lws_context *context;
};

typedef struct lws_context *(*lws_create_context_fn)(const struct lws_context_creation_info *);
typedef void (*lws_context_destroy_fn)(struct lws_context *);
typedef struct lws *(*lws_client_connect_via_info_fn)(const struct lws_client_connect_info *);
typedef int (*lws_service_tsi_fn)(struct lws_context *, int, int);
typedef int (*lws_service_adjust_timeout_fn)(struct lws_context *, int, int);
typedef void (*lws_set_log_level_fn)(int, lws_log_emit_t);
typedef struct lws_context *(*lws_get_context_fn)(const struct lws *);
typedef void *(*lws_context_user_fn)(struct lws_context *);
typedef void (*lws_set_opaque_user_data_fn)(struct lws *, void *);
typedef void *(*lws_get_opaque_user_data_fn)(const struct lws *);
typedef int (*lws_callback_on_writable_fn)(struct lws *);
typedef int (*lws_write_fn)(struct lws *, unsigned char *, size_t, enum lws_write_protocol);
typedef int (*lws_is_final_fragment_fn)(struct lws *);
typedef size_t (*lws_remaining_packet_payload_fn)(struct lws *);
typedef int (*lws_frame_is_binary_fn)(struct lws *);
typedef void (*lws_close_reason_fn)(struct lws *, enum lws_close_status, unsigned char *, size_t);
typedef void (*lws_set_timeout_fn)(struct lws *, enum pending_timeout, int);
typedef struct lws_vhost *(*lws_get_vhost_by_name_fn)(struct lws_context *, const char *);
typedef int (*lws_get_vhost_listen_port_fn)(struct lws_vhost *);
typedef const char *(*lws_get_library_version_fn)(void);
typedef const struct lws_protocols *(*lws_get_protocol_fn)(struct lws *);
typedef void *(*lws_vhost_user_fn)(struct lws_vhost *);
typedef int (*lws_hdr_copy_fn)(struct lws *, char *, int, enum lws_token_indexes);
typedef int (*lws_hdr_total_length_fn)(struct lws *, enum lws_token_indexes);

extern void *chezpp_net_tls_context_native(uptr handle);
extern int chezpp_net_tls_context_copy_credentials(uptr handle, void *destination);

static const char *const websocket_names[] = {
    "libwebsockets.so.21", NULL};
static chezpp_optional_library websocket_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("websockets", websocket_names);
static pthread_once_t websocket_once = PTHREAD_ONCE_INIT;
static int websocket_available;
static unsigned websocket_capabilities;
static struct lws_context *websocket_lifetime_context = NULL;
static int websocket_lifetime_state;
static pthread_mutex_t websocket_runtime_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t websocket_runtime_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t context_list_mutex = PTHREAD_MUTEX_INITIALIZER;
static lws_create_context_fn p_lws_create_context = NULL;
static lws_context_destroy_fn p_lws_context_destroy = NULL;
static lws_client_connect_via_info_fn p_lws_client_connect_via_info = NULL;
static lws_service_tsi_fn p_lws_service_tsi = NULL;
static lws_service_adjust_timeout_fn p_lws_service_adjust_timeout = NULL;
static lws_set_log_level_fn p_lws_set_log_level = NULL;
static lws_get_context_fn p_lws_get_context = NULL;
static lws_context_user_fn p_lws_context_user = NULL;
static lws_set_opaque_user_data_fn p_lws_set_opaque_user_data = NULL;
static lws_get_opaque_user_data_fn p_lws_get_opaque_user_data = NULL;
static lws_callback_on_writable_fn p_lws_callback_on_writable = NULL;
static lws_write_fn p_lws_write = NULL;
static lws_is_final_fragment_fn p_lws_is_final_fragment = NULL;
static lws_remaining_packet_payload_fn p_lws_remaining_packet_payload = NULL;
static lws_frame_is_binary_fn p_lws_frame_is_binary = NULL;
static lws_close_reason_fn p_lws_close_reason = NULL;
static lws_set_timeout_fn p_lws_set_timeout = NULL;
static lws_get_vhost_by_name_fn p_lws_get_vhost_by_name = NULL;
static lws_get_vhost_listen_port_fn p_lws_get_vhost_listen_port = NULL;
static lws_get_protocol_fn p_lws_get_protocol = NULL;
static lws_vhost_user_fn p_lws_vhost_user = NULL;
static lws_hdr_copy_fn p_lws_hdr_copy = NULL;
static lws_hdr_total_length_fn p_lws_hdr_total_length = NULL;
static chezpp_ws_context_node *context_list = NULL;
static atomic_int ws_trace_flag = -1;

static int ws_trace_enabled(void) {
  int enabled = atomic_load(&ws_trace_flag);
  if (enabled < 0) {
    const char *value = getenv("CHEZPP_WS_TRACE");
    int expected = -1;
    enabled = (value != NULL && *value != 0) ? 1 : 0;
    atomic_compare_exchange_strong(&ws_trace_flag, &expected, enabled);
  }
  return atomic_load(&ws_trace_flag);
}

static void ws_tracef(const char *fmt, ...) {
  va_list ap;

  if (!ws_trace_enabled()) return;
  fprintf(stderr, "[chezpp-ws] ");
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  fflush(stderr);
}

enum {
  CHEZPP_WS_TEXT = 1,
  CHEZPP_WS_BINARY = 2,
  CHEZPP_WS_PING = 3,
  CHEZPP_WS_PONG = 4
};

static int websocket_callback(struct lws *wsi, enum lws_callback_reasons reason,
                              void *user, void *in, size_t len);


#define CHEZPP_WS_DEFAULT_PROTOCOL "chezpp-websocket"
#define CHEZPP_WS_SERVICE_STEP_MS 10
#define CHEZPP_WS_NONBLOCK_SERVICE_MS 1

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) {
  return make_status("error", Sstring(msg == NULL ? "websocket error" : msg));
}

static ptr make_would_block_status(chezpp_ws_poll_state *state, int requested_events,
                                   struct lws *wsi, int unowned_only) {
  chezpp_ws_poll_fd *entry = state == NULL ? NULL : state->head;
  ptr detail;
  ptr events = Snil;

  if (state != NULL && state->forced_pending) {
    detail = Smake_vector(2, Sfalse);
    Svector_set(detail, 0, Sfixnum(state->wakeup_pipe[0]));
    Svector_set(detail, 1, Scons(Sstring_to_symbol("read"), Snil));
    return make_status("would-block", detail);
  }
  while (entry != NULL &&
         ((entry->events & requested_events) == 0 ||
          (wsi != NULL && entry->wsi != wsi) ||
          (unowned_only && entry->connection != NULL)))
    entry = entry->next;
  if (entry == NULL)
    return make_error_status_message("websocket has no service descriptor");
  if ((entry->events & POLLOUT) != 0)
    events = Scons(Sstring_to_symbol("write"), events);
  if ((entry->events & POLLIN) != 0)
    events = Scons(Sstring_to_symbol("read"), events);
  if (Snullp(events)) {
    if ((requested_events & POLLOUT) != 0)
      events = Scons(Sstring_to_symbol("write"), events);
    if ((requested_events & POLLIN) != 0)
      events = Scons(Sstring_to_symbol("read"), events);
  }
  detail = Smake_vector(2, Sfalse);
  Svector_set(detail, 0, Sfixnum((iptr)entry->fd));
  Svector_set(detail, 1, events);
  return make_status("would-block", detail);
}

static void update_poll_fd(chezpp_ws_poll_state *state, struct lws *wsi,
                           int fd, int events) {
  chezpp_ws_poll_fd *entry;
  if (state == NULL) return;
  entry = state->head;
  while (entry != NULL && entry->fd != fd) entry = entry->next;
  if (entry == NULL) {
    entry = (chezpp_ws_poll_fd *)calloc(1, sizeof(chezpp_ws_poll_fd));
    if (entry == NULL) return;
    entry->fd = fd;
    entry->next = state->head;
    state->head = entry;
  }
  entry->wsi = wsi;
  entry->events = events;
}

static void assign_poll_connection(chezpp_ws_poll_state *state, struct lws *wsi,
                                   chezpp_ws_connection *connection) {
  chezpp_ws_poll_fd *entry = state == NULL ? NULL : state->head;
  while (entry != NULL) {
    if (entry->wsi == wsi) {
      entry->connection = connection;
      return;
    }
    entry = entry->next;
  }
}

static void remove_poll_fd(chezpp_ws_poll_state *state, int fd) {
  chezpp_ws_poll_fd **entry;
  if (state == NULL) return;
  entry = &state->head;
  while (*entry != NULL) {
    if ((*entry)->fd == fd) {
      chezpp_ws_poll_fd *removed = *entry;
      *entry = removed->next;
      free(removed);
      return;
    }
    entry = &(*entry)->next;
  }
}

static void clear_poll_fds(chezpp_ws_poll_state *state) {
  if (state == NULL) return;
  while (state->head != NULL) {
    chezpp_ws_poll_fd *removed = state->head;
    state->head = removed->next;
    free(removed);
  }
}

static ptr make_websocket_handle(uptr handle) { return Sunsigned(handle); }

static const char *ws_type_symbol(int type) {
  switch (type) {
  case CHEZPP_WS_TEXT:
    return "text";
  case CHEZPP_WS_BINARY:
    return "binary";
  case CHEZPP_WS_PING:
    return "ping";
  case CHEZPP_WS_PONG:
    return "pong";
  default:
    return "binary";
  }
}

static int current_time_ms(long long *out) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0;
  *out = ((long long)ts.tv_sec * 1000LL) + ((long long)ts.tv_nsec / 1000000LL);
  return 1;
}

static int remaining_timeout_ms(int timeout_ms, long long start_ms) {
  long long now;
  long long elapsed;
  long long remaining;

  if (timeout_ms < 0) return -1;
  if (!current_time_ms(&now)) return -2;
  elapsed = now - start_ms;
  if (elapsed < 0) elapsed = 0;
  remaining = (long long)timeout_ms - elapsed;
  if (remaining <= 0) return 0;
  if (remaining > (long long)INT_MAX) return INT_MAX;
  return (int)remaining;
}

static char *ws_strdup(const char *s) {
  size_t len;
  char *out;
  if (s == NULL) return NULL;
  len = strlen(s);
  out = (char *)malloc(len + 1);
  if (out == NULL) return NULL;
  memcpy(out, s, len + 1);
  return out;
}

static void free_server_protocols(chezpp_ws_server *server) {
  size_t index;
  if (server == NULL || server->protocols == NULL) return;
  for (index = 1; index <= server->protocol_count; index++)
    free((char *)server->protocols[index].name);
  free(server->protocols);
  server->protocols = NULL;
}

static int initialize_server_protocols(chezpp_ws_server *server, const char *offered) {
  const char *cursor = offered;
  size_t count = 1;
  size_t index = 1;

  while (*cursor != '\0') {
    if (*cursor++ == ',') count++;
  }
  server->protocols =
      (struct lws_protocols *)calloc(count + 2, sizeof(struct lws_protocols));
  if (server->protocols == NULL) return 0;
  server->protocol_count = count;
  server->protocols[0].name = "http";
  server->protocols[0].callback = websocket_callback;
  cursor = offered;
  while (index <= count) {
    const char *start;
    size_t length;
    char *name;
    while (*cursor == ' ') cursor++;
    start = cursor;
    while (*cursor != '\0' && *cursor != ',') cursor++;
    length = (size_t)(cursor - start);
    while (length > 0 && start[length - 1] == ' ') length--;
    name = (char *)malloc(length + 1);
    if (name == NULL) {
      free_server_protocols(server);
      return 0;
    }
    memcpy(name, start, length);
    name[length] = '\0';
    server->protocols[index].name = name;
    server->protocols[index].callback = websocket_callback;
    index++;
    if (*cursor == ',') cursor++;
  }
  return 1;
}

static int load_symbol(void **out, const char *name) {
  return chezpp_optional_library_symbol(&websocket_library, name, out);
}

static void initialize_websocket_library(void) {
  lws_get_library_version_fn version_fn = NULL;
  const char *version;
  unsigned major;
  unsigned minor;
  unsigned patch;

  if (!chezpp_optional_library_open(&websocket_library)) return;
  if (!load_symbol((void **)&version_fn, "lws_get_library_version")) return;
  version = version_fn();
  if (version == NULL ||
      sscanf(version, "%u.%u.%u", &major, &minor, &patch) != 3) {
    chezpp_optional_library_fail(
        &websocket_library,
        "websockets: unable to parse runtime version %s",
        version == NULL ? "(null)" : version);
    return;
  }
  chezpp_optional_library_set_version(&websocket_library, version);
  if (major < 4 || (major == 4 && minor < 3)) {
    chezpp_optional_library_fail(
        &websocket_library,
        "websockets: runtime ABI version %u.%u.%u requires >= 4.3.0",
        major, minor, patch);
    return;
  }

  if (!load_symbol((void **)&p_lws_create_context, "lws_create_context") ||
      !load_symbol((void **)&p_lws_context_destroy, "lws_context_destroy") ||
      !load_symbol((void **)&p_lws_client_connect_via_info, "lws_client_connect_via_info") ||
      !load_symbol((void **)&p_lws_service_tsi, "lws_service_tsi") ||
      !load_symbol((void **)&p_lws_service_adjust_timeout, "lws_service_adjust_timeout") ||
      !load_symbol((void **)&p_lws_set_log_level, "lws_set_log_level") ||
      !load_symbol((void **)&p_lws_get_context, "lws_get_context") ||
      !load_symbol((void **)&p_lws_context_user, "lws_context_user") ||
      !load_symbol((void **)&p_lws_set_opaque_user_data, "lws_set_opaque_user_data") ||
      !load_symbol((void **)&p_lws_get_opaque_user_data, "lws_get_opaque_user_data") ||
      !load_symbol((void **)&p_lws_callback_on_writable, "lws_callback_on_writable") ||
      !load_symbol((void **)&p_lws_write, "lws_write") ||
      !load_symbol((void **)&p_lws_is_final_fragment, "lws_is_final_fragment") ||
      !load_symbol((void **)&p_lws_remaining_packet_payload, "lws_remaining_packet_payload") ||
      !load_symbol((void **)&p_lws_frame_is_binary, "lws_frame_is_binary") ||
      !load_symbol((void **)&p_lws_close_reason, "lws_close_reason") ||
      !load_symbol((void **)&p_lws_set_timeout, "lws_set_timeout") ||
      !load_symbol((void **)&p_lws_get_vhost_by_name, "lws_get_vhost_by_name") ||
      !load_symbol((void **)&p_lws_get_vhost_listen_port, "lws_get_vhost_listen_port") ||
      !load_symbol((void **)&p_lws_get_protocol, "lws_get_protocol") ||
      !load_symbol((void **)&p_lws_vhost_user, "lws_vhost_user") ||
      !load_symbol((void **)&p_lws_hdr_copy, "lws_hdr_copy") ||
      !load_symbol((void **)&p_lws_hdr_total_length, "lws_hdr_total_length")) return;

  {
    void *symbol = NULL;
    const char *level = getenv("CHEZPP_WS_LOGLEVEL");
    if (chezpp_optional_library_probe_symbol(
            &websocket_library, "lws_init_vhost_client_ssl",
            &symbol))
      websocket_capabilities |= 1U;
    if (level != NULL && *level != 0)
      p_lws_set_log_level((int)strtol(level, NULL, 0), NULL);
    else
      p_lws_set_log_level(0, NULL);
  }
  websocket_available = 1;
}

static int ensure_websocket_loaded(void) {
  pthread_once(&websocket_once, initialize_websocket_library);
  return websocket_available;
}

const chezpp_optional_library *chezpp_net_websocket_library(void) {
  (void)ensure_websocket_loaded();
  return &websocket_library;
}

unsigned chezpp_net_websocket_capabilities(void) {
  (void)ensure_websocket_loaded();
  return websocket_capabilities;
}

static int initialize_websocket_lifetime_context(void) {
  struct lws_context_creation_info info;

  memset(&info, 0, sizeof(info));
  info.port = CONTEXT_PORT_NO_LISTEN;
  info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
  info.gid = (gid_t)-1;
  info.uid = (uid_t)-1;
  websocket_lifetime_context = p_lws_create_context(&info);
  return websocket_lifetime_context != NULL;
}

static int ensure_websocket_lifetime_context(void) {
  int initialized;

  pthread_mutex_lock(&websocket_runtime_mutex);
  while (websocket_lifetime_state == 1)
    pthread_cond_wait(&websocket_runtime_cond, &websocket_runtime_mutex);
  if (websocket_lifetime_state != 0) {
    initialized = websocket_lifetime_state == 2;
    pthread_mutex_unlock(&websocket_runtime_mutex);
    return initialized;
  }
  websocket_lifetime_state = 1;
  pthread_mutex_unlock(&websocket_runtime_mutex);

  initialized = initialize_websocket_lifetime_context();

  pthread_mutex_lock(&websocket_runtime_mutex);
  websocket_lifetime_state = initialized ? 2 : 3;
  pthread_cond_broadcast(&websocket_runtime_cond);
  pthread_mutex_unlock(&websocket_runtime_mutex);
  return initialized;
}

__attribute__((destructor)) static void release_websocket_runtime(void) {
  pthread_mutex_lock(&websocket_runtime_mutex);
  pthread_mutex_lock(&context_list_mutex);
  if (context_list == NULL && websocket_lifetime_context != NULL &&
      p_lws_context_destroy != NULL) {
    p_lws_context_destroy(websocket_lifetime_context);
    websocket_lifetime_context = NULL;
  }
  pthread_mutex_unlock(&context_list_mutex);
  pthread_mutex_unlock(&websocket_runtime_mutex);
}

static int context_registered_unlocked(struct lws_context *context) {
  chezpp_ws_context_node *node = context_list;
  while (node != NULL) {
    if (node->context == context) return 1;
    node = node->next;
  }
  return 0;
}

static struct lws_context *create_registered_context(
    const struct lws_context_creation_info *info) {
  chezpp_ws_context_node *node = (chezpp_ws_context_node *)calloc(1, sizeof(chezpp_ws_context_node));
  chezpp_ws_poll_state *state = (chezpp_ws_poll_state *)info->user;
  struct lws_context *context;
  int index;

  if (node == NULL) return NULL;
  if (pipe(state->wakeup_pipe) != 0) {
    free(node);
    return NULL;
  }
  for (index = 0; index < 2; index++) {
    int fd = state->wakeup_pipe[index];
    int flags = fcntl(fd, F_GETFL, 0);
    int fd_flags = fcntl(fd, F_GETFD, 0);
    if (flags < 0 || fd_flags < 0 ||
        fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0 ||
        fcntl(fd, F_SETFD, fd_flags | FD_CLOEXEC) < 0) {
      close(state->wakeup_pipe[0]);
      close(state->wakeup_pipe[1]);
      free(node);
      return NULL;
    }
  }
  pthread_mutex_lock(&context_list_mutex);
  context = p_lws_create_context(info);
  if (context == NULL) {
    pthread_mutex_unlock(&context_list_mutex);
    close(state->wakeup_pipe[0]);
    close(state->wakeup_pipe[1]);
    free(node);
    return NULL;
  }
  node->context = context;
  node->next = context_list;
  context_list = node;
  pthread_mutex_unlock(&context_list_mutex);
  return context;
}

static void destroy_registered_context(struct lws_context *context) {
  chezpp_ws_context_node **pp;
  pthread_mutex_lock(&context_list_mutex);
  pp = &context_list;
  while (*pp != NULL) {
    if ((*pp)->context == context) {
      chezpp_ws_context_node *node = *pp;
      chezpp_ws_poll_state *state = (chezpp_ws_poll_state *)p_lws_context_user(context);
      *pp = node->next;
      free(node);
      p_lws_context_destroy(context);
      close(state->wakeup_pipe[0]);
      close(state->wakeup_pipe[1]);
      state->forced_pending = 0;
      pthread_mutex_unlock(&context_list_mutex);
      return;
    }
    pp = &(*pp)->next;
  }
  pthread_mutex_unlock(&context_list_mutex);
}

static void service_context_ready_locked(struct lws_context *context) {
  chezpp_ws_poll_state *state = (chezpp_ws_poll_state *)p_lws_context_user(context);
  unsigned char byte = 1;
  unsigned char bytes[64];
  /* Unlike timeout 0 (ignored since LWS 3.2), -1 never waits. LWS services
   * scheduled timers and its live descriptor table, including TLS bytes
   * buffered without socket readiness. Leave a readable continuation
   * signal if forced work remains, so one call stays bounded and Scheme won't
   * sleep on a drained socket before advancing again. */
  while (read(state->wakeup_pipe[0], bytes, sizeof(bytes)) > 0) {}
  (void)p_lws_service_tsi(context, -1, 0);
  state->forced_pending = p_lws_service_adjust_timeout(context, 1, 0) == 0;
  if (state->forced_pending) (void)write(state->wakeup_pipe[1], &byte, 1);
}

static void service_context_locked(struct lws_context *context, int timeout_ms) {
  chezpp_ws_poll_state *state;
  chezpp_ws_poll_fd *entry;
  struct pollfd *pollfds = NULL;
  int count = 0;
  int index = 0;

  if (context == NULL || !context_registered_unlocked(context)) return;
  service_context_ready_locked(context);
  if (timeout_ms <= 0) return;
  state = (chezpp_ws_poll_state *)p_lws_context_user(context);
  if (state == NULL) return;
  if (state->forced_pending) return;
  for (entry = state->head; entry != NULL; entry = entry->next) {
    if (entry->fd >= 0 && entry->events != 0) count += 1;
  }
  if (count == 0) return;
  pollfds = (struct pollfd *)calloc((size_t)count, sizeof(struct pollfd));
  if (pollfds == NULL) return;
  for (entry = state->head; entry != NULL; entry = entry->next) {
    if (entry->fd >= 0 && entry->events != 0) {
      pollfds[index].fd = entry->fd;
      pollfds[index].events = (short)entry->events;
      index += 1;
    }
  }
  /* The snapshot is only for waiting; LWS dispatches its current table itself.
   * Never dispatch stale entries after a callback closes or replaces a WSI. */
  (void)poll(pollfds, (nfds_t)count, timeout_ms);
  free(pollfds);
  service_context_ready_locked(context);
}

static void service_context(struct lws_context *primary, int timeout_ms) {
  pthread_mutex_lock(&context_list_mutex);
  service_context_locked(primary, timeout_ms);
  pthread_mutex_unlock(&context_list_mutex);
}

static void service_registered(struct lws_context *primary, int timeout_ms) {
  chezpp_ws_context_node *node;
  int used_primary = 0;

  pthread_mutex_lock(&context_list_mutex);
  node = context_list;
  while (node != NULL) {
    struct lws_context *context = node->context;
    int step = (context == primary && !used_primary) ? timeout_ms : 0;
    service_context_locked(context, step);
    if (context == primary) used_primary = 1;
    node = node->next;
  }
  pthread_mutex_unlock(&context_list_mutex);
}

static int context_listen_port(struct lws_context *context, const char *vhost_name,
                               int fallback) {
  struct lws_vhost *vhost = NULL;
  int port = fallback;

  pthread_mutex_lock(&context_list_mutex);
  if (context_registered_unlocked(context)) {
    vhost = p_lws_get_vhost_by_name(context, vhost_name);
    if (vhost == NULL) vhost = p_lws_get_vhost_by_name(context, "default");
    if (vhost != NULL) port = p_lws_get_vhost_listen_port(vhost);
  }
  pthread_mutex_unlock(&context_list_mutex);
  return port;
}

static struct lws *connect_context(struct lws_context *context,
                                   const struct lws_client_connect_info *info) {
  struct lws *wsi = NULL;

  pthread_mutex_lock(&context_list_mutex);
  if (context_registered_unlocked(context))
    wsi = p_lws_client_connect_via_info(info);
  pthread_mutex_unlock(&context_list_mutex);
  return wsi;
}

static void clear_opaque_user_data(struct lws_context *context, struct lws *wsi) {
  pthread_mutex_lock(&context_list_mutex);
  if (context_registered_unlocked(context) && wsi != NULL &&
      p_lws_set_opaque_user_data != NULL)
    p_lws_set_opaque_user_data(wsi, NULL);
  pthread_mutex_unlock(&context_list_mutex);
}

static void close_wsi(struct lws_context *context, struct lws *wsi,
                      int code, const unsigned char *reason, size_t reason_len) {
  pthread_mutex_lock(&context_list_mutex);
  if (context_registered_unlocked(context) && wsi != NULL) {
    p_lws_close_reason(wsi, (enum lws_close_status)code,
                       (unsigned char *)reason, reason_len);
    p_lws_set_timeout(wsi, PENDING_TIMEOUT_CLOSE_SEND, LWS_TO_KILL_SYNC);
  }
  pthread_mutex_unlock(&context_list_mutex);
}

static void request_writable(struct lws_context *context, struct lws *wsi) {
  pthread_mutex_lock(&context_list_mutex);
  if (context_registered_unlocked(context) && wsi != NULL)
    p_lws_callback_on_writable(wsi);
  pthread_mutex_unlock(&context_list_mutex);
}

static void clear_messages(chezpp_ws_connection *conn) {
  chezpp_ws_message *msg = conn->msg_head;
  while (msg != NULL) {
    chezpp_ws_message *next = msg->next;
    if (msg->data != NULL) free(msg->data);
    free(msg);
    msg = next;
  }
  conn->msg_head = NULL;
  conn->msg_tail = NULL;
}

static void clear_pending_send(chezpp_ws_connection *conn) {
  if (conn->pending_send != NULL) {
    if (conn->pending_send->buf != NULL) free(conn->pending_send->buf);
    free(conn->pending_send);
    conn->pending_send = NULL;
  }
}

static void clear_rx(chezpp_ws_connection *conn) {
  if (conn->rx_buf != NULL) free(conn->rx_buf);
  conn->rx_buf = NULL;
  conn->rx_len = 0;
  conn->rx_cap = 0;
  conn->rx_type = 0;
}

static void set_negotiated_protocol(chezpp_ws_connection *conn, struct lws *wsi) {
  const struct lws_protocols *protocol;
  if (conn == NULL || wsi == NULL || p_lws_get_protocol == NULL) return;
  if (p_lws_hdr_total_length(wsi, WSI_TOKEN_PROTOCOL) <= 0) return;
  protocol = p_lws_get_protocol(wsi);
  if (protocol != NULL && protocol->name != NULL) {
    if (conn->negotiated_protocol != NULL) free(conn->negotiated_protocol);
    conn->negotiated_protocol = ws_strdup(protocol->name);
  }
}

static void copy_handshake_value(char **slot, struct lws *wsi,
                                 enum lws_token_indexes token) {
  char buffer[512];
  int length;
  if (slot == NULL || wsi == NULL || p_lws_hdr_copy == NULL) return;
  length = p_lws_hdr_copy(wsi, buffer, (int)sizeof(buffer), token);
  if (length <= 0) return;
  if (*slot != NULL) free(*slot);
  *slot = ws_strdup(buffer);
}

static void set_error(char **slot, const char *msg) {
  if (*slot != NULL) free(*slot);
  *slot = ws_strdup(msg == NULL ? "websocket error" : msg);
}

static void destroy_server_resources(chezpp_ws_server *server) {
  if (server->context != NULL) {
    destroy_registered_context(server->context);
    server->context = NULL;
  }
  free_server_protocols(server);
  if (server->protocol_name != NULL) free(server->protocol_name);
  if (server->error != NULL) free(server->error);
  clear_poll_fds(&server->poll_state);
  free(server);
}

static void maybe_release_server(chezpp_ws_server *server) {
  if (server != NULL && server->closed && server->live_count <= 0) {
    destroy_server_resources(server);
  }
}

static chezpp_ws_connection *pop_accept(chezpp_ws_server *server) {
  chezpp_ws_connection *conn = server->accept_head;
  if (conn == NULL) return NULL;
  server->accept_head = conn->next_accept;
  if (server->accept_head == NULL) server->accept_tail = NULL;
  conn->next_accept = NULL;
  conn->handed_out = 1;
  return conn;
}

static int push_message(chezpp_ws_connection *conn, int type, const void *data, size_t len) {
  chezpp_ws_message *msg = (chezpp_ws_message *)calloc(1, sizeof(chezpp_ws_message));
  if (msg == NULL) return 0;
  if (len > 0) {
    msg->data = (unsigned char *)malloc(len);
    if (msg->data == NULL) {
      free(msg);
      return 0;
    }
    memcpy(msg->data, data, len);
  }
  msg->type = type;
  msg->len = len;
  if (conn->msg_tail == NULL) {
    conn->msg_head = msg;
    conn->msg_tail = msg;
  } else {
    conn->msg_tail->next = msg;
    conn->msg_tail = msg;
  }
  return 1;
}

static ptr pop_message_scheme(chezpp_ws_connection *conn) {
  chezpp_ws_message *msg = conn->msg_head;
  ptr v;
  ptr bv;
  if (msg == NULL) return make_status("would-block", Sfalse);
  conn->msg_head = msg->next;
  if (conn->msg_head == NULL) conn->msg_tail = NULL;
  bv = Smake_bytevector((iptr)msg->len, 0);
  if (msg->len > 0) memcpy(Sbytevector_data(bv), msg->data, msg->len);
  v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(ws_type_symbol(msg->type)));
  Svector_set(v, 1, bv);
  if (msg->data != NULL) free(msg->data);
  free(msg);
  return v;
}

static int ensure_rx_capacity(chezpp_ws_connection *conn, size_t extra) {
  size_t need = conn->rx_len + extra;
  unsigned char *buf;
  if (need <= conn->rx_cap) return 1;
  if (conn->rx_cap == 0)
    conn->rx_cap = need;
  else
    while (conn->rx_cap < need) conn->rx_cap *= 2;
  if (conn->rx_cap < need) conn->rx_cap = need;
  buf = (unsigned char *)realloc(conn->rx_buf, conn->rx_cap);
  if (buf == NULL) return 0;
  conn->rx_buf = buf;
  return 1;
}

static int queue_send(chezpp_ws_connection *conn, int type, int first, int final,
                      const unsigned char *data, size_t len) {
  chezpp_ws_send *send = (chezpp_ws_send *)calloc(1, sizeof(chezpp_ws_send));
  if (send == NULL) return 0;
  send->buf = (unsigned char *)malloc(LWS_PRE + len);
  if (send->buf == NULL) {
    free(send);
    return 0;
  }
  if (len > 0) memcpy(send->buf + LWS_PRE, data, len);
  send->type = type;
  send->first = first;
  send->final = final;
  send->len = len;
  conn->pending_send = send;
  return 1;
}

static enum lws_write_protocol write_protocol_for_send(const chezpp_ws_send *send) {
  enum lws_write_protocol protocol;
  if (!send->first)
    protocol = LWS_WRITE_CONTINUATION;
  else
  switch (send->type) {
  case CHEZPP_WS_TEXT:
    protocol = LWS_WRITE_TEXT;
    break;
  case CHEZPP_WS_BINARY:
    protocol = LWS_WRITE_BINARY;
    break;
  case CHEZPP_WS_PING:
    protocol = LWS_WRITE_PING;
    break;
  case CHEZPP_WS_PONG:
    protocol = LWS_WRITE_PONG;
    break;
  default:
    protocol = LWS_WRITE_BINARY;
    break;
  }
  if (!send->final) protocol = (enum lws_write_protocol)(protocol | LWS_WRITE_NO_FIN);
  return protocol;
}

static int websocket_callback(struct lws *wsi, enum lws_callback_reasons reason, void *user,
                              void *in, size_t len) {
  chezpp_ws_connection *conn = NULL;
  void *context_user = NULL;

  (void)user;

  if (wsi != NULL && p_lws_get_opaque_user_data != NULL)
    conn = (chezpp_ws_connection *)p_lws_get_opaque_user_data(wsi);
  if (wsi != NULL)
    context_user = p_lws_context_user(p_lws_get_context(wsi));

  ws_tracef("callback reason=%d wsi=%p conn=%p", (int)reason, (void *)wsi, (void *)conn);

  switch (reason) {
  case LWS_CALLBACK_OPENSSL_LOAD_EXTRA_SERVER_VERIFY_CERTS: {
    chezpp_ws_server *server =
        in == NULL ? NULL : (chezpp_ws_server *)p_lws_vhost_user((struct lws_vhost *)in);
    if (server == NULL || server->tls_context_handle == 0) return 0;
    return chezpp_net_tls_context_copy_credentials(server->tls_context_handle, user) ? 0 : -1;
  }
  case LWS_CALLBACK_ADD_POLL_FD:
  case LWS_CALLBACK_CHANGE_MODE_POLL_FD: {
    const struct lws_pollargs *args = (const struct lws_pollargs *)in;
    if (args != NULL)
      update_poll_fd((chezpp_ws_poll_state *)context_user, wsi,
                     args->fd, args->events);
    return 0;
  }
  case LWS_CALLBACK_DEL_POLL_FD: {
    const struct lws_pollargs *args = (const struct lws_pollargs *)in;
    if (args != NULL)
      remove_poll_fd((chezpp_ws_poll_state *)context_user, args->fd);
    return 0;
  }
  case LWS_CALLBACK_ESTABLISHED: {
    chezpp_ws_server *server =
        (chezpp_ws_server *)p_lws_context_user(p_lws_get_context(wsi));
    chezpp_ws_connection *accepted =
        (chezpp_ws_connection *)calloc(1, sizeof(chezpp_ws_connection));
    if (accepted == NULL) return -1;
    ws_tracef("server established context=%p server=%p", (void *)server->context, (void *)server);
    accepted->context = server->context;
    accepted->wsi = wsi;
    accepted->server = server;
    accepted->established = 1;
    accepted->client_side = 0;
    accepted->close_code = LWS_CLOSE_STATUS_NOSTATUS;
    set_negotiated_protocol(accepted, wsi);
    server->live_count += 1;
    p_lws_set_opaque_user_data(wsi, accepted);
    assign_poll_connection(&server->poll_state, wsi, accepted);
    if (server->accept_tail == NULL) {
      server->accept_head = accepted;
      server->accept_tail = accepted;
    } else {
      server->accept_tail->next_accept = accepted;
      server->accept_tail = accepted;
    }
    return 0;
  }
  case LWS_CALLBACK_CLIENT_ESTABLISHED:
    if (conn != NULL) {
      conn->wsi = wsi;
      conn->established = 1;
      conn->close_code = LWS_CLOSE_STATUS_NOSTATUS;
      assign_poll_connection(&conn->poll_state, wsi, conn);
      ws_tracef("client established conn=%p", (void *)conn);
    }
    return 0;
  case LWS_CALLBACK_CLIENT_FILTER_PRE_ESTABLISH:
    if (conn != NULL) {
      copy_handshake_value(&conn->negotiated_protocol, wsi,
                           WSI_TOKEN_PROTOCOL);

    }
    return 0;
  case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
    if (conn == NULL && wsi != NULL) {
      conn = (chezpp_ws_connection *)p_lws_context_user(p_lws_get_context(wsi));
    }
    if (conn != NULL) {
      conn->failed = 1;
      set_error(&conn->error, in == NULL ? "websocket connection failed" : (const char *)in);
      ws_tracef("client connection error conn=%p message=%s", (void *)conn,
                in == NULL ? "websocket connection failed" : (const char *)in);
    }
    return 0;
  case LWS_CALLBACK_RECEIVE:
  case LWS_CALLBACK_CLIENT_RECEIVE:
    if (conn == NULL) return 0;
    ws_tracef("receive conn=%p len=%zu final=%d remaining=%zu", (void *)conn, len,
              p_lws_is_final_fragment(wsi), p_lws_remaining_packet_payload(wsi));
    if (conn->rx_len == 0)
      conn->rx_type = p_lws_frame_is_binary(wsi) ? CHEZPP_WS_BINARY : CHEZPP_WS_TEXT;
    if (!ensure_rx_capacity(conn, len)) {
      conn->failed = 1;
      set_error(&conn->error, "out of memory while receiving websocket data");
      return -1;
    }
    memcpy(conn->rx_buf + conn->rx_len, in, len);
    conn->rx_len += len;
    if (p_lws_is_final_fragment(wsi) && p_lws_remaining_packet_payload(wsi) == 0) {
      if (!push_message(conn, conn->rx_type, conn->rx_buf, conn->rx_len)) {
        conn->failed = 1;
        set_error(&conn->error, "out of memory while queueing websocket message");
        return -1;
      }
      clear_rx(conn);
    }
    return 0;
  case LWS_CALLBACK_RECEIVE_PONG:
  case LWS_CALLBACK_CLIENT_RECEIVE_PONG:
    if (conn == NULL) return 0;
    conn->pong_count += 1;
    if (!push_message(conn, CHEZPP_WS_PONG, in, len)) {
      conn->failed = 1;
      set_error(&conn->error, "out of memory while queueing websocket pong");
      return -1;
    }
    return 0;
  case LWS_CALLBACK_SERVER_WRITEABLE:
  case LWS_CALLBACK_CLIENT_WRITEABLE:
    if (conn == NULL || conn->pending_send == NULL) return 0;
    if (p_lws_write(wsi, conn->pending_send->buf + LWS_PRE, conn->pending_send->len,
                    write_protocol_for_send(conn->pending_send)) <
        (int)conn->pending_send->len) {
      conn->failed = 1;
      set_error(&conn->error, "websocket write failed");
      clear_pending_send(conn);
      return -1;
    }
    conn->fragment_open = !conn->pending_send->final;
    clear_pending_send(conn);
    return 0;
  case LWS_CALLBACK_WS_PEER_INITIATED_CLOSE:
    if (conn != NULL) {
      size_t reason_len = len > 2 ? len - 2 : 0;
      conn->close_code = len >= 2
                             ? (((const unsigned char *)in)[0] << 8) |
                                   ((const unsigned char *)in)[1]
                             : LWS_CLOSE_STATUS_NOSTATUS;
      if (conn->close_reason != NULL) free(conn->close_reason);
      conn->close_reason = (char *)calloc(reason_len + 1, 1);
      if (conn->close_reason != NULL && reason_len > 0)
        memcpy(conn->close_reason, (const unsigned char *)in + 2, reason_len);
    }
    return 0;
  case LWS_CALLBACK_CLOSED:
  case LWS_CALLBACK_CLIENT_CLOSED:
    if (conn != NULL) {
      conn->wsi = NULL;
      conn->closed = 1;
      ws_tracef("connection closed conn=%p", (void *)conn);
    }
    return 0;
  default:
    return 0;
  }
}

static int wait_for_condition(struct lws_context *primary, int (*pred)(void *), void *data,
                              int timeout_ms, int service_all) {
  long long start_ms = 0;

  if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return 0;

  for (;;) {
    int step_ms;
    if (pred(data)) return 1;
    if (timeout_ms >= 0) {
      int remaining = remaining_timeout_ms(timeout_ms, start_ms);
      if (remaining <= 0) return pred(data);
      step_ms = remaining < CHEZPP_WS_SERVICE_STEP_MS ? remaining : CHEZPP_WS_SERVICE_STEP_MS;
    } else {
      step_ms = CHEZPP_WS_SERVICE_STEP_MS;
    }
    if (service_all)
      service_registered(primary, step_ms);
    else
      service_context(primary, step_ms);
    if (pred(data)) return 1;
  }
}

static int pred_connected(void *data) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)data;
  return conn->established || conn->failed;
}

static int pred_accept_ready(void *data) {
  chezpp_ws_server *server = (chezpp_ws_server *)data;
  return server->accept_head != NULL || server->closed;
}

static int pred_recv_ready(void *data) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)data;
  return conn->msg_head != NULL || conn->closed || conn->failed;
}

static int pred_send_done(void *data) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)data;
  return conn->pending_send == NULL || conn->closed || conn->failed;
}

static ptr connection_error_status(chezpp_ws_connection *conn, const char *fallback) {
  if (conn != NULL && conn->error != NULL) return make_error_status_message(conn->error);
  return make_error_status_message(fallback);
}

ptr chezpp_net_websocket_listen(const char *host, int port, const char *protocol_name,
                                const char *offered_protocols,
                                uptr tls_context_handle) {
  chezpp_ws_server *server;
  struct lws_context_creation_info info;

  if (!ensure_websocket_loaded())
    return make_error_status_message(
        chezpp_optional_library_error(&websocket_library));
  if (!ensure_websocket_lifetime_context())
    return make_error_status_message("failed to initialize libwebsockets");

  server = (chezpp_ws_server *)calloc(1, sizeof(chezpp_ws_server));
  if (server == NULL) return make_status("error", errno_str());
  server->protocol_name = ws_strdup((protocol_name == NULL || *protocol_name == 0)
                                        ? CHEZPP_WS_DEFAULT_PROTOCOL
                                        : protocol_name);
  if (server->protocol_name == NULL) {
    free(server);
    return make_status("error", errno_str());
  }
  if (!initialize_server_protocols(server, offered_protocols)) {
    free(server->protocol_name);
    free(server);
    return make_status("error", errno_str());
  }
  server->tls_context_handle = tls_context_handle;

  memset(&info, 0, sizeof(info));
  info.port = port;
  info.iface = (host != NULL && *host != 0) ? host : NULL;
  info.protocols = server->protocols;
  info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
  if (tls_context_handle != 0)
    info.options |= LWS_SERVER_OPTION_CREATE_VHOST_SSL_CTX;
  info.vhost_name = (host != NULL && *host != 0) ? host : "localhost";
  info.user = server;
  info.gid = (gid_t)-1;
  info.uid = (uid_t)-1;

  server->context = create_registered_context(&info);
  if (server->context == NULL) {
    free_server_protocols(server);
    free(server->protocol_name);
    free(server);
    return make_error_status_message("failed to create websocket server context");
  }

  ws_tracef("listen host=%s port=%d protocol=%s context=%p",
            host == NULL ? "(null)" : host, port, server->protocol_name, (void *)server->context);
  server->port = context_listen_port(server->context, info.vhost_name, port);
  return make_websocket_handle((uptr)server);
}

ptr chezpp_net_websocket_server_close(uptr handle) {
  chezpp_ws_server *server = (chezpp_ws_server *)TO_VOIDP(handle);
  chezpp_ws_connection *queued;
  if (server == NULL) return Strue;
  if (!server->closed) server->closed = 1;
  queued = server->accept_head;
  server->accept_head = NULL;
  server->accept_tail = NULL;
  while (queued != NULL) {
    chezpp_ws_connection *next = queued->next_accept;
    clear_opaque_user_data(queued->context, queued->wsi);
    clear_messages(queued);
    clear_pending_send(queued);
    clear_rx(queued);
    if (queued->error != NULL) free(queued->error);
    if (queued->close_reason != NULL) free(queued->close_reason);
    if (queued->negotiated_protocol != NULL) free(queued->negotiated_protocol);
    if (server->live_count > 0) server->live_count -= 1;
    free(queued);
    queued = next;
  }
  maybe_release_server(server);
  return Strue;
}

ptr chezpp_net_websocket_accept(uptr handle, int nonblocking, int timeout_ms) {
  chezpp_ws_server *server = (chezpp_ws_server *)TO_VOIDP(handle);
  chezpp_ws_connection *conn;
  if (server == NULL || server->closed) return make_error_status_message("invalid websocket server");
  if (server->accept_head == NULL && nonblocking) {
    service_registered(server->context, 0);
  }
  if (server->accept_head == NULL && !nonblocking &&
      !wait_for_condition(server->context, pred_accept_ready, server, timeout_ms, 1))
    return make_error_status_message("websocket accept timed out");
  if (server->accept_head == NULL) {
    if (server->closed) return Seof_object;
    return make_would_block_status(&server->poll_state, POLLIN, NULL, 1);
  }
  conn = pop_accept(server);
  ws_tracef("accept conn=%p server=%p", (void *)conn, (void *)server);
  return make_websocket_handle((uptr)conn);
}

ptr chezpp_net_websocket_connect(const char *host, int port, const char *path,
                                 const char *protocol_name, const char *offered_protocols,
                                 int secure, uptr tls_context_handle,
                                 int timeout_ms) {
  chezpp_ws_connection *conn;
  struct lws_context_creation_info info;
  struct lws_client_connect_info ccinfo;
  const char *protocol = (protocol_name == NULL || *protocol_name == 0)
                             ? CHEZPP_WS_DEFAULT_PROTOCOL
                             : protocol_name;
  char origin[512];

  if (!ensure_websocket_loaded())
    return make_error_status_message(
        chezpp_optional_library_error(&websocket_library));
  if (!ensure_websocket_lifetime_context())
    return make_error_status_message("failed to initialize libwebsockets");

  conn = (chezpp_ws_connection *)calloc(1, sizeof(chezpp_ws_connection));
  if (conn == NULL) return make_status("error", errno_str());
  conn->owns_context = 1;
  conn->client_side = 1;
  conn->protocol_name = ws_strdup(protocol);
  if (conn->protocol_name == NULL) {
    free(conn);
    return make_status("error", errno_str());
  }

  memset(&info, 0, sizeof(info));
  conn->protocols[0].name = conn->protocol_name;
  conn->protocols[0].callback = websocket_callback;
  conn->protocols[0].per_session_data_size = 0;
  conn->protocols[0].rx_buffer_size = 0;
  conn->protocols[1] = (struct lws_protocols)LWS_PROTOCOL_LIST_TERM;
  info.port = CONTEXT_PORT_NO_LISTEN;
  info.protocols = conn->protocols;
  info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
  if (tls_context_handle != 0)
    info.provided_client_ssl_ctx =
        (SSL_CTX *)chezpp_net_tls_context_native(tls_context_handle);
  info.user = conn;
  info.gid = (gid_t)-1;
  info.uid = (uid_t)-1;

  conn->context = create_registered_context(&info);
  if (conn->context == NULL) {
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("failed to create websocket client context");
  }
  ws_tracef("connect-start host=%s port=%d path=%s protocol=%s context=%p",
            host, port, (path != NULL && *path != 0) ? path : "/", protocol, (void *)conn->context);

  memset(&ccinfo, 0, sizeof(ccinfo));
  snprintf(origin, sizeof(origin), "%s://%s:%d", secure ? "https" : "http", host, port);
  ccinfo.context = conn->context;
  ccinfo.address = host;
  ccinfo.port = port;
  ccinfo.path = (path != NULL && *path != 0) ? path : "/";
  ccinfo.host = host;
  ccinfo.origin = origin;
  ccinfo.protocol = offered_protocols;
  ccinfo.local_protocol_name = protocol;
  ccinfo.ietf_version_or_minus_one = -1;
  ccinfo.opaque_user_data = conn;
  ccinfo.ssl_connection = secure ? LCCSCF_USE_SSL : 0;
  conn->wsi = connect_context(conn->context, &ccinfo);
  if (conn->wsi == NULL) {
    destroy_registered_context(conn->context);
    clear_poll_fds(&conn->poll_state);
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("failed to start websocket client connection");
  }

  if (timeout_ms < 0) return make_websocket_handle((uptr)conn);

  if (!wait_for_condition(conn->context, pred_connected, conn, timeout_ms, 1)) {
    ws_tracef("connect-timeout host=%s port=%d path=%s protocol=%s", host, port,
              (path != NULL && *path != 0) ? path : "/", protocol);
    destroy_registered_context(conn->context);
    clear_poll_fds(&conn->poll_state);
    if (conn->error != NULL) free(conn->error);
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("websocket connect timed out");
  }
  if (conn->failed) {
    ptr err = connection_error_status(conn, "websocket connection failed");
    destroy_registered_context(conn->context);
    clear_poll_fds(&conn->poll_state);
    if (conn->error != NULL) free(conn->error);
    free(conn->protocol_name);
    free(conn);
    return err;
  }

  return make_websocket_handle((uptr)conn);
}

ptr chezpp_net_websocket_connect_step(uptr handle) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);

  if (conn == NULL || conn->closed)
    return make_error_status_message("invalid websocket connection");
  if (!conn->established && !conn->failed)
    service_registered(conn->context, 0);
  if (conn->failed)
    return connection_error_status(conn, "websocket connection failed");
  if (conn->established) return Strue;
  return make_would_block_status(&conn->poll_state, POLLIN | POLLOUT,
                                 conn->wsi, 0);
}

ptr chezpp_net_websocket_poll_targets(uptr handle, int server_handle) {
  struct lws_context *context;
  chezpp_ws_poll_state *state;
  chezpp_ws_poll_fd *entry;
  ptr targets;
  int count = 1;
  int index = 0;

  if (handle == 0) return make_error_status_message("invalid websocket context");
  context = server_handle
      ? ((chezpp_ws_server *)TO_VOIDP(handle))->context
      : ((chezpp_ws_connection *)TO_VOIDP(handle))->context;
  pthread_mutex_lock(&context_list_mutex);
  if (context == NULL || !context_registered_unlocked(context)) {
    pthread_mutex_unlock(&context_list_mutex);
    return make_error_status_message("invalid websocket context");
  }
  state = (chezpp_ws_poll_state *)p_lws_context_user(context);
  for (entry = state->head; entry != NULL; entry = entry->next)
    if (entry->fd >= 0 && entry->events != 0) count++;
  targets = Smake_vector(count, Sfalse);
  for (entry = state->head; entry != NULL; entry = entry->next) {
    ptr item;
    if (entry->fd < 0 || entry->events == 0) continue;
    item = Smake_vector(2, Sfalse);
    Svector_set(item, 0, Sfixnum(entry->fd));
    Svector_set(item, 1, Sfixnum(entry->events));
    Svector_set(targets, index++, item);
  }
  {
    ptr item = Smake_vector(2, Sfalse);
    Svector_set(item, 0, Sfixnum(state->wakeup_pipe[0]));
    Svector_set(item, 1, Sfixnum(POLLIN));
    Svector_set(targets, index, item);
  }
  pthread_mutex_unlock(&context_list_mutex);
  return targets;
}

static ptr websocket_close_with_reason(uptr handle, int code,
                                       const unsigned char *reason, size_t reason_len) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  chezpp_ws_server *server = NULL;
  if (conn == NULL) return Strue;
  server = conn->server;
  clear_opaque_user_data(conn->context, conn->wsi);
  if (!conn->closed && conn->established && conn->wsi != NULL) {
    close_wsi(conn->context, conn->wsi, code, reason, reason_len);
    service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
  }
  conn->wsi = NULL;
  conn->closed = 1;
  clear_messages(conn);
  clear_pending_send(conn);
  clear_rx(conn);
  if (conn->owns_context && conn->context != NULL) {
    destroy_registered_context(conn->context);
    conn->context = NULL;
    clear_poll_fds(&conn->poll_state);
  } else if (server != NULL) {
    if (server->live_count > 0) server->live_count -= 1;
    maybe_release_server(server);
  }
  if (conn->error != NULL) free(conn->error);
  if (conn->protocol_name != NULL) free(conn->protocol_name);
  if (conn->close_reason != NULL) free(conn->close_reason);
  if (conn->negotiated_protocol != NULL) free(conn->negotiated_protocol);
  free(conn);
  return Strue;
}

ptr chezpp_net_websocket_close(uptr handle) {
  return websocket_close_with_reason(handle, LWS_CLOSE_STATUS_NORMAL, NULL, 0);
}

ptr chezpp_net_websocket_close_with_reason(uptr handle, int code, ptr reason) {
  return websocket_close_with_reason(handle, code, Sbytevector_data(reason),
                                     (size_t)Sbytevector_length(reason));
}

ptr chezpp_net_websocket_state(uptr handle) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  ptr state;
  if (conn == NULL) return make_error_status_message("invalid websocket connection");
  if (!conn->closed && conn->context != NULL) service_context(conn->context, 0);
  state = Smake_vector(4, Sfalse);
  Svector_set(state, 0, conn->negotiated_protocol == NULL
                            ? Sfalse : Sstring(conn->negotiated_protocol));
  Svector_set(state, 1, conn->close_code == LWS_CLOSE_STATUS_NOSTATUS
                            ? Sfalse : Sfixnum(conn->close_code));
  Svector_set(state, 2, conn->close_reason == NULL
                            ? Sstring("") : Sstring(conn->close_reason));
  Svector_set(state, 3, Sunsigned(conn->pong_count));
  return state;
}

ptr chezpp_net_websocket_cancel_send(uptr handle) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);

  if (conn == NULL || conn->closed || conn->wsi == NULL)
    return make_error_status_message("invalid websocket connection");

  clear_pending_send(conn);
  service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
  if (conn->failed) return connection_error_status(conn, "websocket send failed");
  return Strue;
}

static ptr websocket_send_impl(uptr handle, int type, ptr bv, int start, int stop,
                               int first, int final, int nonblocking, int timeout_ms) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  size_t len;
  chezpp_ws_poll_state *poll_state;

  if (conn == NULL || conn->closed || conn->wsi == NULL)
    return make_error_status_message("invalid websocket connection");
  poll_state = conn->server == NULL ? &conn->poll_state : &conn->server->poll_state;

  if (conn->pending_send != NULL) {
    if (nonblocking) {
      size_t pending_len = conn->pending_send->len;
      service_context(conn->context, 0);
      if (conn->pending_send != NULL)
        return make_would_block_status(poll_state, POLLOUT, conn->wsi, 0);
      if (conn->failed) return connection_error_status(conn, "websocket send failed");
      return Sfixnum((iptr)pending_len);
    } else if (!wait_for_condition(conn->context, pred_send_done, conn, timeout_ms, 0)) {
      return make_error_status_message("websocket send timed out");
    }
  }

  if (conn->failed) return connection_error_status(conn, "websocket send failed");

  if ((first && conn->fragment_open) || (!first && !conn->fragment_open))
    return make_error_status_message("invalid websocket fragment sequence");

  len = (size_t)(stop - start);
  if (!queue_send(conn, type, first, final, Sbytevector_data(bv) + start, len))
    return make_status("error", errno_str());
  request_writable(conn->context, conn->wsi);

  if (nonblocking) {
    service_context(conn->context, 0);
    if (conn->pending_send != NULL)
      return make_would_block_status(poll_state, POLLOUT, conn->wsi, 0);
  } else if (!wait_for_condition(conn->context, pred_send_done, conn, timeout_ms, 0)) {
    clear_pending_send(conn);
    return make_error_status_message("websocket send timed out");
  }

  if (conn->failed) return connection_error_status(conn, "websocket send failed");
  return Sfixnum((iptr)len);
}

ptr chezpp_net_websocket_send(uptr handle, int type, ptr bv, int start, int stop,
                              int nonblocking, int timeout_ms) {
  return websocket_send_impl(handle, type, bv, start, stop, 1, 1,
                             nonblocking, timeout_ms);
}

ptr chezpp_net_websocket_send_fragment(uptr handle, int type, ptr bv, int start, int stop,
                                       int final, int nonblocking, int timeout_ms) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  int first = conn == NULL ? 1 : !conn->fragment_open;
  return websocket_send_impl(handle, type, bv, start, stop, first, final,
                             nonblocking, timeout_ms);
}

ptr chezpp_net_websocket_recv(uptr handle, int nonblocking, int timeout_ms) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  chezpp_ws_poll_state *poll_state;

  if (conn == NULL || (conn->closed && conn->msg_head == NULL))
    return make_error_status_message("invalid websocket connection");
  poll_state = conn->server == NULL ? &conn->poll_state : &conn->server->poll_state;

  if (conn->msg_head == NULL) {
    if (nonblocking) {
      service_context(conn->context, 0);
    } else if (!wait_for_condition(conn->context, pred_recv_ready, conn, timeout_ms, 0)) {
      return make_error_status_message("websocket receive timed out");
    }
  }

  if (conn->msg_head != NULL) return pop_message_scheme(conn);
  if (conn->failed) return connection_error_status(conn, "websocket receive failed");
  if (conn->closed) return Seof_object;
  return make_would_block_status(poll_state, POLLIN, conn->wsi, 0);
}
