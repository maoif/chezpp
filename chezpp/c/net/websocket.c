#include "../common.h"

#include <dlfcn.h>
#include <libwebsockets.h>

typedef struct chezpp_ws_message chezpp_ws_message;
typedef struct chezpp_ws_send chezpp_ws_send;
typedef struct chezpp_ws_server chezpp_ws_server;
typedef struct chezpp_ws_connection chezpp_ws_connection;
typedef struct chezpp_ws_context_node chezpp_ws_context_node;

struct chezpp_ws_message {
  chezpp_ws_message *next;
  int type;
  size_t len;
  unsigned char *data;
};

struct chezpp_ws_send {
  int type;
  size_t len;
  unsigned char *buf;
};

struct chezpp_ws_server {
  struct lws_context *context;
  struct lws_protocols protocols[2];
  char *protocol_name;
  int port;
  int closed;
  char *error;
  chezpp_ws_connection *accept_head;
  chezpp_ws_connection *accept_tail;
};

struct chezpp_ws_connection {
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
typedef int (*lws_service_fn)(struct lws_context *, int);
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

static void *websocket_handle = NULL;
static lws_create_context_fn p_lws_create_context = NULL;
static lws_context_destroy_fn p_lws_context_destroy = NULL;
static lws_client_connect_via_info_fn p_lws_client_connect_via_info = NULL;
static lws_service_fn p_lws_service = NULL;
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
static chezpp_ws_context_node *context_list = NULL;

enum {
  CHEZPP_WS_TEXT = 1,
  CHEZPP_WS_BINARY = 2,
  CHEZPP_WS_PING = 3,
  CHEZPP_WS_PONG = 4
};

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

static int load_symbol(void **out, const char *name) {
  *out = dlsym(websocket_handle, name);
  return *out != NULL;
}

static int ensure_websocket_loaded(void) {
  const char *names[] = {"libwebsockets.so.20", "libwebsockets.so", NULL};
  int i;

  if (websocket_handle != NULL) return 1;

  for (i = 0; names[i] != NULL; ++i) {
    websocket_handle = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (websocket_handle != NULL) break;
  }
  if (websocket_handle == NULL) return 0;

  if (!load_symbol((void **)&p_lws_create_context, "lws_create_context") ||
      !load_symbol((void **)&p_lws_context_destroy, "lws_context_destroy") ||
      !load_symbol((void **)&p_lws_client_connect_via_info, "lws_client_connect_via_info") ||
      !load_symbol((void **)&p_lws_service, "lws_service") ||
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
      !load_symbol((void **)&p_lws_get_vhost_listen_port, "lws_get_vhost_listen_port")) {
    dlclose(websocket_handle);
    websocket_handle = NULL;
    return 0;
  }

  p_lws_set_log_level(0, NULL);
  return 1;
}

static void register_context(struct lws_context *context) {
  chezpp_ws_context_node *node = (chezpp_ws_context_node *)calloc(1, sizeof(chezpp_ws_context_node));
  if (node == NULL) return;
  node->context = context;
  node->next = context_list;
  context_list = node;
}

static void unregister_context(struct lws_context *context) {
  chezpp_ws_context_node **pp = &context_list;
  while (*pp != NULL) {
    if ((*pp)->context == context) {
      chezpp_ws_context_node *node = *pp;
      *pp = node->next;
      free(node);
      return;
    }
    pp = &(*pp)->next;
  }
}

static void service_context(struct lws_context *primary, int timeout_ms) {
  if (primary != NULL) p_lws_service(primary, timeout_ms);
}

static void service_registered(struct lws_context *primary, int timeout_ms) {
  chezpp_ws_context_node *node = context_list;
  int used_primary = 0;

  while (node != NULL) {
    chezpp_ws_context_node *next = node->next;
    int step = (node->context == primary && !used_primary) ? timeout_ms : 0;
    p_lws_service(node->context, step);
    if (node->context == primary) used_primary = 1;
    node = next;
  }

  if (primary != NULL && !used_primary) p_lws_service(primary, timeout_ms);
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

static void set_error(char **slot, const char *msg) {
  if (*slot != NULL) free(*slot);
  *slot = ws_strdup(msg == NULL ? "websocket error" : msg);
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

static int queue_send(chezpp_ws_connection *conn, int type, const unsigned char *data, size_t len) {
  chezpp_ws_send *send = (chezpp_ws_send *)calloc(1, sizeof(chezpp_ws_send));
  if (send == NULL) return 0;
  send->buf = (unsigned char *)malloc(LWS_PRE + len);
  if (send->buf == NULL) {
    free(send);
    return 0;
  }
  if (len > 0) memcpy(send->buf + LWS_PRE, data, len);
  send->type = type;
  send->len = len;
  conn->pending_send = send;
  return 1;
}

static enum lws_write_protocol write_protocol_for_type(int type) {
  switch (type) {
  case CHEZPP_WS_TEXT:
    return LWS_WRITE_TEXT;
  case CHEZPP_WS_BINARY:
    return LWS_WRITE_BINARY;
  case CHEZPP_WS_PING:
    return LWS_WRITE_PING;
  case CHEZPP_WS_PONG:
    return LWS_WRITE_PONG;
  default:
    return LWS_WRITE_BINARY;
  }
}

static int websocket_callback(struct lws *wsi, enum lws_callback_reasons reason, void *user,
                              void *in, size_t len) {
  chezpp_ws_connection *conn = NULL;

  (void)user;

  if (wsi != NULL && p_lws_get_opaque_user_data != NULL)
    conn = (chezpp_ws_connection *)p_lws_get_opaque_user_data(wsi);

  switch (reason) {
  case LWS_CALLBACK_ESTABLISHED: {
    chezpp_ws_server *server =
        (chezpp_ws_server *)p_lws_context_user(p_lws_get_context(wsi));
    chezpp_ws_connection *accepted =
        (chezpp_ws_connection *)calloc(1, sizeof(chezpp_ws_connection));
    if (accepted == NULL) return -1;
    accepted->context = server->context;
    accepted->wsi = wsi;
    accepted->server = server;
    accepted->established = 1;
    accepted->client_side = 0;
    p_lws_set_opaque_user_data(wsi, accepted);
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
    }
    return 0;
  case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
    if (conn == NULL && wsi != NULL) {
      conn = (chezpp_ws_connection *)p_lws_context_user(p_lws_get_context(wsi));
    }
    if (conn != NULL) {
      conn->failed = 1;
      set_error(&conn->error, in == NULL ? "websocket connection failed" : (const char *)in);
    }
    return 0;
  case LWS_CALLBACK_RECEIVE:
  case LWS_CALLBACK_CLIENT_RECEIVE:
    if (conn == NULL) return 0;
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
                    write_protocol_for_type(conn->pending_send->type)) <
        (int)conn->pending_send->len) {
      conn->failed = 1;
      set_error(&conn->error, "websocket write failed");
      clear_pending_send(conn);
      return -1;
    }
    clear_pending_send(conn);
    return 0;
  case LWS_CALLBACK_CLOSED:
  case LWS_CALLBACK_CLIENT_CLOSED:
    if (conn != NULL) {
      conn->wsi = NULL;
      conn->closed = 1;
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
      service_registered(primary, 0);
    else
      service_context(primary, 0);
    if (pred(data)) return 1;
    if (poll(NULL, 0, step_ms) < 0 && errno != EINTR) return pred(data);
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

ptr chezpp_net_websocket_listen(const char *host, int port, const char *protocol_name) {
  chezpp_ws_server *server;
  struct lws_context_creation_info info;
  struct lws_vhost *vhost;

  if (!ensure_websocket_loaded())
    return make_error_status_message("failed to load libwebsockets");

  server = (chezpp_ws_server *)calloc(1, sizeof(chezpp_ws_server));
  if (server == NULL) return make_status("error", errno_str());
  server->protocol_name = ws_strdup((protocol_name == NULL || *protocol_name == 0)
                                        ? CHEZPP_WS_DEFAULT_PROTOCOL
                                        : protocol_name);
  if (server->protocol_name == NULL) {
    free(server);
    return make_status("error", errno_str());
  }

  memset(&info, 0, sizeof(info));
  server->protocols[0].name = server->protocol_name;
  server->protocols[0].callback = websocket_callback;
  server->protocols[0].per_session_data_size = 0;
  server->protocols[0].rx_buffer_size = 0;
  server->protocols[1] = (struct lws_protocols)LWS_PROTOCOL_LIST_TERM;

  info.port = port;
  info.iface = NULL;
  info.protocols = server->protocols;
  info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;
  info.vhost_name = (host != NULL && *host != 0) ? host : "localhost";
  info.user = server;
  info.gid = (gid_t)-1;
  info.uid = (uid_t)-1;

  server->context = p_lws_create_context(&info);
  if (server->context == NULL) {
    free(server->protocol_name);
    free(server);
    return make_error_status_message("failed to create websocket server context");
  }

  register_context(server->context);
  vhost = p_lws_get_vhost_by_name(server->context, info.vhost_name);
  if (vhost == NULL) vhost = p_lws_get_vhost_by_name(server->context, "default");
  if (vhost != NULL) server->port = p_lws_get_vhost_listen_port(vhost);
  else server->port = port;
  return make_websocket_handle((uptr)server);
}

ptr chezpp_net_websocket_server_close(uptr handle) {
  chezpp_ws_server *server = (chezpp_ws_server *)TO_VOIDP(handle);
  chezpp_ws_connection *queued;
  if (server == NULL) return Strue;
  if (!server->closed) {
    server->closed = 1;
    if (server->context != NULL) {
      unregister_context(server->context);
      p_lws_context_destroy(server->context);
      server->context = NULL;
    }
  }
  queued = server->accept_head;
  while (queued != NULL) {
    chezpp_ws_connection *next = queued->next_accept;
    clear_messages(queued);
    clear_pending_send(queued);
    clear_rx(queued);
    if (queued->error != NULL) free(queued->error);
    free(queued);
    queued = next;
  }
  if (server->protocol_name != NULL) free(server->protocol_name);
  if (server->error != NULL) free(server->error);
  free(server);
  return Strue;
}

ptr chezpp_net_websocket_accept(uptr handle, int nonblocking, int timeout_ms) {
  chezpp_ws_server *server = (chezpp_ws_server *)TO_VOIDP(handle);
  chezpp_ws_connection *conn;
  if (server == NULL || server->closed) return make_error_status_message("invalid websocket server");
  if (server->accept_head == NULL && !nonblocking &&
      !wait_for_condition(server->context, pred_accept_ready, server, timeout_ms, 1))
    return make_error_status_message("websocket accept timed out");
  if (server->accept_head == NULL) {
    if (server->closed) return Seof_object;
    return make_status("would-block", Sfalse);
  }
  conn = pop_accept(server);
  return make_websocket_handle((uptr)conn);
}

ptr chezpp_net_websocket_connect(const char *host, int port, const char *path,
                                 const char *protocol_name, int secure, int timeout_ms) {
  chezpp_ws_connection *conn;
  struct lws_context_creation_info info;
  struct lws_client_connect_info ccinfo;
  const char *protocol = (protocol_name == NULL || *protocol_name == 0)
                             ? CHEZPP_WS_DEFAULT_PROTOCOL
                             : protocol_name;
  char origin[512];

  if (!ensure_websocket_loaded())
    return make_error_status_message("failed to load libwebsockets");

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
  info.user = conn;
  info.gid = (gid_t)-1;
  info.uid = (uid_t)-1;

  conn->context = p_lws_create_context(&info);
  if (conn->context == NULL) {
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("failed to create websocket client context");
  }
  register_context(conn->context);

  memset(&ccinfo, 0, sizeof(ccinfo));
  snprintf(origin, sizeof(origin), "%s://%s:%d", secure ? "https" : "http", host, port);
  ccinfo.context = conn->context;
  ccinfo.address = host;
  ccinfo.port = port;
  ccinfo.path = (path != NULL && *path != 0) ? path : "/";
  ccinfo.host = host;
  ccinfo.origin = origin;
  ccinfo.protocol = protocol;
  ccinfo.local_protocol_name = protocol;
  ccinfo.ietf_version_or_minus_one = -1;
  ccinfo.opaque_user_data = conn;
  ccinfo.ssl_connection = secure ? LCCSCF_USE_SSL : 0;
  conn->wsi = p_lws_client_connect_via_info(&ccinfo);
  if (conn->wsi == NULL) {
    unregister_context(conn->context);
    p_lws_context_destroy(conn->context);
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("failed to start websocket client connection");
  }

  if (!wait_for_condition(conn->context, pred_connected, conn, timeout_ms, 1)) {
    unregister_context(conn->context);
    p_lws_context_destroy(conn->context);
    if (conn->error != NULL) free(conn->error);
    free(conn->protocol_name);
    free(conn);
    return make_error_status_message("websocket connect timed out");
  }
  if (conn->failed) {
    ptr err = connection_error_status(conn, "websocket connection failed");
    unregister_context(conn->context);
    p_lws_context_destroy(conn->context);
    if (conn->error != NULL) free(conn->error);
    free(conn->protocol_name);
    free(conn);
    return err;
  }

  return make_websocket_handle((uptr)conn);
}

ptr chezpp_net_websocket_close(uptr handle) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  if (conn == NULL) return Strue;
  if (!conn->closed && conn->wsi != NULL) {
    p_lws_close_reason(conn->wsi, LWS_CLOSE_STATUS_NORMAL, NULL, 0);
    p_lws_set_timeout(conn->wsi, PENDING_TIMEOUT_CLOSE_SEND, LWS_TO_KILL_SYNC);
    service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
  }
  conn->closed = 1;
  clear_messages(conn);
  clear_pending_send(conn);
  clear_rx(conn);
  if (conn->owns_context && conn->context != NULL) {
    unregister_context(conn->context);
    p_lws_context_destroy(conn->context);
    conn->context = NULL;
  }
  if (conn->error != NULL) free(conn->error);
  if (conn->protocol_name != NULL) free(conn->protocol_name);
  free(conn);
  return Strue;
}

ptr chezpp_net_websocket_send(uptr handle, int type, ptr bv, int start, int stop, int nonblocking,
                              int timeout_ms) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);
  size_t len;

  if (conn == NULL || conn->closed || conn->wsi == NULL)
    return make_error_status_message("invalid websocket connection");

  if (conn->pending_send != NULL) {
    if (nonblocking) {
      service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
      if (conn->pending_send != NULL) return make_status("would-block", Sfalse);
    } else if (!wait_for_condition(conn->context, pred_send_done, conn, timeout_ms, 0)) {
      return make_error_status_message("websocket send timed out");
    }
  }

  if (conn->failed) return connection_error_status(conn, "websocket send failed");

  len = (size_t)(stop - start);
  if (!queue_send(conn, type, Sbytevector_data(bv) + start, len))
    return make_status("error", errno_str());
  p_lws_callback_on_writable(conn->wsi);

  if (nonblocking) {
    service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
    if (conn->pending_send != NULL) return make_status("would-block", Sfalse);
  } else if (!wait_for_condition(conn->context, pred_send_done, conn, timeout_ms, 0)) {
    clear_pending_send(conn);
    return make_error_status_message("websocket send timed out");
  }

  if (conn->failed) return connection_error_status(conn, "websocket send failed");
  return Sfixnum((iptr)len);
}

ptr chezpp_net_websocket_recv(uptr handle, int nonblocking, int timeout_ms) {
  chezpp_ws_connection *conn = (chezpp_ws_connection *)TO_VOIDP(handle);

  if (conn == NULL || (conn->closed && conn->msg_head == NULL))
    return make_error_status_message("invalid websocket connection");

  if (conn->msg_head == NULL) {
    if (nonblocking) {
      service_context(conn->context, CHEZPP_WS_NONBLOCK_SERVICE_MS);
    } else if (!wait_for_condition(conn->context, pred_recv_ready, conn, timeout_ms, 0)) {
      return make_error_status_message("websocket receive timed out");
    }
  }

  if (conn->msg_head != NULL) return pop_message_scheme(conn);
  if (conn->failed) return connection_error_status(conn, "websocket receive failed");
  if (conn->closed) return Seof_object;
  return make_status("would-block", Sfalse);
}
