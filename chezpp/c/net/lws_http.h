#ifndef CHEZPP_LWS_HTTP_H
#define CHEZPP_LWS_HTTP_H

#include <libwebsockets.h>
#include <pthread.h>
#include <scheme.h>
#include <stddef.h>
#include <stdint.h>

typedef enum lws_http_event_tag {
  LWS_HTTP_EVENT_POLL_ADD = 1,
  LWS_HTTP_EVENT_POLL_CHANGE,
  LWS_HTTP_EVENT_POLL_DELETE,
  LWS_HTTP_EVENT_CONNECTED,
  LWS_HTTP_EVENT_HEADERS,
  LWS_HTTP_EVENT_READABLE,
  LWS_HTTP_EVENT_WRITABLE,
  LWS_HTTP_EVENT_COMPLETE,
  LWS_HTTP_EVENT_CLOSED,
  LWS_HTTP_EVENT_FAILED,
  LWS_HTTP_EVENT_RESET,
  LWS_HTTP_EVENT_GOAWAY
} lws_http_event_tag;

typedef enum lws_http_protocol {
  LWS_HTTP_PROTOCOL_UNKNOWN = 0,
  LWS_HTTP_PROTOCOL_HTTP1,
  LWS_HTTP_PROTOCOL_HTTP2
} lws_http_protocol;

typedef enum lws_http_terminal_scope {
  LWS_HTTP_TERMINAL_SCOPE_NONE = 0,
  LWS_HTTP_TERMINAL_SCOPE_STREAM,
  LWS_HTTP_TERMINAL_SCOPE_CONNECTION
} lws_http_terminal_scope;

typedef struct lws_http_event lws_http_event;
typedef struct lws_http_connection lws_http_connection;
typedef struct lws_http_stream lws_http_stream;
typedef struct lws_poll_entry lws_poll_entry;
typedef struct lws_http_signal lws_http_signal;
typedef struct lws_http_context lws_http_context;

#define CHEZPP_LWS_HTTP_STATUS_H2_READY (-2000)

struct lws_http_event {
  lws_http_event *next;
  lws_http_event_tag tag;
  uint64_t context_id;
  uint64_t connection_id;
  uint64_t stream_id;
  uint64_t generation;
  int status;
  lws_http_protocol protocol;
  int reusable;
  uint32_t peer_h2_capacity;
  int peer_h2_capacity_known;
  lws_http_terminal_scope terminal_scope;
  size_t payload_length;
  unsigned char *payload;
};

struct lws_http_connection {
  lws_http_connection *next_free;
  uint64_t identity;
  uint64_t generation;
  struct lws *wsi;
  unsigned active_streams;
  int active;
  int terminal;
};

struct lws_http_stream {
  lws_http_stream *next_free;
  lws_http_connection *connection;
  uint64_t connection_identity;
  uint64_t identity;
  uint64_t generation;
  struct lws *wsi;
  unsigned char *outbound;
  unsigned char *headers;
  unsigned char *terminal_payload;
  size_t outbound_length;
  size_t submitted_body_length;
  size_t headers_length;
  size_t terminal_payload_length;
  size_t pending_body_bytes;
  int outbound_final;
  int has_request_body;
  int active;
  int terminal;
  int terminal_pending;
  lws_http_event_tag terminal_tag;
  int terminal_status;
  lws_http_protocol terminal_protocol;
  int terminal_reusable;
  uint32_t terminal_peer_h2_capacity;
  int terminal_peer_h2_capacity_known;
  lws_http_terminal_scope terminal_scope;
  int failure_pending;
  int failure_status;
  int server_stream;
  int server_request_complete;
  size_t server_body_remaining;
  int response_status;
  int response_headers_sent;
  int h2;
  int h2_ready;
  lws_http_protocol observed_protocol;
  int reusable;
  uint32_t peer_h2_capacity;
  int peer_h2_capacity_known;
  char address[256];
  char host[256];
  char path[1024];
  char method[16];
};

struct lws_http_signal {
  lws_http_signal *next_free;
  lws_http_context *context;
  int pipe[2];
  int active;
};

struct lws_poll_entry {
  lws_poll_entry *next_free;
  int fd;
  short events;
  int active;
};

struct lws_http_context {
  uint64_t identity;
  struct lws_context *lws;
  struct lws_vhost *vhosts;
  struct lws_protocols protocols[2];
  int wakeup_pipe[2];
  pthread_mutex_t lock;

  lws_http_event *events;
  lws_http_event *event_free;
  lws_http_event *event_head;
  lws_http_event *event_tail;
  unsigned char *event_payloads;
  size_t event_capacity;
  size_t event_in_use;
  size_t event_high_water;
  size_t event_misses;
  size_t event_exhaustions;
  size_t payload_capacity;

  lws_poll_entry *poll_entries;
  lws_poll_entry *poll_free;
  size_t poll_capacity;
  size_t poll_in_use;
  size_t poll_high_water;

  lws_http_connection *connections;
  lws_http_connection *connection_free;
  size_t connection_capacity;
  size_t connection_in_use;
  size_t connection_high_water;

  lws_http_stream *streams;
  lws_http_stream *stream_free;
  unsigned char *stream_payloads;
  unsigned char *stream_headers;
  unsigned char *stream_terminal_payloads;
  size_t stream_capacity;
  size_t stream_in_use;
  size_t stream_high_water;

  lws_http_signal *signals;
  lws_http_signal *signal_free;
  size_t signal_capacity;
  size_t signal_in_use;
  size_t signal_high_water;
  size_t signal_misses;

  unsigned char *drain_buffer;
  size_t queued_body_bytes;
  size_t body_byte_limit;
  size_t live_handle_count;
  int tls_verify_peer;
  uintptr_t server_tls_context_handle;
  int initializing;
  int closing;
};

uintptr_t chezpp_lws_http_context_open(size_t event_capacity,
                                       size_t payload_capacity,
                                       uintptr_t tls_context_handle,
                                       const char *proxy_address,
                                       int proxy_port);
uintptr_t chezpp_lws_http_server_context_open(size_t event_capacity,
                                              size_t payload_capacity,
                                              const char *interface_name,
                                              int port,
                                              uintptr_t tls_context_handle);
void chezpp_lws_http_context_close(uintptr_t context_handle);
int chezpp_lws_http_context_wakeup_fd(uintptr_t context_handle);
ptr chezpp_lws_http_context_poll_snapshot(uintptr_t context_handle);
int chezpp_lws_http_context_service_fd(uintptr_t context_handle, int fd,
                                       int revents);
ptr chezpp_lws_http_context_next_event(uintptr_t context_handle);
int chezpp_lws_http_context_timeout_ms(uintptr_t context_handle,
                                       int maximum_timeout_ms);
int chezpp_lws_http_context_wakeup(uintptr_t context_handle);
ptr chezpp_lws_http_context_pool_metrics(uintptr_t context_handle);
uintptr_t chezpp_lws_http_signal_open(uintptr_t context_handle);
int chezpp_lws_http_signal_fd(uintptr_t signal_handle);
int chezpp_lws_http_signal_notify(uintptr_t signal_handle);
void chezpp_lws_http_signal_drain(uintptr_t signal_handle);
void chezpp_lws_http_signal_close(uintptr_t signal_handle);

int chezpp_lws_http_client_start(uintptr_t context_handle,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, const char *address,
                                 int port, int tls, const char *method,
                                 const char *host, const char *path,
                                 ptr headers, ptr initial_body, int has_body,
                                 const char *alpn);
int chezpp_lws_http_client_acquire(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation);
int chezpp_lws_http_client_release(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation);
int chezpp_lws_http_client_body_submit(uintptr_t context_handle,
                                       uint64_t connection_id,
                                       uint64_t stream_id, uint64_t generation,
                                       ptr payload, int final_chunk);
int chezpp_lws_http_client_body_drain(uintptr_t context_handle,
                                      uint64_t connection_id,
                                      uint64_t stream_id, uint64_t generation);
ptr chezpp_lws_http_server_request_dequeue(uintptr_t context_handle);
int chezpp_lws_http_server_response_submit(
    uintptr_t context_handle, uint64_t connection_id, uint64_t stream_id,
    uint64_t generation, int status, ptr headers, ptr payload, int final_chunk);
int chezpp_lws_http_stream_cancel(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, int status);
int chezpp_lws_http_body_consumed(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, size_t byte_count);

int chezpp_lws_http_inject_event(uintptr_t context_handle, int tag,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, int status, ptr payload,
                                 int protocol, int reusable,
                                 uint32_t peer_h2_capacity,
                                 int peer_h2_capacity_known,
                                 int terminal_scope);
int chezpp_lws_http_inject_poll(uintptr_t context_handle, int operation, int fd,
                                int events);

#endif
