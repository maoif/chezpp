#include "unavailable.h"

uintptr_t chezpp_lws_http_context_open(size_t event_capacity,
                                       size_t payload_capacity,
                                       uintptr_t tls_context_handle,
                                       const char *proxy_address,
                                       int proxy_port) {
  (void)event_capacity;
  (void)payload_capacity;
  (void)tls_context_handle;
  (void)proxy_address;
  (void)proxy_port;
  return 0;
}

uintptr_t chezpp_lws_http_server_context_open(size_t event_capacity,
                                              size_t payload_capacity,
                                              const char *interface_name,
                                              int port,
                                              uintptr_t tls_context_handle) {
  (void)event_capacity;
  (void)payload_capacity;
  (void)interface_name;
  (void)port;
  (void)tls_context_handle;
  return 0;
}

void chezpp_lws_http_context_close(uintptr_t context_handle) {
  (void)context_handle;
}

int chezpp_lws_http_context_wakeup_fd(uintptr_t context_handle) {
  (void)context_handle;
  return -1;
}

ptr chezpp_lws_http_context_poll_snapshot(uintptr_t context_handle) {
  (void)context_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

int chezpp_lws_http_context_service_fd(uintptr_t context_handle, int fd,
                                       int revents) {
  (void)context_handle;
  (void)fd;
  (void)revents;
  return -1;
}

ptr chezpp_lws_http_context_next_event(uintptr_t context_handle) {
  (void)context_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

int chezpp_lws_http_context_timeout_ms(uintptr_t context_handle,
                                       int maximum_timeout_ms) {
  (void)context_handle;
  (void)maximum_timeout_ms;
  return -1;
}

int chezpp_lws_http_context_wakeup(uintptr_t context_handle) {
  (void)context_handle;
  return -1;
}

ptr chezpp_lws_http_context_pool_metrics(uintptr_t context_handle) {
  (void)context_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

uintptr_t chezpp_lws_http_signal_open(uintptr_t context_handle) {
  (void)context_handle;
  return 0;
}

int chezpp_lws_http_signal_fd(uintptr_t signal_handle) {
  (void)signal_handle;
  return -1;
}

int chezpp_lws_http_signal_notify(uintptr_t signal_handle) {
  (void)signal_handle;
  return -1;
}

void chezpp_lws_http_signal_drain(uintptr_t signal_handle) {
  (void)signal_handle;
}

void chezpp_lws_http_signal_close(uintptr_t signal_handle) {
  (void)signal_handle;
}

int chezpp_lws_http_client_start(uintptr_t context_handle,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, const char *address,
                                 int port, int tls, const char *method,
                                 const char *host, const char *path,
                                 ptr headers, ptr initial_body, int has_body,
                                 const char *alpn) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)address;
  (void)port;
  (void)tls;
  (void)method;
  (void)host;
  (void)path;
  (void)headers;
  (void)initial_body;
  (void)has_body;
  (void)alpn;
  return -1;
}

int chezpp_lws_http_client_acquire(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  return -1;
}

int chezpp_lws_http_client_release(uintptr_t context_handle,
                                   uint64_t connection_id, uint64_t stream_id,
                                   uint64_t generation) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  return -1;
}

int chezpp_lws_http_client_body_submit(uintptr_t context_handle,
                                       uint64_t connection_id,
                                       uint64_t stream_id, uint64_t generation,
                                       ptr payload, int final_chunk) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)payload;
  (void)final_chunk;
  return -1;
}

int chezpp_lws_http_client_body_drain(uintptr_t context_handle,
                                      uint64_t connection_id,
                                      uint64_t stream_id,
                                      uint64_t generation) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  return -1;
}

ptr chezpp_lws_http_server_request_dequeue(uintptr_t context_handle) {
  (void)context_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

int chezpp_lws_http_server_response_submit(uintptr_t context_handle, uint64_t connection_id, uint64_t stream_id,
    uint64_t generation, int status, ptr headers, ptr payload, int final_chunk) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)status;
  (void)headers;
  (void)payload;
  (void)final_chunk;
  return -1;
}

int chezpp_lws_http_stream_cancel(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, int status) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)status;
  return -1;
}

int chezpp_lws_http_body_consumed(uintptr_t context_handle,
                                  uint64_t connection_id, uint64_t stream_id,
                                  uint64_t generation, size_t byte_count) {
  (void)context_handle;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)byte_count;
  return -1;
}

int chezpp_lws_http_inject_event(uintptr_t context_handle, int tag,
                                 uint64_t connection_id, uint64_t stream_id,
                                 uint64_t generation, int status, ptr payload,
                                 int protocol, int reusable,
                                 uint32_t peer_h2_capacity,
                                 int peer_h2_capacity_known,
                                 int terminal_scope) {
  (void)context_handle;
  (void)tag;
  (void)connection_id;
  (void)stream_id;
  (void)generation;
  (void)status;
  (void)payload;
  (void)protocol;
  (void)reusable;
  (void)peer_h2_capacity;
  (void)peer_h2_capacity_known;
  (void)terminal_scope;
  return -1;
}

int chezpp_lws_http_inject_poll(uintptr_t context_handle, int operation, int fd,
                                int events) {
  (void)context_handle;
  (void)operation;
  (void)fd;
  (void)events;
  return -1;
}
