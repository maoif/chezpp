#include "unavailable.h"

ptr chezpp_net_grpc_channel_open(const char *target) {
  (void)target;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_channel_open_tls(const char *target, const char *root_certs,
                                     const char *certificate_chain, const char *private_key) {
  (void)target;
  (void)root_certs;
  (void)certificate_chain;
  (void)private_key;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_channel_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_open(const char *host, int port) {
  (void)host;
  (void)port;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_open_tls(const char *host, int port, const char *root_certs,
                                    const char *certificate_chain, const char *private_key) {
  (void)host;
  (void)port;
  (void)root_certs;
  (void)certificate_chain;
  (void)private_key;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_unary_call(uptr handle, const char *method, ptr payload, int start, int stop,
                               ptr metadata, int timeout_ms) {
  (void)handle;
  (void)method;
  (void)payload;
  (void)start;
  (void)stop;
  (void)metadata;
  (void)timeout_ms;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_unary_start(uptr handle, const char *method, ptr payload, int start, int stop,
                                ptr metadata, int timeout_ms) {
  (void)handle;
  (void)method;
  (void)payload;
  (void)start;
  (void)stop;
  (void)metadata;
  (void)timeout_ms;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_unary_poll(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_unary_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_open(uptr handle, const char *method, int shape, ptr payload, int start,
                                int stop, ptr metadata_ls, int timeout_ms) {
  (void)handle;
  (void)method;
  (void)shape;
  (void)payload;
  (void)start;
  (void)stop;
  (void)metadata_ls;
  (void)timeout_ms;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_open_start(uptr handle, const char *method, int shape, ptr payload,
                                      int start, int stop, ptr metadata_ls, int timeout_ms) {
  (void)handle;
  (void)method;
  (void)shape;
  (void)payload;
  (void)start;
  (void)stop;
  (void)metadata_ls;
  (void)timeout_ms;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_open_poll(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_send(uptr handle, ptr payload, int start, int stop) {
  (void)handle;
  (void)payload;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_recv(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_close_send(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_finish(uptr handle, ptr payload, int start, int stop, int status_code,
                                  const char *status_message, ptr metadata_ls) {
  (void)handle;
  (void)payload;
  (void)start;
  (void)stop;
  (void)status_code;
  (void)status_message;
  (void)metadata_ls;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_stream_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_request(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_request_stream(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

ptr chezpp_net_grpc_server_respond(uptr handle, ptr payload, int start, int stop, int status_code,
                                   const char *status_message, ptr metadata_ls) {
  (void)handle;
  (void)payload;
  (void)start;
  (void)stop;
  (void)status_code;
  (void)status_message;
  (void)metadata_ls;
  return chezpp_unavailable_status("grpc: disabled at build time");
}

unsigned chezpp_net_grpc_capabilities(void) {
  return 0;
}

int chezpp_net_grpc_driver_fd(void) {
  return -1;
}

ptr chezpp_net_grpc_driver_drain(void) {
  return chezpp_unavailable_status("grpc: disabled at build time");
}
