#include "unavailable.h"

ptr chezpp_net_websocket_listen(const char *host, int port, const char *protocol_name,
                                const char *offered_protocols,
                                uptr tls_context_handle) {
  (void)host;
  (void)port;
  (void)protocol_name;
  (void)offered_protocols;
  (void)tls_context_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_server_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_accept(uptr handle, int nonblocking, int timeout_ms) {
  (void)handle;
  (void)nonblocking;
  (void)timeout_ms;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_connect(const char *host, int port, const char *path,
                                 const char *protocol_name, const char *offered_protocols,
                                 int secure, uptr tls_context_handle,
                                 int timeout_ms) {
  (void)host;
  (void)port;
  (void)path;
  (void)protocol_name;
  (void)offered_protocols;
  (void)secure;
  (void)tls_context_handle;
  (void)timeout_ms;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_connect_step(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_close_with_reason(uptr handle, int code, ptr reason) {
  (void)handle;
  (void)code;
  (void)reason;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_state(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_cancel_send(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_send(uptr handle, int type, ptr bv, int start, int stop,
                              int nonblocking, int timeout_ms) {
  (void)handle;
  (void)type;
  (void)bv;
  (void)start;
  (void)stop;
  (void)nonblocking;
  (void)timeout_ms;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_send_fragment(uptr handle, int type, ptr bv, int start, int stop,
                                       int final, int nonblocking, int timeout_ms) {
  (void)handle;
  (void)type;
  (void)bv;
  (void)start;
  (void)stop;
  (void)final;
  (void)nonblocking;
  (void)timeout_ms;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_recv(uptr handle, int nonblocking, int timeout_ms) {
  (void)handle;
  (void)nonblocking;
  (void)timeout_ms;
  return chezpp_unavailable_status("websockets: disabled at build time");
}

ptr chezpp_net_websocket_poll_targets(uptr handle, int server_handle) {
  (void)handle;
  (void)server_handle;
  return chezpp_unavailable_status("websockets: disabled at build time");
}
