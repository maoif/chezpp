#include "unavailable.h"

uptr chezpp_net_tls_context_create(int mode) {
  (void)mode;
  return 0;
}

void chezpp_net_tls_context_free(uptr handle) {
  (void)handle;
}

ptr chezpp_net_tls_context_load_ca_file(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_ca_path(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_default_ca(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_cert_file(uptr handle, const char *path, int format) {
  (void)handle;
  (void)path;
  (void)format;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_cert_bytes(uptr handle, ptr bv, int start, int stop, int format) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)format;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_key_file(uptr handle, const char *path, int format) {
  (void)handle;
  (void)path;
  (void)format;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_load_key_bytes(uptr handle, ptr bv, int start, int stop, int format) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)format;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_check_key(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_set_verify(uptr handle, int verify_mode) {
  (void)handle;
  (void)verify_mode;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_set_alpn(uptr handle, ptr bv, int start, int stop) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_set_policy(uptr handle, int minimum_version,
                                      int maximum_version,
                                      const char *cipher_list,
                                      const char *ciphersuites,
                                      int ocsp_policy) {
  (void)handle;
  (void)minimum_version;
  (void)maximum_version;
  (void)cipher_list;
  (void)ciphersuites;
  (void)ocsp_policy;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_import_session(uptr handle, ptr bv, int start,
                                          int stop) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_context_enable_sni(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_connect(uptr handle, int fd, const char *server_name, int timeout_ms) {
  (void)handle;
  (void)fd;
  (void)server_name;
  (void)timeout_ms;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_accept(uptr handle, int fd, int timeout_ms) {
  (void)handle;
  (void)fd;
  (void)timeout_ms;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_handshake_step(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_read(uptr handle, int size, int timeout_ms, int nonblocking) {
  (void)handle;
  (void)size;
  (void)timeout_ms;
  (void)nonblocking;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_read_into(uptr handle, ptr bv, int start, int stop, int timeout_ms,
                             int nonblocking) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)timeout_ms;
  (void)nonblocking;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_write(uptr handle, ptr bv, int start, int stop, int timeout_ms,
                         int nonblocking) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)timeout_ms;
  (void)nonblocking;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_shutdown(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_protocol_version(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_negotiated_alpn(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_cipher_name(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_verified(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_session_export(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_session_reused(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_session_select_context(uptr session_handle,
                                          uptr context_handle) {
  (void)session_handle;
  (void)context_handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_stapled_ocsp(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_ocsp_result(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_peer_certificate_der(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_peer_certificate_chain_der(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr chezpp_net_tls_load_error(void) { return Sstring("openssl: disabled at build time"); }
void *chezpp_net_tls_context_native(uptr handle) { (void)handle; return NULL; }
int chezpp_net_tls_context_copy_credentials(uptr handle, void *destination) {
  (void)handle; (void)destination; return 0;
}

int chezpp_net_tls_context_verifies_peer(uptr handle) { (void)handle; return 0; }
