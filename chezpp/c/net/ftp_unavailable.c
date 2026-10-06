#include "unavailable.h"

ptr chezpp_net_ftp_list(const char *url, const char *user, const char *pass, int passive,
                        int timeout_ms, int use_tls, int verify_peer, int verify_host) {
  (void)url;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_stat(const char *url, const char *user, const char *pass, int passive,
                        int timeout_ms, int use_tls, int verify_peer, int verify_host,
                        const char *path) {
  (void)url;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  (void)path;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_download(const char *url, const char *dest, const char *user, const char *pass,
                            int passive, int timeout_ms, int use_tls, int verify_peer,
                            int verify_host) {
  (void)url;
  (void)dest;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_upload(const char *url, const char *src, const char *user, const char *pass,
                          int passive, int timeout_ms, int use_tls, int verify_peer,
                          int verify_host) {
  (void)url;
  (void)src;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_command(const char *url, const char *user, const char *pass, int passive,
                           int timeout_ms, int use_tls, int verify_peer, int verify_host,
                           const char *cmd) {
  (void)url;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  (void)cmd;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_rename(const char *url, const char *user, const char *pass, int passive,
                          int timeout_ms, int use_tls, int verify_peer, int verify_host,
                          const char *from_path, const char *to_path) {
  (void)url;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  (void)from_path;
  (void)to_path;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_transfer_start(int kind, const char *url, const char *path,
                                  const char *user, const char *pass, int passive,
                                  int timeout_ms, int use_tls, int verify_peer,
                                  int verify_host) {
  (void)kind;
  (void)url;
  (void)path;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_transfer_step(uptr handle, ptr ready, int timer_expired) {
  (void)handle;
  (void)ready;
  (void)timer_expired;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_transfer_cancel(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("curl: disabled at build time");
}

void chezpp_net_ftp_transfer_close(uptr handle) {
  (void)handle;
}

ptr chezpp_net_ftp_session_open(void) {
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_session_close(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_open(uptr session_handle, int direction,
                             const char *url, const char *user, const char *pass,
                             int passive, int timeout_ms, int use_tls,
                             int verify_peer, int verify_host, iptr offset) {
  (void)session_handle;
  (void)direction;
  (void)url;
  (void)user;
  (void)pass;
  (void)passive;
  (void)timeout_ms;
  (void)use_tls;
  (void)verify_peer;
  (void)verify_host;
  (void)offset;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_step(uptr handle, ptr ready, int timer_expired) {
  (void)handle;
  (void)ready;
  (void)timer_expired;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_read(uptr handle, int count) {
  (void)handle;
  (void)count;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_read_into(uptr handle, ptr bytevector, int length,
                                  int start, int count) {
  (void)handle;
  (void)bytevector;
  (void)length;
  (void)start;
  (void)count;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_write(uptr handle, ptr bytevector, int length,
                              int start, int count) {
  (void)handle;
  (void)bytevector;
  (void)length;
  (void)start;
  (void)count;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_finish(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("curl: disabled at build time");
}

ptr chezpp_net_ftp_file_cancel(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("curl: disabled at build time");
}

void chezpp_net_ftp_file_close(uptr handle) {
  (void)handle;
}
