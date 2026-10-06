#include "unavailable.h"

uptr chezpp_net_dns_start(const char *name, int family, int timeout_ms) {
  (void)name;
  (void)family;
  (void)timeout_ms;
  return 0;
}

ptr chezpp_net_dns_advance(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("cares: disabled at build time");
}

ptr chezpp_net_dns_cancel(uptr handle) {
  (void)handle;
  return chezpp_unavailable_status("cares: disabled at build time");
}

void chezpp_net_dns_close(uptr handle) {
  (void)handle;
}
