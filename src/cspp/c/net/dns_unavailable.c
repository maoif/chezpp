#include "unavailable.h"

uptr cspp_net_dns_start(const char *name, int family, int timeout_ms) {
  (void)name;
  (void)family;
  (void)timeout_ms;
  return 0;
}

ptr cspp_net_dns_advance(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("cares: disabled at build time");
}

ptr cspp_net_dns_cancel(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("cares: disabled at build time");
}

void cspp_net_dns_close(uptr handle) {
  (void)handle;
}
