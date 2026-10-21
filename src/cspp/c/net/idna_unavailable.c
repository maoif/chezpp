#include "unavailable.h"

ptr cspp_net_idna_to_ascii(const char *domain) {
  (void)domain;
  return cspp_unavailable_status("idn2: disabled at build time");
}

ptr cspp_net_idna_to_unicode(const char *domain) {
  (void)domain;
  return cspp_unavailable_status("idn2: disabled at build time");
}
