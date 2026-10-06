#include "unavailable.h"

ptr chezpp_net_idna_to_ascii(const char *domain) {
  (void)domain;
  return chezpp_unavailable_status("idn2: disabled at build time");
}

ptr chezpp_net_idna_to_unicode(const char *domain) {
  (void)domain;
  return chezpp_unavailable_status("idn2: disabled at build time");
}
