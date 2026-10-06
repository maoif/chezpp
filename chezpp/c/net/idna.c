#include "../build-config.h"
#if CHEZPP_WITH_IDN2
#include <idn2.h>
#endif
#include "../common.h"
#include "../idn2_loader.h"

#include <string.h>



static ptr make_status(const char *tag, ptr value) {
  ptr out = Smake_vector(2, Sfalse);
  Svector_set(out, 0, Sstring_to_symbol(tag));
  Svector_set(out, 1, value);
  return out;
}

ptr chezpp_net_idna_to_ascii(const char *domain) {
  uint8_t *output = NULL;
  int status;
  ptr result;
  if (!chezpp_idn2_require())
    return make_status("error", Sstring(chezpp_idn2_library()->error));
  status = idn2_lookup_u8((const uint8_t *)domain, &output,
                  IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL |
                      IDN2_USE_STD3_ASCII_RULES);
  if (status != IDN2_OK) return make_status("error", Sstring(idn2_strerror(status)));
  result = Sstring((const char *)output);
  idn2_free(output);
  return result;
}

ptr chezpp_net_idna_to_unicode(const char *domain) {
  char *output = NULL;
  int status;
  ptr result;
  if (!chezpp_idn2_require())
    return make_status("error", Sstring(chezpp_idn2_library()->error));
  status = idn2_to_unicode_8z8z(domain, &output, IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL);
  if (status != IDN2_OK) return make_status("error", Sstring(idn2_strerror(status)));
  result = Sstring_utf8(output, (iptr)strlen(output));
  idn2_free(output);
  return result;
}
