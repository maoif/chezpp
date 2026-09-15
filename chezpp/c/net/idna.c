#include "../common.h"
#include "../idn2_loader.h"

#include <idn2.h>
#include <string.h>

typedef int (*idn2_lookup_u8_fn)(const uint8_t *, uint8_t **, int);
typedef int (*idn2_to_unicode_8z8z_fn)(const char *, char **, int);
typedef const char *(*idn2_strerror_fn)(int);
typedef void (*idn2_free_fn)(void *);

static ptr make_status(const char *tag, ptr value) {
  ptr out = Smake_vector(2, Sfalse);
  Svector_set(out, 0, Sstring_to_symbol(tag));
  Svector_set(out, 1, value);
  return out;
}

ptr chezpp_net_idna_to_ascii(const char *domain) {
  idn2_lookup_u8_fn lookup =
      (idn2_lookup_u8_fn)chezpp_idn2_symbol("idn2_lookup_u8");
  idn2_strerror_fn strerror_fn =
      (idn2_strerror_fn)chezpp_idn2_symbol("idn2_strerror");
  idn2_free_fn free_fn = (idn2_free_fn)chezpp_idn2_symbol("idn2_free");
  uint8_t *output = NULL;
  int status;
  ptr result;
  if (lookup == NULL || strerror_fn == NULL || free_fn == NULL)
    return make_status("error", Sstring("libidn2 support is unavailable"));
  status = lookup((const uint8_t *)domain, &output,
                  IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL |
                      IDN2_USE_STD3_ASCII_RULES);
  if (status != IDN2_OK) return make_status("error", Sstring(strerror_fn(status)));
  result = Sstring((const char *)output);
  free_fn(output);
  return result;
}

ptr chezpp_net_idna_to_unicode(const char *domain) {
  idn2_to_unicode_8z8z_fn decode =
      (idn2_to_unicode_8z8z_fn)chezpp_idn2_symbol("idn2_to_unicode_8z8z");
  idn2_strerror_fn strerror_fn =
      (idn2_strerror_fn)chezpp_idn2_symbol("idn2_strerror");
  idn2_free_fn free_fn = (idn2_free_fn)chezpp_idn2_symbol("idn2_free");
  char *output = NULL;
  int status;
  ptr result;
  if (decode == NULL || strerror_fn == NULL || free_fn == NULL)
    return make_status("error", Sstring("libidn2 support is unavailable"));
  status = decode(domain, &output, IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL);
  if (status != IDN2_OK) return make_status("error", Sstring(strerror_fn(status)));
  result = Sstring_utf8(output, (iptr)strlen(output));
  free_fn(output);
  return result;
}
