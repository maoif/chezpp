#include "lws_loader.h"
#include "../common.h"
#include "../build-config.h"
#if CHEZPP_WITH_WEBSOCKETS
#include <libwebsockets.h>
#endif
int chezpp_lws_available(void) {
#if CHEZPP_WITH_WEBSOCKETS
  unsigned major, minor, patch;
  const char *version = lws_get_library_version();
  return version != NULL && sscanf(version, "%u.%u.%u", &major, &minor, &patch) == 3 &&
         (major > 4 || (major == 4 && minor >= 3));
#else
  return 0;
#endif
}
unsigned chezpp_lws_capabilities(void) {
  unsigned capabilities = 0;
#if CHEZPP_WITH_WEBSOCKETS
  if (!chezpp_lws_available()) return 0;
  capabilities = CHEZPP_LWS_CAP_HTTP1 | CHEZPP_LWS_CAP_EXTERNAL_POLL;
#if defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2)
  capabilities |= CHEZPP_LWS_CAP_HTTP2;
#endif
#if defined(LWS_WITH_TLS)
  capabilities |= CHEZPP_LWS_CAP_TLS;
#endif
#if defined(LWS_WITH_SOCKS5)
  capabilities |= CHEZPP_LWS_CAP_SOCKS5;
#endif
#endif
  return capabilities;
}
const char *chezpp_lws_error(void) {
#if CHEZPP_WITH_WEBSOCKETS
  return chezpp_lws_available() ? "" : "libwebsockets HTTP requires version >= 4.3.0";
#else
  return "websockets: disabled at build time";
#endif
}
const char *chezpp_lws_version(void) {
#if CHEZPP_WITH_WEBSOCKETS
  return lws_get_library_version();
#else
  return NULL;
#endif
}

ptr chezpp_lws_status(void) {
  ptr result = Smake_vector(4, Sfalse);
  int available = chezpp_lws_available();
  const char *version = chezpp_lws_version();
  const char *error = chezpp_lws_error();

  Svector_set(result, 0, available ? Strue : Sfalse);
  Svector_set(result, 1, Sfixnum((iptr)chezpp_lws_capabilities()));
  Svector_set(result, 2,
              version == NULL || version[0] == '\0' ? Sfalse
                                                      : Sstring(version));
  Svector_set(result, 3,
              error == NULL || error[0] == '\0' ? Sfalse : Sstring(error));
  return result;
}
