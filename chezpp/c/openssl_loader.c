#include "openssl_loader.h"
#include <pthread.h>
static chezpp_optional_library openssl_library = CHEZPP_OPTIONAL_LIBRARY_INIT("openssl");
#if CHEZPP_WITH_OPENSSL
static pthread_once_t openssl_once = PTHREAD_ONCE_INIT;
static void initialize_openssl(void) {
  unsigned major = (unsigned)((OpenSSL_version_num() >> 28) & 0xfUL);
  chezpp_optional_library_set_version(&openssl_library, OpenSSL_version(OPENSSL_VERSION));
  if (major != 3)
    chezpp_optional_library_fail(&openssl_library,
                                "OpenSSL runtime ABI major %u; requires major 3", major);
  else if (OPENSSL_init_crypto(0, NULL) != 1 || OPENSSL_init_ssl(0, NULL) != 1)
    chezpp_optional_library_fail(&openssl_library, "OpenSSL runtime initialization failed");
}
#endif
int chezpp_openssl_require(void) {
#if CHEZPP_WITH_OPENSSL
  pthread_once(&openssl_once, initialize_openssl);
  return openssl_library.state == 1;
#else
  return 0;
#endif
}
const chezpp_optional_library *chezpp_openssl_library(void) {
  (void)chezpp_openssl_require();
  return &openssl_library;
}
#if !CHEZPP_WITH_OPENSSL
#include "common.h"
ptr crypto_openssl_load_error(void) { return Sstring("openssl: disabled at build time"); }
#endif
