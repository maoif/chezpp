#include "cares_loader.h"
#if CHEZPP_WITH_CARES
#include <ares.h>
static pthread_once_t once = PTHREAD_ONCE_INIT;
static chezpp_optional_library library = CHEZPP_OPTIONAL_LIBRARY_INIT("cares");
static void initialize(void) {
  int number = 0;
  const char *version = ares_version(&number);
  chezpp_optional_library_set_version(&library, version);
  if (version == NULL) {
    chezpp_optional_library_fail(&library, "c-ares returned no runtime version");
    return;
  }
  if (number < 0x011200) {
    chezpp_optional_library_fail(&library,
      "c-ares runtime %s; requires SONAME 2 and version >= 1.18.0", version);
    return;
  }
  if (ares_library_init(ARES_LIB_INIT_ALL) != ARES_SUCCESS) chezpp_optional_library_fail(&library, "c-ares global initialization failed");
}
int chezpp_cares_require(void) { pthread_once(&once, initialize); return library.state == 1; }
const chezpp_optional_library *chezpp_cares_library(void) {
  (void)chezpp_cares_require(); return &library;
}
#endif
