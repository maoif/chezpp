#include "idn2_loader.h"
#if CSPP_WITH_IDN2
#include <idn2.h>
static pthread_once_t once = PTHREAD_ONCE_INIT;
static cspp_optional_library library = CSPP_OPTIONAL_LIBRARY_INIT("idn2");
static void initialize(void) {

  const char *version = idn2_check_version(NULL);
  cspp_optional_library_set_version(&library, version);
  if (version == NULL || idn2_check_version("2.3.0") == NULL)
    cspp_optional_library_fail(&library, "libidn2 requires runtime version >= 2.3.0");

}
int cspp_idn2_require(void) { pthread_once(&once, initialize); return library.state == 1; }
const cspp_optional_library *cspp_idn2_library(void) {
  (void)cspp_idn2_require(); return &library;
}
#endif
