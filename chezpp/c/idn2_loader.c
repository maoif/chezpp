#include "idn2_loader.h"

#include <idn2.h>
#include <pthread.h>

static const char *const idn2_names[] = {"libidn2.so.0", NULL};
static chezpp_optional_library idn2_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("idn2", idn2_names);
static pthread_once_t idn2_once = PTHREAD_ONCE_INIT;
static int idn2_available;

typedef const char *(*idn2_check_version_fn)(const char *required_version);

static const char *const required_symbols[] = {
    "idn2_lookup_u8", "idn2_to_unicode_8z8z", "idn2_strerror", "idn2_free", NULL};

static void initialize_idn2(void) {
  idn2_check_version_fn check_version = NULL;
  const char *version;
  size_t index;
  if (!chezpp_optional_library_open(&idn2_library)) return;
  if (!chezpp_optional_library_symbol(&idn2_library, "idn2_check_version",
                                      (void **)&check_version))
    return;
  version = check_version("2.3.0");
  if (version == NULL) {
    chezpp_optional_library_fail(&idn2_library,
                                 "libidn2 requires runtime version >= 2.3.0");
    return;
  }
  chezpp_optional_library_set_version(&idn2_library, version);
  for (index = 0; required_symbols[index] != NULL; index++) {
    void *symbol = NULL;
    if (!chezpp_optional_library_symbol(&idn2_library, required_symbols[index], &symbol))
      return;
  }
  idn2_available = 1;
}

int chezpp_idn2_require(void) {
  pthread_once(&idn2_once, initialize_idn2);
  return idn2_available;
}

const chezpp_optional_library *chezpp_idn2_library(void) {
  (void)chezpp_idn2_require();
  return &idn2_library;
}

void *chezpp_idn2_symbol(const char *name) {
  void *symbol = NULL;
  if (name == NULL || !chezpp_idn2_require()) return NULL;
  if (!chezpp_optional_library_symbol(&idn2_library, name, &symbol)) return NULL;
  return symbol;
}
