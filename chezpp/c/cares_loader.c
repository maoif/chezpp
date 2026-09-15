#include "cares_loader.h"

#include <ares.h>
#include <pthread.h>

static const char *const cares_names[] = {"libcares.so.2", NULL};
static chezpp_optional_library cares_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("cares", cares_names);
static pthread_once_t cares_once = PTHREAD_ONCE_INIT;
static int cares_available;

typedef const char *(*cares_version_fn)(int *version);
typedef int (*cares_library_init_fn)(int flags);

static const char *const required_symbols[] = {
    "ares_library_init", "ares_init_options", "ares_destroy", "ares_cancel",
    "ares_getaddrinfo", "ares_freeaddrinfo", "ares_getsock", "ares_timeout",
    "ares_process_fd", "ares_strerror", NULL};

static void initialize_cares(void) {
  cares_version_fn version_fn = NULL;
  cares_library_init_fn library_init_fn = NULL;
  const char *version_string;
  int version = 0;
  size_t index;

  if (!chezpp_optional_library_open(&cares_library)) return;
  if (!chezpp_optional_library_symbol(&cares_library, "ares_version",
                                      (void **)&version_fn))
    return;
  version_string = version_fn(&version);
  if (version_string == NULL) {
    chezpp_optional_library_fail(&cares_library, "c-ares returned no runtime version");
    return;
  }
  chezpp_optional_library_set_version(&cares_library, version_string);
  if (version < 0x011200) {
    chezpp_optional_library_fail(
        &cares_library, "c-ares runtime %s; requires SONAME 2 and version >= 1.18.0",
        version_string);
    return;
  }
  for (index = 0; required_symbols[index] != NULL; index++) {
    void *symbol = NULL;
    if (!chezpp_optional_library_symbol(&cares_library, required_symbols[index], &symbol))
      return;
  }
  if (!chezpp_optional_library_symbol(&cares_library, "ares_library_init",
                                      (void **)&library_init_fn))
    return;
  if (library_init_fn(ARES_LIB_INIT_ALL) != ARES_SUCCESS) {
    chezpp_optional_library_fail(&cares_library, "c-ares global initialization failed");
    return;
  }
  cares_available = 1;
}

int chezpp_cares_require(void) {
  pthread_once(&cares_once, initialize_cares);
  return cares_available;
}

const chezpp_optional_library *chezpp_cares_library(void) {
  (void)chezpp_cares_require();
  return &cares_library;
}

void *chezpp_cares_symbol(const char *name) {
  void *symbol = NULL;
  if (name == NULL || !chezpp_cares_require()) return NULL;
  if (!chezpp_optional_library_symbol(&cares_library, name, &symbol)) return NULL;
  return symbol;
}
