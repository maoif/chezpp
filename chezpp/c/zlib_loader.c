#include "zlib_loader.h"

#include <pthread.h>
#include <stdio.h>
#include <zlib.h>

static const char *const zlib_names[] = {"libz.so.1", NULL};
static chezpp_optional_library zlib_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("zlib", zlib_names);
static pthread_once_t zlib_once = PTHREAD_ONCE_INIT;
static int zlib_available;

typedef const char *(*zlib_version_fn)(void);

static void initialize_zlib(void) {
  zlib_version_fn version_fn = NULL;
  void *deflate_init = NULL;
  void *deflate_run = NULL;
  void *deflate_end = NULL;
  void *inflate_init = NULL;
  void *inflate_run = NULL;
  void *inflate_end = NULL;
  const char *version;
  unsigned major = 0, minor = 0, patch = 0;

  if (!chezpp_optional_library_open(&zlib_library)) return;
  if (!chezpp_optional_library_symbol(&zlib_library, "zlibVersion",
                                      (void **)&version_fn) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflateInit2_",
                                      &deflate_init) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflate", &deflate_run) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflateEnd", &deflate_end) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflateInit2_",
                                      &inflate_init) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflate", &inflate_run) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflateEnd", &inflate_end))
    return;
  version = version_fn();
  chezpp_optional_library_set_version(&zlib_library, version);
  if (version == NULL || sscanf(version, "%u.%u.%u", &major, &minor, &patch) < 2 ||
      major != 1 || minor < 2 || (minor == 2 && patch < 11)) {
    chezpp_optional_library_fail(
        &zlib_library, "zlib runtime %s; requires ABI 1 and version >= 1.2.11",
        version == NULL ? "unknown" : version);
    return;
  }
  zlib_available = 1;
}

int chezpp_zlib_require(void) {
  pthread_once(&zlib_once, initialize_zlib);
  return zlib_available;
}

const chezpp_optional_library *chezpp_zlib_library(void) {
  (void)chezpp_zlib_require();
  return &zlib_library;
}
