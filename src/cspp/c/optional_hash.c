#include "common.h"
#include "optional_library.h"
#if CSPP_WITH_XXHASH
#include <xxhash.h>
static cspp_optional_library xxhash_library = CSPP_OPTIONAL_LIBRARY_INIT("xxhash");
static pthread_once_t xxhash_once = PTHREAD_ONCE_INIT;
static void initialize_xxhash(void) {
  unsigned version = XXH_versionNumber();
  char text[32];
  snprintf(text, sizeof(text), "%u.%u.%u", version / 10000, (version / 100) % 100, version % 100);
  cspp_optional_library_set_version(&xxhash_library, text);
  if (version / 10000 != 0 || (version / 100) % 100 != 8)
    cspp_optional_library_fail(&xxhash_library, "xxhash: runtime version %u.%u requires version 0.8.x",
      version / 10000, (version / 100) % 100);
}
const cspp_optional_library *cspp_xxhash_library(void) {
  pthread_once(&xxhash_once, initialize_xxhash); return &xxhash_library;
}
#endif
#if CSPP_WITH_BLAKE3
#include <blake3.h>
static cspp_optional_library blake3_library = CSPP_OPTIONAL_LIBRARY_INIT("blake3");
static pthread_once_t blake3_once = PTHREAD_ONCE_INIT;
static void initialize_blake3(void) {
  unsigned major = 0, minor = 0;
  const char *version = blake3_version();
  cspp_optional_library_set_version(&blake3_library, version);
  if (version == NULL || sscanf(version, "%u.%u", &major, &minor) != 2 || major != 1 || minor != 8)
    cspp_optional_library_fail(&blake3_library, "blake3: runtime version %u.%u requires version 1.8.x", major, minor);
}
const cspp_optional_library *cspp_blake3_library(void) {
  pthread_once(&blake3_once, initialize_blake3); return &blake3_library;
}
#endif
ptr cspp_xxhash_load_error(void) {
#if CSPP_WITH_XXHASH
  const cspp_optional_library *library = cspp_xxhash_library();
  return library->state == 1 ? Sfalse : Sstring(library->error);
#else
  return Sstring("xxhash: disabled at build time");
#endif
}
ptr cspp_blake3_load_error(void) {
#if CSPP_WITH_BLAKE3
  const cspp_optional_library *library = cspp_blake3_library();
  return library->state == 1 ? Sfalse : Sstring(library->error);
#else
  return Sstring("blake3: disabled at build time");
#endif
}
