#include "common.h"
#include "blake3-1.8-abi.h"
#include "optional_library.h"
#include "xxhash-0.8-abi.h"

chezpp_xxhash_api chezpp_xxhash;
chezpp_blake3_api chezpp_blake3;

static const char *const xxhash_names[] = {
    "libxxhash.so.0", "libxxhash.so", NULL};
static chezpp_optional_library xxhash_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("xxhash", xxhash_names);
static pthread_once_t xxhash_once = PTHREAD_ONCE_INIT;

static const char *const blake3_names[] = {
    "libblake3.so.1", "libblake3.so.0", "libblake3.so", NULL};
static chezpp_optional_library blake3_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("blake3", blake3_names);
static pthread_once_t blake3_once = PTHREAD_ONCE_INIT;

#define LOAD(library, api, field, symbol)                                     \
  chezpp_optional_library_symbol((library), (symbol),                         \
                                 (void **)&(api).field)

static void initialize_xxhash(void) {
  char version_string[32];
  unsigned version;
  unsigned major;
  unsigned minor;

  if (!chezpp_optional_library_open(&xxhash_library)) return;
  if (!LOAD(&xxhash_library, chezpp_xxhash, version_number,
            "XXH_versionNumber")) return;

  version = chezpp_xxhash.version_number();
  major = version / 10000;
  minor = (version / 100) % 100;
  snprintf(version_string, sizeof(version_string), "%u.%u.%u", major, minor,
           version % 100);
  chezpp_optional_library_set_version(&xxhash_library, version_string);
  if (major != CHEZPP_XXHASH_VERSION_MAJOR ||
      minor != CHEZPP_XXHASH_VERSION_MINOR) {
    chezpp_optional_library_fail(
        &xxhash_library,
        "xxhash: runtime version %u.%u requires version %u.%u.x", major, minor,
        CHEZPP_XXHASH_VERSION_MAJOR, CHEZPP_XXHASH_VERSION_MINOR);
    return;
  }

  if (!LOAD(&xxhash_library, chezpp_xxhash, hash32, "XXH32") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64, "XXH64") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_with_seed,
            "XXH3_64bits_withSeed") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_create_state,
            "XXH32_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_free_state,
            "XXH32_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_reset, "XXH32_reset") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_update, "XXH32_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_digest, "XXH32_digest") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_create_state,
            "XXH64_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_free_state,
            "XXH64_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_reset, "XXH64_reset") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_update, "XXH64_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_digest, "XXH64_digest") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_create_state,
            "XXH3_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_free_state,
            "XXH3_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_reset_with_seed,
            "XXH3_64bits_reset_withSeed") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_update,
            "XXH3_64bits_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_digest,
            "XXH3_64bits_digest")) return;
}

static void initialize_blake3(void) {
  const char *version;
  unsigned major;
  unsigned minor;

  if (!chezpp_optional_library_open(&blake3_library)) return;
  if (!LOAD(&blake3_library, chezpp_blake3, version, "blake3_version"))
    return;

  version = chezpp_blake3.version();
  if (version == NULL || sscanf(version, "%u.%u", &major, &minor) != 2) {
    chezpp_optional_library_fail(&blake3_library,
                                 "blake3: invalid runtime version %s",
                                 version == NULL ? "(null)" : version);
    return;
  }
  chezpp_optional_library_set_version(&blake3_library, version);
  if (major != CHEZPP_BLAKE3_VERSION_MAJOR ||
      minor != CHEZPP_BLAKE3_VERSION_MINOR) {
    chezpp_optional_library_fail(
        &blake3_library,
        "blake3: runtime version %u.%u requires version %u.%u.x", major, minor,
        CHEZPP_BLAKE3_VERSION_MAJOR, CHEZPP_BLAKE3_VERSION_MINOR);
    return;
  }

  if (!LOAD(&blake3_library, chezpp_blake3, hasher_init,
            "blake3_hasher_init") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_update,
            "blake3_hasher_update") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_finalize,
            "blake3_hasher_finalize") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_reset,
            "blake3_hasher_reset")) return;
}

ptr chezpp_xxhash_load_error(void) {
  const char *error;
  pthread_once(&xxhash_once, initialize_xxhash);
  error = chezpp_optional_library_error(&xxhash_library);
  return error == NULL || error[0] == '\0' ? Sfalse : Sstring(error);
}

ptr chezpp_blake3_load_error(void) {
  const char *error;
  pthread_once(&blake3_once, initialize_blake3);
  error = chezpp_optional_library_error(&blake3_library);
  return error == NULL || error[0] == '\0' ? Sfalse : Sstring(error);
}
