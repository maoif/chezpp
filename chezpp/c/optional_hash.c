#include "common.h"
#include "blake3-1.8-abi.h"
#include "xxhash-0.8-abi.h"

#include <dlfcn.h>
#include <stdatomic.h>

chezpp_xxhash_api chezpp_xxhash;
chezpp_blake3_api chezpp_blake3;

typedef struct {
  atomic_int state;
  void *handle;
  char error[256];
} optional_library;

static optional_library xxhash_library;
static optional_library blake3_library;

static void set_error(optional_library *library, const char *format,
                      const char *detail) {
  snprintf(library->error, sizeof(library->error), format, detail);
}

static void *open_library(optional_library *library, const char *const *names,
                          const char *dependency) {
  const char *last_error = NULL;
  for (size_t i = 0; names[i] != NULL; i++) {
    library->handle = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (library->handle != NULL) return library->handle;
    last_error = dlerror();
  }
  set_error(library, "%s: shared library could not be loaded", dependency);
  if (last_error != NULL) {
    size_t used = strlen(library->error);
    snprintf(library->error + used, sizeof(library->error) - used, ": %s",
             last_error);
  }
  return NULL;
}

static int load_symbol(optional_library *library, void **target,
                       const char *dependency, const char *name) {
  dlerror();
  *target = dlsym(library->handle, name);
  if (*target != NULL && dlerror() == NULL) return 1;
  snprintf(library->error, sizeof(library->error),
           "%s: missing required symbol %s", dependency, name);
  return 0;
}

#define LOAD(library, api, field, dependency, symbol)                         \
  load_symbol((library), (void **)&(api).field, (dependency), (symbol))

static void initialize_xxhash(void) {
  static const char *const names[] = {"libxxhash.so.0", "libxxhash.so", NULL};
  unsigned version;
  unsigned major;
  unsigned minor;

  if (open_library(&xxhash_library, names, "xxhash") == NULL) return;
  if (!LOAD(&xxhash_library, chezpp_xxhash, version_number, "xxhash",
            "XXH_versionNumber")) return;

  version = chezpp_xxhash.version_number();
  major = version / 10000;
  minor = (version / 100) % 100;
  if (major != CHEZPP_XXHASH_VERSION_MAJOR ||
      minor != CHEZPP_XXHASH_VERSION_MINOR) {
    snprintf(xxhash_library.error, sizeof(xxhash_library.error),
             "xxhash: runtime version %u.%u requires version %u.%u.x", major,
             minor, CHEZPP_XXHASH_VERSION_MAJOR, CHEZPP_XXHASH_VERSION_MINOR);
    return;
  }

  if (!LOAD(&xxhash_library, chezpp_xxhash, hash32, "xxhash", "XXH32") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64, "xxhash", "XXH64") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_with_seed, "xxhash",
            "XXH3_64bits_withSeed") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_create_state, "xxhash",
            "XXH32_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_free_state, "xxhash",
            "XXH32_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_reset, "xxhash", "XXH32_reset") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_update, "xxhash", "XXH32_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash32_digest, "xxhash", "XXH32_digest") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_create_state, "xxhash",
            "XXH64_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_free_state, "xxhash",
            "XXH64_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_reset, "xxhash", "XXH64_reset") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_update, "xxhash", "XXH64_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash64_digest, "xxhash", "XXH64_digest") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_create_state, "xxhash",
            "XXH3_createState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_free_state, "xxhash",
            "XXH3_freeState") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_reset_with_seed, "xxhash",
            "XXH3_64bits_reset_withSeed") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_update, "xxhash",
            "XXH3_64bits_update") ||
      !LOAD(&xxhash_library, chezpp_xxhash, hash3_64_digest, "xxhash",
            "XXH3_64bits_digest")) return;

  xxhash_library.error[0] = '\0';
}

static void initialize_blake3(void) {
  static const char *const names[] = {"libblake3.so.0", "libblake3.so", NULL};
  const char *version;
  unsigned major;
  unsigned minor;

  if (open_library(&blake3_library, names, "blake3") == NULL) return;
  if (!LOAD(&blake3_library, chezpp_blake3, version, "blake3",
            "blake3_version")) return;

  version = chezpp_blake3.version();
  if (version == NULL || sscanf(version, "%u.%u", &major, &minor) != 2) {
    set_error(&blake3_library, "blake3: invalid runtime version %s",
              version == NULL ? "(null)" : version);
    return;
  }
  if (major != CHEZPP_BLAKE3_VERSION_MAJOR ||
      minor != CHEZPP_BLAKE3_VERSION_MINOR) {
    snprintf(blake3_library.error, sizeof(blake3_library.error),
             "blake3: runtime version %u.%u requires version %u.%u.x", major,
             minor, CHEZPP_BLAKE3_VERSION_MAJOR, CHEZPP_BLAKE3_VERSION_MINOR);
    return;
  }

  if (!LOAD(&blake3_library, chezpp_blake3, hasher_init, "blake3",
            "blake3_hasher_init") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_update, "blake3",
            "blake3_hasher_update") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_finalize, "blake3",
            "blake3_hasher_finalize") ||
      !LOAD(&blake3_library, chezpp_blake3, hasher_reset, "blake3",
            "blake3_hasher_reset")) return;

  blake3_library.error[0] = '\0';
}

static const char *ensure_loaded(optional_library *library,
                                 void (*initialize)(void)) {
  int expected = 0;
  if (atomic_compare_exchange_strong(&library->state, &expected, 1)) {
    initialize();
    atomic_store(&library->state, 2);
  } else {
    while (atomic_load(&library->state) == 1) {
    }
  }
  return library->error[0] == '\0' ? NULL : library->error;
}

ptr chezpp_xxhash_load_error(void) {
  const char *error = ensure_loaded(&xxhash_library, initialize_xxhash);
  return error == NULL ? Sfalse : Sstring(error);
}

ptr chezpp_blake3_load_error(void) {
  const char *error = ensure_loaded(&blake3_library, initialize_blake3);
  return error == NULL ? Sfalse : Sstring(error);
}
