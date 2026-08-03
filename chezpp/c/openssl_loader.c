#include "openssl_loader.h"

#include <dlfcn.h>
#include <pthread.h>
#include <stdio.h>

static const char *const crypto_names[] = {"libcrypto.so.3", NULL};
static chezpp_optional_library crypto_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("openssl", crypto_names);
static const char *const ssl_names[] = {"libssl.so.3", NULL};
static chezpp_optional_library ssl_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("openssl", ssl_names);
static pthread_once_t openssl_once = PTHREAD_ONCE_INIT;
static int openssl_available;

#define CHEZPP_OPENSSL_FIELD(kind, name) __typeof__(&name) name;
typedef struct {
  CHEZPP_OPENSSL_SYMBOLS(CHEZPP_OPENSSL_FIELD)
} chezpp_openssl_table;
#undef CHEZPP_OPENSSL_FIELD

#define CHEZPP_OPENSSL_DEFINE(kind, name) __typeof__(&name) chezpp_openssl_##name;
CHEZPP_OPENSSL_SYMBOLS(CHEZPP_OPENSSL_DEFINE)
#undef CHEZPP_OPENSSL_DEFINE

typedef unsigned long (*openssl_version_num_fn)(void);
typedef const char *(*openssl_version_fn)(int);

#define CHEZPP_OPENSSL_LOAD_crypto(name)                                      \
  chezpp_optional_library_symbol(&crypto_library, #name, (void **)&table.name)
#define CHEZPP_OPENSSL_LOAD_ssl(name)                                         \
  chezpp_optional_library_symbol(&ssl_library, #name, (void **)&table.name)
#define CHEZPP_OPENSSL_LOAD(kind, name)                                       \
  if (!CHEZPP_OPENSSL_LOAD_##kind(name)) goto openssl_failure;
#define CHEZPP_OPENSSL_PUBLISH(kind, name) chezpp_openssl_##name = table.name;

static void initialize_openssl(void) {
  chezpp_openssl_table table = {0};
  openssl_version_num_fn version_num = NULL;
  openssl_version_fn version_text = NULL;
  unsigned long version;
  unsigned major;
  const char *text;

  if (!chezpp_optional_library_open(&crypto_library)) return;
  if (!chezpp_optional_library_symbol(&crypto_library, "OpenSSL_version_num",
                                      (void **)&version_num) ||
      !chezpp_optional_library_symbol(&crypto_library, "OpenSSL_version",
                                      (void **)&version_text))
    goto openssl_failure;
  version = version_num();
  major = (unsigned)((version >> 28) & 0xfUL);
  text = version_text(OPENSSL_VERSION);
  chezpp_optional_library_set_version(&crypto_library, text);
  if (major != 3) {
    chezpp_optional_library_fail(
        &crypto_library,
        "OpenSSL runtime ABI major %u; requires major 3", major);
    goto openssl_failure;
  }
  if (!chezpp_optional_library_open(&ssl_library)) goto openssl_failure;
  chezpp_optional_library_set_version(&ssl_library, text);

  CHEZPP_OPENSSL_SYMBOLS(CHEZPP_OPENSSL_LOAD)
  if (table.OPENSSL_init_crypto(0, NULL) != 1 ||
      table.OPENSSL_init_ssl(0, NULL) != 1) {
    chezpp_optional_library_fail(&ssl_library,
                                 "OpenSSL runtime initialization failed");
    goto openssl_failure;
  }
  CHEZPP_OPENSSL_SYMBOLS(CHEZPP_OPENSSL_PUBLISH)
  openssl_available = 1;
  return;

openssl_failure:
  pthread_mutex_lock(&crypto_library.mutex);
  if (crypto_library.handle != NULL) {
    dlclose(crypto_library.handle);
    crypto_library.handle = NULL;
  }
  if (crypto_library.state == 1) crypto_library.state = -1;
  pthread_mutex_unlock(&crypto_library.mutex);
  pthread_mutex_lock(&ssl_library.mutex);
  if (ssl_library.handle != NULL) {
    dlclose(ssl_library.handle);
    ssl_library.handle = NULL;
  }
  if (ssl_library.state == 1) ssl_library.state = -1;
  pthread_mutex_unlock(&ssl_library.mutex);
}

#undef CHEZPP_OPENSSL_PUBLISH
#undef CHEZPP_OPENSSL_LOAD
#undef CHEZPP_OPENSSL_LOAD_ssl
#undef CHEZPP_OPENSSL_LOAD_crypto

int chezpp_openssl_require(void) {
  pthread_once(&openssl_once, initialize_openssl);
  return openssl_available;
}

int chezpp_openssl_crypto_symbol(const char *name, void **target) {
  if (!chezpp_openssl_require()) return 0;
  return chezpp_optional_library_symbol(&crypto_library, name, target);
}

int chezpp_openssl_ssl_symbol(const char *name, void **target) {
  if (!chezpp_openssl_require()) return 0;
  return chezpp_optional_library_symbol(&ssl_library, name, target);
}

const chezpp_optional_library *chezpp_openssl_library(void) {
  if (crypto_library.error[0] != '\0') return &crypto_library;
  if (ssl_library.error[0] != '\0') return &ssl_library;
  if (crypto_library.state == -1) return &crypto_library;
  if (ssl_library.state == -1) return &ssl_library;
  return &crypto_library;
}
