#include "common.h"
#include "optional_library.h"

#if CSPP_WITH_OPENSSL
extern const cspp_optional_library *cspp_openssl_library(void);
static const char *openssl_version(void) {
  const cspp_optional_library *library = cspp_openssl_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_XXHASH
extern const cspp_optional_library *cspp_xxhash_library(void);
static const char *xxhash_version(void) {
  const cspp_optional_library *library = cspp_xxhash_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_BLAKE3
extern const cspp_optional_library *cspp_blake3_library(void);
static const char *blake3_version(void) {
  const cspp_optional_library *library = cspp_blake3_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_CURL
extern const cspp_optional_library *cspp_net_curl_library(void);
static const char *curl_version(void) {
  const cspp_optional_library *library = cspp_net_curl_library();
  return library->state == 1 ? library->version : NULL;
}
extern unsigned cspp_net_curl_capabilities(void);
#endif
#if CSPP_WITH_LIBSSH
extern const cspp_optional_library *cspp_net_ssh_library(void);
static const char *ssh_version(void) {
  const cspp_optional_library *library = cspp_net_ssh_library();
  return library->state == 1 ? library->version : NULL;
}
extern unsigned cspp_net_ssh_capabilities(void);
#endif
#if CSPP_WITH_WEBSOCKETS
extern const cspp_optional_library *cspp_net_websocket_library(void);
static const char *websockets_version(void) {
  const cspp_optional_library *library = cspp_net_websocket_library();
  return library->state == 1 ? library->version : NULL;
}
extern unsigned cspp_net_websocket_capabilities(void);
#endif
#if CSPP_WITH_GRPC
extern const cspp_optional_library *cspp_net_grpc_library(void);
static const char *grpc_version(void) {
  const cspp_optional_library *library = cspp_net_grpc_library();
  return library->state == 1 ? library->version : NULL;
}
extern unsigned cspp_net_grpc_capabilities(void);
#endif
#if CSPP_WITH_ZLIB
extern const cspp_optional_library *cspp_zlib_library(void);
static const char *zlib_version(void) {
  const cspp_optional_library *library = cspp_zlib_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_CARES
extern const cspp_optional_library *cspp_cares_library(void);
static const char *cares_version(void) {
  const cspp_optional_library *library = cspp_cares_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_IDN2
extern const cspp_optional_library *cspp_idn2_library(void);
static const char *idn2_version(void) {
  const cspp_optional_library *library = cspp_idn2_library();
  return library->state == 1 ? library->version : NULL;
}
#endif
#if CSPP_WITH_UUID
/* libuuid exposes no portable runtime version API. */
static const char *uuid_version(void) { return NULL; }
#endif

static const cspp_optional_descriptor descriptors[] = {
#if CSPP_WITH_OPENSSL
  {"openssl", 1, openssl_version, NULL, NULL},
#else
  {"openssl", 0, NULL, NULL, "openssl: disabled at build time"},
#endif
#if CSPP_WITH_XXHASH
  {"xxhash", 1, xxhash_version, NULL, NULL},
#else
  {"xxhash", 0, NULL, NULL, "xxhash: disabled at build time"},
#endif
#if CSPP_WITH_BLAKE3
  {"blake3", 1, blake3_version, NULL, NULL},
#else
  {"blake3", 0, NULL, NULL, "blake3: disabled at build time"},
#endif
#if CSPP_WITH_CURL
  {"curl", 1, curl_version, cspp_net_curl_capabilities, NULL},
#else
  {"curl", 0, NULL, NULL, "curl: disabled at build time"},
#endif
#if CSPP_WITH_LIBSSH
  {"ssh", 1, ssh_version, cspp_net_ssh_capabilities, NULL},
#else
  {"ssh", 0, NULL, NULL, "ssh: disabled at build time"},
#endif
#if CSPP_WITH_WEBSOCKETS
  {"websockets", 1, websockets_version, cspp_net_websocket_capabilities, NULL},
#else
  {"websockets", 0, NULL, NULL, "websockets: disabled at build time"},
#endif
#if CSPP_WITH_GRPC
  {"grpc", 1, grpc_version, cspp_net_grpc_capabilities, NULL},
#else
  {"grpc", 0, NULL, NULL, "grpc: disabled at build time"},
#endif
#if CSPP_WITH_ZLIB
  {"zlib", 1, zlib_version, NULL, NULL},
#else
  {"zlib", 0, NULL, NULL, "zlib: disabled at build time"},
#endif
#if CSPP_WITH_CARES
  {"cares", 1, cares_version, NULL, NULL},
#else
  {"cares", 0, NULL, NULL, "cares: disabled at build time"},
#endif
#if CSPP_WITH_IDN2
  {"idn2", 1, idn2_version, NULL, NULL},
#else
  {"idn2", 0, NULL, NULL, "idn2: disabled at build time"},
#endif
#if CSPP_WITH_UUID
  {"uuid", 1, uuid_version, NULL, NULL},
#else
  {"uuid", 0, NULL, NULL, "uuid: disabled at build time"},
#endif
};

static ptr capability_list(const char *name, unsigned bits) {
  ptr result = Snil;
  if (strcmp(name, "ssh") == 0 && (bits & 1U) != 0)
    result = Scons(Sstring_to_symbol("sftp-aio"), result);
  if (strcmp(name, "websockets") == 0 && (bits & 1U) != 0)
    result = Scons(Sstring_to_symbol("tls"), result);
  if (strcmp(name, "grpc") == 0) {
    if ((bits & 1U) != 0)
      result = Scons(Sstring_to_symbol("tls"), result);
    if ((bits & 2U) != 0)
      result = Scons(Sstring_to_symbol("compression"), result);
  }
  return result;
}

static ptr optional_string(const char *value) {
  return value == NULL || value[0] == '\0' ? Sfalse : Sstring(value);
}

static const char *initialization_error(const char *name) {
  (void)name;
#if CSPP_WITH_OPENSSL
  if (strcmp(name, "openssl") == 0) return cspp_openssl_library()->error;
#endif
#if CSPP_WITH_XXHASH
  if (strcmp(name, "xxhash") == 0) return cspp_xxhash_library()->error;
#endif
#if CSPP_WITH_BLAKE3
  if (strcmp(name, "blake3") == 0) return cspp_blake3_library()->error;
#endif
#if CSPP_WITH_CURL
  if (strcmp(name, "curl") == 0) return cspp_net_curl_library()->error;
#endif
#if CSPP_WITH_LIBSSH
  if (strcmp(name, "ssh") == 0) return cspp_net_ssh_library()->error;
#endif
#if CSPP_WITH_WEBSOCKETS
  if (strcmp(name, "websockets") == 0) return cspp_net_websocket_library()->error;
#endif
#if CSPP_WITH_GRPC
  if (strcmp(name, "grpc") == 0) return cspp_net_grpc_library()->error;
#endif
#if CSPP_WITH_ZLIB
  if (strcmp(name, "zlib") == 0) return cspp_zlib_library()->error;
#endif
#if CSPP_WITH_CARES
  if (strcmp(name, "cares") == 0) return cspp_cares_library()->error;
#endif
#if CSPP_WITH_IDN2
  if (strcmp(name, "idn2") == 0) return cspp_idn2_library()->error;
#endif
  return "optional library initialization failed";
}

ptr cspp_optional_library_info(const char *name) {
  if (name == NULL) return Sfalse;
  for (size_t index = 0; index < sizeof(descriptors) / sizeof(descriptors[0]); index++) {
    const cspp_optional_descriptor *descriptor = &descriptors[index];
    if (strcmp(name, descriptor->name) == 0) {
      const char *version = descriptor->version == NULL ? NULL : descriptor->version();
      int available = descriptor->enabled && (version != NULL || strcmp(name, "uuid") == 0);
      unsigned capabilities = available && descriptor->capabilities != NULL
                              ? descriptor->capabilities() : 0;
      const char *error = descriptor->error;
      if (descriptor->enabled && !available) error = initialization_error(name);
      ptr result = Smake_vector(5, Sfalse);
      Svector_set(result, 0, Sstring_to_symbol(name));
      Svector_set(result, 1, available ? Strue : Sfalse);
      Svector_set(result, 2, optional_string(version));
      Svector_set(result, 3, capability_list(name, capabilities));
      Svector_set(result, 4, optional_string(error));
      return result;
    }
  }
  return Sfalse;
}
