#include "common.h"
#include "openssl_loader.h"
#include "optional_library.h"

extern const chezpp_optional_library *chezpp_xxhash_library(void);
extern const chezpp_optional_library *chezpp_blake3_library(void);
extern const chezpp_optional_library *chezpp_net_curl_library(void);
extern const chezpp_optional_library *chezpp_net_ssh_library(void);
extern const chezpp_optional_library *chezpp_net_websocket_library(void);
extern const chezpp_optional_library *chezpp_net_grpc_library(void);
extern const chezpp_optional_library *chezpp_zlib_library(void);
extern const chezpp_optional_library *chezpp_nghttp2_library(void);
extern const chezpp_optional_library *chezpp_cares_library(void);
extern const chezpp_optional_library *chezpp_idn2_library(void);
extern unsigned chezpp_net_curl_capabilities(void);
extern unsigned chezpp_net_ssh_capabilities(void);
extern unsigned chezpp_net_websocket_capabilities(void);
extern unsigned chezpp_net_grpc_capabilities(void);

static ptr capability_list(const char *name, unsigned bits) {
  ptr result = Snil;
  if (strcmp(name, "ssh") == 0 && (bits & 1U) != 0)
    result = Scons(Sstring_to_symbol("sftp-aio"), result);
  if (strcmp(name, "websockets") == 0) {
    if ((bits & 1U) != 0)
      result = Scons(Sstring_to_symbol("tls"), result);
    if ((bits & 2U) != 0)
      result = Scons(Sstring_to_symbol("compression"), result);
  }
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

static const chezpp_optional_library *find_library(const char *name,
                                                    unsigned *capabilities) {
  *capabilities = 0;
  if (strcmp(name, "openssl") == 0) {
    (void)chezpp_openssl_require();
    return chezpp_openssl_library();
  }
  if (strcmp(name, "xxhash") == 0) return chezpp_xxhash_library();
  if (strcmp(name, "blake3") == 0) return chezpp_blake3_library();
  if (strcmp(name, "zlib") == 0) return chezpp_zlib_library();
  if (strcmp(name, "nghttp2") == 0) return chezpp_nghttp2_library();
  if (strcmp(name, "cares") == 0) return chezpp_cares_library();
  if (strcmp(name, "idn2") == 0) return chezpp_idn2_library();
  if (strcmp(name, "curl") == 0) {
    *capabilities = chezpp_net_curl_capabilities();
    return chezpp_net_curl_library();
  }
  if (strcmp(name, "ssh") == 0) {
    *capabilities = chezpp_net_ssh_capabilities();
    return chezpp_net_ssh_library();
  }
  if (strcmp(name, "websockets") == 0) {
    *capabilities = chezpp_net_websocket_capabilities();
    return chezpp_net_websocket_library();
  }
  if (strcmp(name, "grpc") == 0) {
    *capabilities = chezpp_net_grpc_capabilities();
    return chezpp_net_grpc_library();
  }
  return NULL;
}

ptr chezpp_optional_library_info(const char *name) {
  const chezpp_optional_library *library;
  unsigned capabilities;
  ptr result;

  if (name == NULL || (library = find_library(name, &capabilities)) == NULL)
    return Sfalse;
  result = Smake_vector(5, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol(name));
  Svector_set(result, 1, library->state == 1 ? Strue : Sfalse);
  Svector_set(result, 2, optional_string(library->version));
  Svector_set(result, 3, capability_list(name, capabilities));
  Svector_set(result, 4, optional_string(library->error));
  return result;
}
