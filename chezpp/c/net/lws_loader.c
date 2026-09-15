#include "lws_loader.h"

#include "../common.h"
#include "../optional_library.h"

#include <pthread.h>
#include <stdio.h>

typedef const char *(*lws_get_library_version_fn)(void);

static const char *const lws_names[] = {"libwebsockets.so.21", NULL};
static chezpp_optional_library lws_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("libwebsockets HTTP", lws_names);
static pthread_once_t lws_once = PTHREAD_ONCE_INIT;
static int lws_available;
static unsigned lws_capability_mask;

static const char *const required_symbols[] = {
    "lws_create_context",
    "lws_context_destroy",
    "lws_client_connect_via_info",
    "lws_service_fd",
    "lws_service_tsi",
    "lws_service_adjust_timeout",
    "lws_cancel_service",
    "lws_callback_on_writable",
    "lws_get_socket_fd",
    "lws_http_client_read",
    "lws_client_http_body_pending",
    "lws_http_transaction_completed",
    "lws_hdr_copy",
    "lws_hdr_copy_fragment",
    "lws_hdr_custom_name_foreach",
    "lws_token_to_string",
    "lws_hdr_total_length",
    "lws_hdr_custom_copy",
    "lws_get_context",
    "lws_context_user",
    "lws_set_opaque_user_data",
    "lws_get_opaque_user_data",
    NULL};

static int load_required_symbols(void) {
  size_t index;
  for (index = 0; required_symbols[index] != NULL; index++) {
    void *symbol = NULL;
    if (!chezpp_optional_library_symbol(&lws_library,
                                        required_symbols[index], &symbol))
      return 0;
  }
  return 1;
}

static int probe_symbol(const char *name) {
  void *symbol = NULL;
  return chezpp_optional_library_probe_symbol(&lws_library, name, &symbol);
}

static void initialize_lws(void) {
  lws_get_library_version_fn version_fn = NULL;
  const char *version;
  unsigned major;
  unsigned minor;
  unsigned patch;

  if (!chezpp_optional_library_open(&lws_library)) return;
  if (!chezpp_optional_library_symbol(&lws_library,
                                      "lws_get_library_version",
                                      (void **)&version_fn))
    return;
  version = version_fn();
  if (version == NULL ||
      sscanf(version, "%u.%u.%u", &major, &minor, &patch) != 3) {
    chezpp_optional_library_fail(
        &lws_library, "libwebsockets HTTP returned invalid version %s",
        version == NULL ? "(null)" : version);
    return;
  }
  chezpp_optional_library_set_version(&lws_library, version);
  if (major < 4 || (major == 4 && minor < 3)) {
    chezpp_optional_library_fail(
        &lws_library,
        "libwebsockets HTTP runtime %u.%u.%u requires version >= 4.3.0",
        major, minor, patch);
    return;
  }
  if (!load_required_symbols()) return;

  lws_capability_mask =
      CHEZPP_LWS_CAP_HTTP1 | CHEZPP_LWS_CAP_EXTERNAL_POLL;
  if (probe_symbol("lws_h2_get_peer_txcredit_estimate"))
    lws_capability_mask |= CHEZPP_LWS_CAP_HTTP2;
  if (probe_symbol("lws_init_vhost_client_ssl"))
    lws_capability_mask |= CHEZPP_LWS_CAP_TLS;
  if (probe_symbol("lws_set_socks"))
    lws_capability_mask |= CHEZPP_LWS_CAP_SOCKS5;
  lws_available = 1;
}

int chezpp_lws_ensure_loaded(void) {
  pthread_once(&lws_once, initialize_lws);
  return lws_available;
}

unsigned chezpp_lws_capabilities(void) {
  (void)chezpp_lws_ensure_loaded();
  return lws_capability_mask;
}

const char *chezpp_lws_error(void) {
  (void)chezpp_lws_ensure_loaded();
  return chezpp_optional_library_error(&lws_library);
}

const char *chezpp_lws_version(void) {
  (void)chezpp_lws_ensure_loaded();
  return lws_library.version;
}

void *chezpp_lws_symbol(const char *name) {
  void *symbol = NULL;
  if (name == NULL || !chezpp_lws_ensure_loaded()) return NULL;
  if (!chezpp_optional_library_symbol(&lws_library, name, &symbol)) return NULL;
  return symbol;
}

ptr chezpp_lws_status(void) {
  ptr result = Smake_vector(4, Sfalse);
  int available = chezpp_lws_ensure_loaded();
  const char *version = chezpp_lws_version();
  const char *error = chezpp_lws_error();

  Svector_set(result, 0, available ? Strue : Sfalse);
  Svector_set(result, 1, Sfixnum((iptr)chezpp_lws_capabilities()));
  Svector_set(result, 2,
              version == NULL || version[0] == '\0' ? Sfalse
                                                      : Sstring(version));
  Svector_set(result, 3,
              error == NULL || error[0] == '\0' ? Sfalse : Sstring(error));
  return result;
}
