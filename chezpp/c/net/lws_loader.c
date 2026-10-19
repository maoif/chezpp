#include "lws_loader.h"
#include "../common.h"
#include "../build-config.h"
#if CHEZPP_WITH_WEBSOCKETS
#include <libwebsockets.h>
#include <pthread.h>

/* Even with a private `log_cx`, LWS 4.5 creates its internal `system` vhost
 * against the default logging context. With `LWS_MAX_SMP=1`, its refcount is
 * non-atomic. Share this lock across HTTP, WebSocket, and lifetime contexts;
 * separate adapter locks cannot protect the dependency's shared counter.
 * Ordinary configurations use private log contexts during service. Builds with
 * system networking or JIT trust may bind additional objects to the global log
 * during connection setup, timer callbacks, reads, or WSI destruction. Their
 * callback-capable entry points must also acquire this same gate. */
static pthread_mutex_t lws_runtime_mutex = PTHREAD_MUTEX_INITIALIZER;
static _Thread_local unsigned lws_runtime_depth;

#if LWS_MAX_SMP == 1 && (defined(LWS_WITH_TLS_JIT_TRUST) || \
    defined(LWS_WITH_SYS_ASYNC_DNS) || defined(LWS_WITH_SYS_DHCP_CLIENT) || \
    defined(LWS_WITH_SYS_NTPCLIENT))
#define CHEZPP_LWS_GUARD_RUNTIME 1
#else
#define CHEZPP_LWS_GUARD_RUNTIME 0
#endif

static void enter_lws_runtime(void) {
  /* Native callbacks can reenter an adapter helper on this OS thread. Count
   * nesting instead of taking the nonrecursive mutex twice. No Scheme code or
   * continuation runs within these calls. The outer entry always releases it. */
  if (lws_runtime_depth++ == 0) pthread_mutex_lock(&lws_runtime_mutex);
}

static void leave_lws_runtime(void) {
  if (--lws_runtime_depth == 0) pthread_mutex_unlock(&lws_runtime_mutex);
}

static void enter_lws_service(void) {
  if (CHEZPP_LWS_GUARD_RUNTIME) enter_lws_runtime();
}

static void leave_lws_service(void) {
  if (CHEZPP_LWS_GUARD_RUNTIME) leave_lws_runtime();
}

struct lws_context *chezpp_lws_create_context(
    const struct lws_context_creation_info *information) {
  struct lws_context *context;
  enter_lws_runtime();
  context = lws_create_context(information);
  leave_lws_runtime();
  return context;
}

void chezpp_lws_context_destroy(struct lws_context *context) {
  enter_lws_runtime();
  lws_context_destroy(context);
  leave_lws_runtime();
}

/* Do not hold the gate across the adapters' OS poll waits. Every service call
 * below is bounded by the caller, which uses LWS's nonblocking timeout `-1`.
 * Keep the wrappers here so HTTP and WebSocket cannot accidentally use separate
 * locks. The feature-disabled path compiles to a direct dependency call. */
int chezpp_lws_service_tsi(struct lws_context *context, int timeout_ms, int tsi) {
  int result;
  enter_lws_service();
  result = lws_service_tsi(context, timeout_ms, tsi);
  leave_lws_service();
  return result;
}

int chezpp_lws_service_fd(struct lws_context *context, struct lws_pollfd *descriptor) {
  int result;
  enter_lws_service();
  result = lws_service_fd(context, descriptor);
  leave_lws_service();
  return result;
}

int chezpp_lws_service_adjust_timeout(struct lws_context *context, int timeout_ms, int tsi) {
  int result;
  enter_lws_service();
  /* This also initializes protocols and can dispatch SMD / timer callbacks. */
  result = lws_service_adjust_timeout(context, timeout_ms, tsi);
  leave_lws_service();
  return result;
}

struct lws *chezpp_lws_client_connect_via_info(
    const struct lws_client_connect_info *information) {
  struct lws *result;
  enter_lws_service();
  result = lws_client_connect_via_info(information);
  leave_lws_service();
  return result;
}

int chezpp_lws_http_client_read(struct lws *wsi, char **buffer, int *length) {
  int result;
  enter_lws_service();
  result = lws_http_client_read(wsi, buffer, length);
  leave_lws_service();
  return result;
}

int chezpp_lws_rx_flow_control(struct lws *wsi, int enable) {
  int result;
  enter_lws_service();
  result = lws_rx_flow_control(wsi, enable);
  leave_lws_service();
  return result;
}

int chezpp_lws_callback_on_writable(struct lws *wsi) {
  int result;
  enter_lws_service();
  result = lws_callback_on_writable(wsi);
  leave_lws_service();
  return result;
}

void chezpp_lws_set_timeout(struct lws *wsi, enum pending_timeout reason, int seconds) {
  enter_lws_service();
  /* `LWS_TO_KILL_SYNC` can destroy a WSI and invoke close callbacks immediately. */
  lws_set_timeout(wsi, reason, seconds);
  leave_lws_service();
}

void chezpp_lws_cancel_service(struct lws_context *context) {
  enter_lws_service();
  lws_cancel_service(context);
  leave_lws_service();
}

void chezpp_lws_set_log_level(int flags, lws_log_emit_t emit) {
  /* Initialization can race with another adapter already servicing contexts.
   * Protect writes to the dependency's global logging configuration as well. */
  enter_lws_runtime();
  lws_set_log_level(flags, emit);
  leave_lws_runtime();
}
#endif
int chezpp_lws_available(void) {
#if CHEZPP_WITH_WEBSOCKETS
  unsigned major, minor, patch;
  const char *version = lws_get_library_version();
  return version != NULL && sscanf(version, "%u.%u.%u", &major, &minor, &patch) == 3 &&
         (major > 4 || (major == 4 && minor >= 3));
#else
  return 0;
#endif
}
unsigned chezpp_lws_capabilities(void) {
  unsigned capabilities = 0;
#if CHEZPP_WITH_WEBSOCKETS
  if (!chezpp_lws_available()) return 0;
  capabilities = CHEZPP_LWS_CAP_HTTP1 | CHEZPP_LWS_CAP_EXTERNAL_POLL;
#if defined(LWS_ROLE_H2) || defined(LWS_WITH_HTTP2)
  capabilities |= CHEZPP_LWS_CAP_HTTP2;
#endif
#if defined(LWS_WITH_TLS)
  capabilities |= CHEZPP_LWS_CAP_TLS;
#endif
#if defined(LWS_WITH_SOCKS5)
  capabilities |= CHEZPP_LWS_CAP_SOCKS5;
#endif
#endif
  return capabilities;
}
const char *chezpp_lws_error(void) {
#if CHEZPP_WITH_WEBSOCKETS
  return chezpp_lws_available() ? "" : "libwebsockets HTTP requires version >= 4.3.0";
#else
  return "websockets: disabled at build time";
#endif
}
const char *chezpp_lws_version(void) {
#if CHEZPP_WITH_WEBSOCKETS
  return lws_get_library_version();
#else
  return NULL;
#endif
}

ptr chezpp_lws_status(void) {
  ptr result = Smake_vector(4, Sfalse);
  int available = chezpp_lws_available();
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
