#ifndef CSPP_LWS_LOADER_H
#define CSPP_LWS_LOADER_H
#include "../build-config.h"
#if CSPP_WITH_WEBSOCKETS
#include <libwebsockets.h>
#endif

#define CSPP_LWS_CAP_HTTP1 (1U << 0)
#define CSPP_LWS_CAP_HTTP2 (1U << 1)
#define CSPP_LWS_CAP_TLS (1U << 2)
#define CSPP_LWS_CAP_SOCKS5 (1U << 3)
#define CSPP_LWS_CAP_EXTERNAL_POLL (1U << 4)

struct lws_context;
struct lws_context_creation_info;

/* Both adapters must serialize LWS's process-global system-vhost bookkeeping.
 * Creation first initializes global logging once, before any context can exist.
 * Private logging keeps service concurrent except for the affected features below. */
struct lws_context *cspp_lws_create_context(
    const struct lws_context_creation_info *information);
void cspp_lws_context_destroy(struct lws_context *context);

#if CSPP_WITH_WEBSOCKETS
/* Use these entry points outside native callbacks. Feature builds may create
 * globally logged system objects while connecting, servicing, or closing WSIs.
 * Callbacks may use raw LWS calls: their enclosing entry point owns the gate. */
int cspp_lws_service_tsi(struct lws_context *context, int timeout_ms, int tsi);
int cspp_lws_service_fd(struct lws_context *context, struct lws_pollfd *descriptor);
int cspp_lws_service_adjust_timeout(struct lws_context *context, int timeout_ms, int tsi);
struct lws *cspp_lws_client_connect_via_info(const struct lws_client_connect_info *information);
int cspp_lws_http_client_read(struct lws *wsi, char **buffer, int *length);
int cspp_lws_rx_flow_control(struct lws *wsi, int enable);
int cspp_lws_callback_on_writable(struct lws *wsi);
void cspp_lws_set_timeout(struct lws *wsi, enum pending_timeout reason, int seconds);
void cspp_lws_cancel_service(struct lws_context *context);
#endif

int cspp_lws_available(void);
unsigned cspp_lws_capabilities(void);
const char *cspp_lws_error(void);
const char *cspp_lws_version(void);

#endif
