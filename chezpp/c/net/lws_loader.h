#ifndef CHEZPP_LWS_LOADER_H
#define CHEZPP_LWS_LOADER_H

#define CHEZPP_LWS_CAP_HTTP1 (1U << 0)
#define CHEZPP_LWS_CAP_HTTP2 (1U << 1)
#define CHEZPP_LWS_CAP_TLS (1U << 2)
#define CHEZPP_LWS_CAP_SOCKS5 (1U << 3)
#define CHEZPP_LWS_CAP_EXTERNAL_POLL (1U << 4)

struct lws_context;
struct lws_context_creation_info;

/* Both adapters must serialize LWS's process-global system-vhost bookkeeping.
 * Callers also supply private logging contexts so service can stay concurrent. */
struct lws_context *chezpp_lws_create_context(
    const struct lws_context_creation_info *information);
void chezpp_lws_context_destroy(struct lws_context *context);

int chezpp_lws_available(void);
unsigned chezpp_lws_capabilities(void);
const char *chezpp_lws_error(void);
const char *chezpp_lws_version(void);

#endif
