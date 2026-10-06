#ifndef CHEZPP_LWS_LOADER_H
#define CHEZPP_LWS_LOADER_H

#define CHEZPP_LWS_CAP_HTTP1 (1U << 0)
#define CHEZPP_LWS_CAP_HTTP2 (1U << 1)
#define CHEZPP_LWS_CAP_TLS (1U << 2)
#define CHEZPP_LWS_CAP_SOCKS5 (1U << 3)
#define CHEZPP_LWS_CAP_EXTERNAL_POLL (1U << 4)

int chezpp_lws_ensure_loaded(void);
unsigned chezpp_lws_capabilities(void);
const char *chezpp_lws_error(void);
const char *chezpp_lws_version(void);

#endif
