/* Observe and forward real LWS calls: adapter startup must finish configuring
 * global logging before any context exists, then leave it immutable during
 * service. Private WebSocket logging may still use a different log level. */
#include "../cspp/c/net/lws_http.h"
#include "../cspp/c/net/lws_loader.h"
#include "../cspp/c/optional_library.h"

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern const cspp_optional_library *cspp_net_websocket_library(void);
extern void __real_lws_set_log_level(int flags, lws_log_emit_t emit);
extern struct lws_context *__real_lws_create_context(
    const struct lws_context_creation_info *information);

static atomic_int log_writes;
static atomic_int context_entries;
static atomic_int late_writes;
static atomic_int missing_initialization;
static pthread_barrier_t start;
static int http_first;
static int callback_ran;

void __wrap_lws_set_log_level(int flags, lws_log_emit_t emit) {
  if (atomic_load(&context_entries)) atomic_fetch_add(&late_writes, 1);
  atomic_fetch_add(&log_writes, 1);
  __real_lws_set_log_level(flags, emit);
}

struct lws_context *__wrap_lws_create_context(
    const struct lws_context_creation_info *information) {
  if (atomic_load(&log_writes) != 1) atomic_store(&missing_initialization, 1);
  atomic_fetch_add(&context_entries, 1);
  return __real_lws_create_context(information);
}

static void initialize_http(void) {
  uintptr_t context = cspp_lws_http_context_open(32, 4096, 0, NULL, 0);
  if (!context) abort();
  cspp_lws_http_context_close(context);
}

static void initialize_websocket(void) {
  const cspp_optional_library *library = cspp_net_websocket_library();
  if (library->state < 0) abort();
}

static void *initialize_adapter(void *argument) {
  pthread_barrier_wait(&start);
  if ((uintptr_t)argument % 2) initialize_http();
  else initialize_websocket();
  return NULL;
}

static void initialize_peer(lws_sorted_usec_list_t *timer) {
  (void)timer;
  /* Starting the second adapter during real service used to rewrite global
   * flags while unrelated service threads could be reading those same flags. */
  if (http_first) initialize_websocket();
  else initialize_http();
  callback_ran = 1;
}

int main(int argc, char **argv) {
  if (argc != 2 || setenv("CSPP_WS_LOGLEVEL", "1", 1)) return 2;
  if (!strcmp(argv[1], "concurrent")) {
    pthread_t threads[8];
    if (pthread_barrier_init(&start, NULL, 8)) return 2;
    for (uintptr_t index = 0; index < 8; index++)
      if (pthread_create(&threads[index], NULL, initialize_adapter, (void *)index)) return 2;
    for (int index = 0; index < 8; index++) pthread_join(threads[index], NULL);
    pthread_barrier_destroy(&start);
  } else {
    struct lws_context_creation_info information = {0};
    lws_log_cx_t log_context = {.u.emit = lwsl_emit_stderr};
    lws_sorted_usec_list_t timer = {0};
    struct lws_context *context;
    http_first = !strcmp(argv[1], "http-first");
    if (http_first) initialize_http();
    else initialize_websocket();
    information.port = CONTEXT_PORT_NO_LISTEN;
    information.log_cx = &log_context;
    information.fd_limit_per_thread = 16;
    context = cspp_lws_create_context(&information);
    if (!context) return 2;
    lws_sul_schedule(context, 0, &timer, initialize_peer, 1);
    for (int remaining = 1000; remaining && !callback_ran; remaining--)
      if (cspp_lws_service_tsi(context, -1, 0) < 0) return 2;
    cspp_lws_context_destroy(context);
#if LWS_MAX_SMP > 1
    if (log_context.inited == 2) pthread_mutex_destroy(&log_context.refcount_lock);
#endif
    if (!callback_ran) return 2;
  }
  if (atomic_load(&log_writes) != 1 || atomic_load(&late_writes) ||
      atomic_load(&missing_initialization)) {
    fprintf(stderr, "LWS logging (%s): writes=%d late=%d missing-initialization=%d\n",
            argv[1], atomic_load(&log_writes), atomic_load(&late_writes),
            atomic_load(&missing_initialization));
    return 1;
  }
  return 0;
}
