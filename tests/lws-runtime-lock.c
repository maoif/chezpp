/* Real LWS timer callbacks exercise the adapter's conditional runtime lock.
 * Feature-selection headers used by the runner change only the guard policy,
 * after the dependency has declared its ABI. No LWS functions are mocked. */
#include "../cspp/c/net/lws_http.h"
#include "../cspp/c/net/lws_loader.h"

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static atomic_int active_callbacks;
static atomic_int overlapped;
static atomic_int callbacks;
static pthread_barrier_t start;

typedef struct {
  lws_http_context adapter;
  lws_sorted_usec_list_t timer;
  int reenter;
} worker;

static void pause_ms(long milliseconds) {
  struct timespec duration = {milliseconds / 1000, milliseconds % 1000 * 1000000};
  nanosleep(&duration, NULL);
}

static void timer_callback(lws_sorted_usec_list_t *timer) {
  worker *state = (worker *)((char *)timer - offsetof(worker, timer));
  int count = atomic_fetch_add(&active_callbacks, 1) + 1;
  if (count > 1) atomic_store(&overlapped, 1);
  atomic_fetch_add(&callbacks, 1);
  /* Nested calls are possible from native callbacks. A nonrecursive gate hangs. */
  if (state->reenter) {
    struct lws_context_creation_info info = {0};
    lws_log_cx_t log_context = {.u.emit = lwsl_emit_stderr};
    struct lws_context *nested;
    (void)cspp_lws_http_context_timeout_ms((uintptr_t)&state->adapter, 1);
    /* Lifecycle calls and service must share a reentrant gate, too. */
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.log_cx = &log_context;
    info.fd_limit_per_thread = 16;
    nested = cspp_lws_create_context(&info);
    if (!nested) abort();
    cspp_lws_context_destroy(nested);
#if LWS_MAX_SMP > 1
    if (log_context.inited == 2) pthread_mutex_destroy(&log_context.refcount_lock);
#endif
  }
  /* A safe configuration must allow the peer to enter. A protected one must
   * keep it out until this callback returns; neither case waits indefinitely. */
  for (int remaining = 100; remaining && !atomic_load(&overlapped); remaining--)
    pause_ms(1);
  atomic_fetch_sub(&active_callbacks, 1);
}

static void *service(void *argument) {
  worker *state = argument;
  pthread_barrier_wait(&start);
  if (cspp_lws_http_context_service_fd((uintptr_t)&state->adapter, -1, 0) < 0)
    abort();
  return NULL;
}

int main(int argc, char **argv) {
  worker states[2] = {0};
  pthread_t threads[2];
  int expect_serialized = argc > 1 && atoi(argv[1]);
  if (pthread_barrier_init(&start, NULL, 2)) return 2;
  for (int index = 0; index < 2; index++) {
    struct lws_context_creation_info info = {0};
    states[index].adapter.log_context.u.emit = lwsl_emit_stderr;
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.log_cx = &states[index].adapter.log_context;
    info.fd_limit_per_thread = 16;
    states[index].adapter.lws = cspp_lws_create_context(&info);
    if (!states[index].adapter.lws) return 2;
    states[index].reenter = 1;
    lws_sul_schedule(states[index].adapter.lws, 0, &states[index].timer, timer_callback, 1);
  }
  pause_ms(1);
  for (int index = 0; index < 2; index++)
    if (pthread_create(&threads[index], NULL, service, &states[index])) return 2;
  for (int index = 0; index < 2; index++) {
    pthread_join(threads[index], NULL);
    cspp_lws_context_destroy(states[index].adapter.lws);
#if LWS_MAX_SMP > 1
    if (states[index].adapter.log_context.inited == 2)
      pthread_mutex_destroy(&states[index].adapter.log_context.refcount_lock);
#endif
  }
  pthread_barrier_destroy(&start);
  if (atomic_load(&callbacks) != 2 ||
      atomic_load(&overlapped) == expect_serialized) {
    fprintf(stderr, "runtime guard: callbacks=%d overlap=%d expected-serialized=%d\n",
            atomic_load(&callbacks), atomic_load(&overlapped), expect_serialized);
    return 1;
  }
  return 0;
}
