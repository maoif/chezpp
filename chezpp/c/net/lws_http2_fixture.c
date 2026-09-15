#include <libwebsockets.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Test-only executable: all protocol and TLS work belongs to dynamically loaded LWS. */
#define LWS_FUNCTIONS(X) \
  X(lws_create_context) X(lws_context_destroy) X(lws_service) X(lws_cancel_service) \
  X(lws_callback_on_writable) X(lws_get_network_wsi) X(lws_get_vhost_by_name) \
  X(lws_get_vhost_listen_port) X(lws_add_http_header_status) \
  X(lws_add_http_header_by_name) X(lws_add_http_header_by_token) \
  X(lws_finalize_write_http_header) X(lws_write) X(lws_http_transaction_completed) \
  X(lws_set_timeout) X(lws_get_peer_write_allowance) X(lws_hdr_total_length) \
  X(lws_set_log_level)
#define DECLARE(name) static __typeof__(name) *fn_##name;
LWS_FUNCTIONS(DECLARE)
#undef DECLARE

typedef struct fixture_stream {
  struct lws *wsi;
  unsigned connection;
  size_t length;
  size_t sent;
  size_t received;
  int active;
  int held;
  int headers_sent;
} fixture_stream;

static struct lws *networks[64];
static unsigned network_ids[64];
static fixture_stream *streams[64];
static unsigned accepts, active, peak, requests;
static unsigned goaways, admission_rejections;
static size_t received_bytes;
static int running = 1;

/* LWS exposes GOAWAY scheduling through its public log callback, not a frame callback.
 * This observes the LWS action without encoding or parsing any HTTP/2 frames here. */
static void fixture_log(int level, const char *line) {
  (void)level;
  if (strstr(line, "lws_h2_goaway:") != NULL) goaways++;
  if (strstr(line, "Another stream not allowed") != NULL) admission_rejections++;
}

static unsigned network_id(struct lws *wsi) {
  struct lws *network = fn_lws_get_network_wsi(wsi);
  size_t i;
  for (i = 0; i < 64; i++)
    if (networks[i] == network) return network_ids[i];
  for (i = 0; i < 64; i++) {
    if (networks[i] == NULL) {
      networks[i] = network;
      network_ids[i] = ++accepts;
      return network_ids[i];
    }
  }
  return 0;
}

static void detach_stream(fixture_stream *stream) {
  size_t i;
  if (stream == NULL || !stream->active) return;
  for (i = 0; i < 64; i++) if (streams[i] == stream) streams[i] = NULL;
  stream->active = 0;
  stream->wsi = NULL;
  active--;
}

static int callback(struct lws *wsi, enum lws_callback_reasons reason,
                    void *user, void *input, size_t length) {
  fixture_stream *stream = user;
  size_t i;
  switch (reason) {
    case LWS_CALLBACK_SERVER_NEW_CLIENT_INSTANTIATED:
      return network_id(wsi) == 0 ? -1 : 0;
    case LWS_CALLBACK_HTTP:
      if (stream == NULL) return -1;
      memset(stream, 0, sizeof(*stream));
      stream->wsi = wsi;
      stream->connection = network_id(wsi);
      stream->length = length == 6 && memcmp(input, "/large", 6) == 0 ? 262144 : 10;
      stream->held = length >= 5 && memcmp(input, "/hold", 5) == 0;
      for (i = 0; i < 64 && streams[i] != NULL; i++) {}
      if (i == 64 || stream->connection == 0) return -1;
      streams[i] = stream;
      stream->active = 1;
      requests++;
      if (++active > peak) peak = active;
      if (!stream->held && fn_lws_hdr_total_length(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH) == 0)
        fn_lws_callback_on_writable(wsi);
      return 0;
    case LWS_CALLBACK_HTTP_BODY:
      if (stream == NULL || !stream->active) return -1;
      stream->received += length;
      received_bytes += length;
      return stream->received > 16777216 ? -1 : 0;
    case LWS_CALLBACK_HTTP_BODY_COMPLETION:
      if (stream != NULL && stream->active && !stream->held)
        fn_lws_callback_on_writable(wsi);
      return 0;
    case LWS_CALLBACK_HTTP_WRITEABLE: {
      unsigned char buffer[LWS_PRE + 4096];
      unsigned char *start = buffer + LWS_PRE, *cursor = start;
      unsigned char *end = buffer + sizeof(buffer);
      char number[32];
      int digits, written;
      size_t count, allowance;
      if (stream == NULL || !stream->active || stream->held) return 0;
      if (!stream->headers_sent) {
        digits = snprintf(number, sizeof(number), "%zu", stream->length);
        if (fn_lws_add_http_header_status(wsi, 200, &cursor, end) ||
            fn_lws_add_http_header_by_token(wsi, WSI_TOKEN_HTTP_CONTENT_LENGTH,
                                           (unsigned char *)number, digits, &cursor, end))
          return -1;
        digits = snprintf(number, sizeof(number), "%u", stream->connection);
        if (fn_lws_add_http_header_by_name(wsi, (const unsigned char *)"x-fixture-connection:",
                                         (unsigned char *)number, digits, &cursor, end) ||
            fn_lws_finalize_write_http_header(wsi, start, &cursor, end))
          return -1;
        stream->headers_sent = 1;
        return fn_lws_callback_on_writable(wsi) < 0 ? -1 : 0;
      }
      count = stream->length - stream->sent;
      if (count > 4096) count = 4096;
      allowance = fn_lws_get_peer_write_allowance(wsi);
      if (allowance != (size_t)-1 && count > allowance) count = allowance;
      if (count == 0) return 0;
      memset(start, 'x', count);
      written = fn_lws_write(wsi, start, count,
                            stream->sent + count == stream->length
                                ? LWS_WRITE_HTTP_FINAL : LWS_WRITE_HTTP);
      if (written < 0 || (size_t)written > count) return -1;
      stream->sent += (size_t)written;
      if (stream->sent == stream->length) return fn_lws_http_transaction_completed(wsi);
      return fn_lws_callback_on_writable(wsi) < 0 ? -1 : 0;
    }
    case LWS_CALLBACK_HTTP_DROP_PROTOCOL:
    case LWS_CALLBACK_CLOSED_HTTP:
      detach_stream(stream);
      return 0;
    case LWS_CALLBACK_WSI_DESTROY:
      for (i = 0; i < 64; i++) if (networks[i] == wsi) networks[i] = NULL;
      return 0;
    default:
      return 0;
  }
}

static void command(const char *line) {
  size_t i;
  if (strcmp(line, "stats") == 0) {
    printf("(stats %u %u %u %u %zu %u %u)\n", accepts, active, peak, requests, received_bytes,
           goaways, admission_rejections);
    fflush(stdout);
  } else if (strcmp(line, "release") == 0) {
    for (i = 0; i < 64; i++) {
      if (streams[i] != NULL) {
        streams[i]->held = 0;
        fn_lws_callback_on_writable(streams[i]->wsi);
      }
    }
  } else if (strcmp(line, "close") == 0) {
    for (i = 0; i < 64; i++)
      if (networks[i] != NULL)
        fn_lws_set_timeout(networks[i], PENDING_TIMEOUT_CLOSE_SEND, LWS_TO_KILL_ASYNC);
  } else if (strcmp(line, "reset") == 0) {
    for (i = 0; i < 64; i++) {
      if (streams[i] != NULL) {
        fn_lws_set_timeout(streams[i]->wsi, PENDING_TIMEOUT_CLOSE_SEND, LWS_TO_KILL_ASYNC);
        break;
      }
    }
  } else if (strcmp(line, "stop") == 0) {
    running = 0;
  }
}

int main(int argc, char **argv) {
  void *library;
  struct lws_context_creation_info info;
  struct lws_context *context;
  struct lws_protocols protocols[] = {
    {"fixture", callback, sizeof(fixture_stream), 4096, 0, NULL, 0},
    {NULL, NULL, 0, 0, 0, NULL, 0}
  };
  struct timespec started, now;
  char commands[128];
  size_t used = 0;
  int port, maximum_streams;
  if (argc != 3 && argc != 5) return 2;
  port = atoi(argv[1]);
  maximum_streams = atoi(argv[2]);
  if (port < 0 || port > 65535 || maximum_streams < 1 || maximum_streams > 64) return 2;
  library = dlopen("libwebsockets.so.21", RTLD_NOW | RTLD_LOCAL);
  if (library == NULL) return 3;
#define LOAD(name) do { \
  fn_##name = (__typeof__(fn_##name))dlsym(library, #name); \
  if (fn_##name == NULL) return 3; \
} while (0);
  LWS_FUNCTIONS(LOAD)
#undef LOAD
  fn_lws_set_log_level(LLL_ERR | LLL_WARN | LLL_NOTICE | LLL_INFO, fixture_log);
  memset(&info, 0, sizeof(info));
  info.port = port;
  info.iface = "127.0.0.1";
  info.vhost_name = "fixture";
  info.protocols = protocols;
  info.alpn = "h2,http/1.1";
  info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT | LWS_SERVER_OPTION_NO_LWS_SYSTEM_STATES;
  if (argc == 5) {
    info.ssl_cert_filepath = argv[3];
    info.ssl_private_key_filepath = argv[4];
  } else {
    info.options |= LWS_SERVER_OPTION_H2_PRIOR_KNOWLEDGE;
  }
  info.http2_settings[0] = 1;
  info.http2_settings[1] = 4096;
  info.http2_settings[2] = 0;
  info.http2_settings[3] = (uint32_t)maximum_streams;
  info.http2_settings[4] = 65535;
  info.http2_settings[5] = 16384;
  info.http2_settings[6] = 65536;
  context = fn_lws_create_context(&info);
  if (context == NULL) return 4;
  port = fn_lws_get_vhost_listen_port(fn_lws_get_vhost_by_name(context, "fixture"));
  printf("(ready %d)\n", port);
  fflush(stdout);
  fcntl(STDIN_FILENO, F_SETFL, fcntl(STDIN_FILENO, F_GETFL, 0) | O_NONBLOCK);
  clock_gettime(CLOCK_MONOTONIC, &started);
  while (running) {
    struct pollfd input = {STDIN_FILENO, POLLIN, 0};
    ssize_t count;
    fn_lws_cancel_service(context);
    if (fn_lws_service(context, 0) < 0) break;
    if (poll(&input, 1, 2) > 0) {
      count = read(STDIN_FILENO, commands + used, sizeof(commands) - used - 1);
      if (count == 0) break;
      if (count > 0) {
        char *newline;
        used += (size_t)count;
        commands[used] = 0;
        while ((newline = memchr(commands, '\n', used)) != NULL) {
          size_t consumed = (size_t)(newline - commands) + 1;
          *newline = 0;
          command(commands);
          used -= consumed;
          memmove(commands, commands + consumed, used);
          commands[used] = 0;
        }
        if (used == sizeof(commands) - 1) break;
      } else if (errno != EAGAIN && errno != EINTR) break;
    }
    clock_gettime(CLOCK_MONOTONIC, &now);
    if (now.tv_sec - started.tv_sec >= 20) break;
  }
  fn_lws_context_destroy(context);
  dlclose(library);
  return 0;
}
