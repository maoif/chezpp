#include "zlib_loader.h"

#include <pthread.h>
#include <stdio.h>
#include <zlib.h>

static const char *const zlib_names[] = {"libz.so.1", NULL};
static chezpp_optional_library zlib_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("zlib", zlib_names);
static pthread_once_t zlib_once = PTHREAD_ONCE_INIT;
static int zlib_available;

typedef const char *(*zlib_version_fn)(void);
typedef int (*zlib_deflate_init2_fn)(z_streamp, int, int, int, int, int,
                                     const char *, int);
typedef int (*zlib_deflate_fn)(z_streamp, int);
typedef int (*zlib_deflate_end_fn)(z_streamp);
typedef int (*zlib_inflate_init2_fn)(z_streamp, int, const char *, int);
typedef int (*zlib_inflate_fn)(z_streamp, int);
typedef int (*zlib_inflate_end_fn)(z_streamp);

typedef struct {
  z_stream stream;
  int compress;
  int closed;
  int ended;
} chezpp_zlib_stream;

static zlib_version_fn p_zlib_version;
static zlib_deflate_init2_fn p_deflate_init2;
static zlib_deflate_fn p_deflate;
static zlib_deflate_end_fn p_deflate_end;
static zlib_inflate_init2_fn p_inflate_init2;
static zlib_inflate_fn p_inflate;
static zlib_inflate_end_fn p_inflate_end;

static ptr zlib_status(const char *tag, const char *message) {
  ptr result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol(tag));
  Svector_set(result, 1, Sstring(message));
  return result;
}

static void initialize_zlib(void) {
  const char *version;
  unsigned major = 0, minor = 0, patch = 0;

  if (!chezpp_optional_library_open(&zlib_library)) return;
  if (!chezpp_optional_library_symbol(&zlib_library, "zlibVersion",
                                      (void **)&p_zlib_version) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflateInit2_",
                                      (void **)&p_deflate_init2) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflate", (void **)&p_deflate) ||
      !chezpp_optional_library_symbol(&zlib_library, "deflateEnd", (void **)&p_deflate_end) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflateInit2_",
                                      (void **)&p_inflate_init2) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflate", (void **)&p_inflate) ||
      !chezpp_optional_library_symbol(&zlib_library, "inflateEnd", (void **)&p_inflate_end))
    return;
  version = p_zlib_version();
  chezpp_optional_library_set_version(&zlib_library, version);
  if (version == NULL || sscanf(version, "%u.%u.%u", &major, &minor, &patch) < 2 ||
      major != 1 || minor < 2 || (minor == 2 && patch < 11)) {
    chezpp_optional_library_fail(
        &zlib_library, "zlib runtime %s; requires ABI 1 and version >= 1.2.11",
        version == NULL ? "unknown" : version);
    return;
  }
  zlib_available = 1;
}

int chezpp_zlib_require(void) {
  pthread_once(&zlib_once, initialize_zlib);
  return zlib_available;
}

const chezpp_optional_library *chezpp_zlib_library(void) {
  (void)chezpp_zlib_require();
  return &zlib_library;
}

static void close_stream(chezpp_zlib_stream *state) {
  if (state == NULL || state->closed) return;
  if (state->compress)
    (void)p_deflate_end(&state->stream);
  else
    (void)p_inflate_end(&state->stream);
  state->closed = 1;
}

uptr chezpp_zlib_stream_open(int compress, int gzip) {
  chezpp_zlib_stream *state;
  int rc;
  int window_bits = gzip ? 15 + 16 : 15;

  if (!chezpp_zlib_require()) return 0;
  state = (chezpp_zlib_stream *)calloc(1, sizeof(chezpp_zlib_stream));
  if (state == NULL) return 0;
  state->compress = compress ? 1 : 0;
  if (state->compress)
    rc = p_deflate_init2(&state->stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                         window_bits, 8, Z_DEFAULT_STRATEGY,
                         p_zlib_version(), (int)sizeof(z_stream));
  else
    rc = p_inflate_init2(&state->stream, window_bits,
                         p_zlib_version(), (int)sizeof(z_stream));
  if (rc != Z_OK) {
    free(state);
    return 0;
  }
  return (uptr)state;
}

ptr chezpp_zlib_stream_process(uptr handle, ptr input, int start, int stop,
                               int finish, int maximum_output) {
  chezpp_zlib_stream *state = (chezpp_zlib_stream *)TO_VOIDP(handle);
  unsigned char *output;
  size_t produced;
  int rc = Z_OK;
  ptr result;
  ptr bytes;

  if (state == NULL || state->closed)
    return zlib_status("error", "zlib stream is closed");
  if (start < 0 || stop < start || stop > Sbytevector_length(input) || maximum_output <= 0)
    return zlib_status("error", "invalid zlib stream bounds");
  if (state->ended) {
    if (stop != start)
      return zlib_status("error", "data follows the end of the zlib stream");
    bytes = Smake_bytevector(0, 0);
    result = Smake_vector(2, Sfalse);
    Svector_set(result, 0, bytes);
    Svector_set(result, 1, Strue);
    if (finish) close_stream(state);
    return result;
  }
  output = (unsigned char *)malloc((size_t)maximum_output);
  if (output == NULL) return zlib_status("error", "failed to allocate zlib output");
  state->stream.next_in = Sbytevector_data(input) + start;
  state->stream.avail_in = (uInt)(stop - start);
  state->stream.next_out = output;
  state->stream.avail_out = (uInt)maximum_output;
  do {
    uLong before_in = state->stream.total_in;
    uLong before_out = state->stream.total_out;
    rc = state->compress
             ? p_deflate(&state->stream, finish ? Z_FINISH : Z_NO_FLUSH)
             : p_inflate(&state->stream, finish ? Z_FINISH : Z_NO_FLUSH);
    if (rc != Z_OK && rc != Z_STREAM_END && rc != Z_BUF_ERROR) break;
    if (state->stream.avail_out == 0 && (state->stream.avail_in > 0 || finish)) {
      free(output);
      return zlib_status("limit", "zlib output exceeds configured limit");
    }
    if (state->stream.total_in == before_in && state->stream.total_out == before_out) break;
  } while (state->stream.avail_in > 0 || (finish && rc != Z_STREAM_END));
  if (rc != Z_OK && rc != Z_STREAM_END && !(rc == Z_BUF_ERROR && !finish)) {
    char message[256];
    snprintf(message, sizeof(message), "%s",
             state->stream.msg == NULL ? "zlib stream processing failed"
                                       : state->stream.msg);
    free(output);
    close_stream(state);
    return zlib_status("error", message);
  }
  if (rc == Z_STREAM_END && state->stream.avail_in > 0) {
    free(output);
    close_stream(state);
    return zlib_status("error", "data follows the end of the zlib stream");
  }
  if (rc == Z_STREAM_END) state->ended = 1;
  produced = (size_t)maximum_output - state->stream.avail_out;
  bytes = Smake_bytevector((iptr)produced, 0);
  if (produced > 0) memcpy(Sbytevector_data(bytes), output, produced);
  free(output);
  result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, bytes);
  Svector_set(result, 1, rc == Z_STREAM_END ? Strue : Sfalse);
  if (finish) close_stream(state);
  return result;
}

void chezpp_zlib_stream_close(uptr handle) {
  chezpp_zlib_stream *state = (chezpp_zlib_stream *)TO_VOIDP(handle);
  if (state == NULL) return;
  close_stream(state);
  free(state);
}
