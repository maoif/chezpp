#include "../common.h"
#include "../nghttp2_loader.h"

#include <nghttp2/nghttp2.h>
#include <stdlib.h>
#include <string.h>

typedef int (*callbacks_new_fn)(nghttp2_session_callbacks **);
typedef void (*callbacks_del_fn)(nghttp2_session_callbacks *);
typedef int (*session_new_fn)(nghttp2_session **, const nghttp2_session_callbacks *, void *);
typedef void (*session_del_fn)(nghttp2_session *);
typedef void (*set_user_data_fn)(nghttp2_session *, void *);
typedef void (*set_header_cb_fn)(nghttp2_session_callbacks *, nghttp2_on_header_callback);
typedef void (*set_data_cb_fn)(nghttp2_session_callbacks *, nghttp2_on_data_chunk_recv_callback);
typedef void (*set_frame_cb_fn)(nghttp2_session_callbacks *, nghttp2_on_frame_recv_callback);
typedef void (*set_close_cb_fn)(nghttp2_session_callbacks *, nghttp2_on_stream_close_callback);
typedef int32_t (*submit_request_fn)(nghttp2_session *, const nghttp2_priority_spec *,
                                    const nghttp2_nv *, size_t, const nghttp2_data_provider *,
                                    void *);
typedef int (*submit_response_fn)(nghttp2_session *, int32_t, const nghttp2_nv *, size_t,
                                  const nghttp2_data_provider *);
typedef int (*submit_settings_fn)(nghttp2_session *, uint8_t, const nghttp2_settings_entry *,
                                  size_t);
typedef ssize_t (*mem_send_fn)(nghttp2_session *, const uint8_t **);
typedef ssize_t (*mem_recv_fn)(nghttp2_session *, const uint8_t *, size_t);
typedef int (*consume_fn)(nghttp2_session *, int32_t, size_t);
typedef int (*rst_fn)(nghttp2_session *, uint8_t, int32_t, uint32_t);
typedef int (*goaway_fn)(nghttp2_session *, uint8_t, int32_t, uint32_t, const uint8_t *, size_t);
typedef int (*want_fn)(nghttp2_session *);

typedef struct h2_event h2_event;
typedef struct h2_stream h2_stream;
typedef struct h2_session h2_session;

struct h2_event {
  int type;
  int32_t stream_id;
  uint32_t flags;
  unsigned char *data;
  size_t data_len;
  h2_event *next;
};

struct h2_stream {
  int32_t id;
  unsigned char *body;
  size_t body_len;
  size_t body_offset;
  h2_stream *next;
};

struct h2_session {
  nghttp2_session *session;
  nghttp2_session_callbacks *callbacks;
  h2_stream *streams;
  h2_event *events;
  h2_event *events_tail;
};

static callbacks_new_fn p_callbacks_new;
static callbacks_del_fn p_callbacks_del;
static session_new_fn p_client_new;
static session_new_fn p_server_new;
static session_del_fn p_session_del;
static set_user_data_fn p_set_user_data;
static set_header_cb_fn p_set_header_cb;
static set_data_cb_fn p_set_data_cb;
static set_frame_cb_fn p_set_frame_cb;
static set_close_cb_fn p_set_close_cb;
static submit_request_fn p_submit_request;
static submit_response_fn p_submit_response;
static submit_settings_fn p_submit_settings;
static mem_send_fn p_mem_send;
static mem_recv_fn p_mem_recv;
static consume_fn p_consume;
static rst_fn p_rst;
static goaway_fn p_goaway;
static want_fn p_want_read;
static want_fn p_want_write;
static int h2_symbols_loaded;
static set_user_data_fn p_session_set_user_data;
static set_header_cb_fn p_session_callbacks_set_on_header_callback;
static set_data_cb_fn p_session_callbacks_set_on_data_chunk_recv_callback;
static set_frame_cb_fn p_session_callbacks_set_on_frame_recv_callback;
static set_close_cb_fn p_session_callbacks_set_on_stream_close_callback;
static mem_send_fn p_session_mem_send;
static mem_recv_fn p_session_mem_recv;
static consume_fn p_session_consume;
static rst_fn p_submit_rst_stream;
static goaway_fn p_submit_goaway;
static want_fn p_session_want_read;
static want_fn p_session_want_write;

static int load_h2_symbols(void) {
  if (h2_symbols_loaded) return 1;
#define LOAD(name, type) \
  do { *(void **)(&p_##name) = chezpp_nghttp2_symbol("nghttp2_" #name); \
       if (p_##name == NULL) return 0; } while (0)
  *(void **)(&p_callbacks_new) = chezpp_nghttp2_symbol("nghttp2_session_callbacks_new");
  *(void **)(&p_callbacks_del) = chezpp_nghttp2_symbol("nghttp2_session_callbacks_del");
  if (p_callbacks_new == NULL || p_callbacks_del == NULL) return 0;
  *(void **)(&p_client_new) = chezpp_nghttp2_symbol("nghttp2_session_client_new");
  *(void **)(&p_server_new) = chezpp_nghttp2_symbol("nghttp2_session_server_new");
  if (p_client_new == NULL || p_server_new == NULL) return 0;
  LOAD(session_del, session_del_fn);
  LOAD(session_set_user_data, set_user_data_fn);
  LOAD(session_callbacks_set_on_header_callback, set_header_cb_fn);
  LOAD(session_callbacks_set_on_data_chunk_recv_callback, set_data_cb_fn);
  LOAD(session_callbacks_set_on_frame_recv_callback, set_frame_cb_fn);
  LOAD(session_callbacks_set_on_stream_close_callback, set_close_cb_fn);
  LOAD(submit_request, submit_request_fn);
  LOAD(submit_response, submit_response_fn);
  LOAD(submit_settings, submit_settings_fn);
  LOAD(session_mem_send, mem_send_fn);
  LOAD(session_mem_recv, mem_recv_fn);
  LOAD(session_consume, consume_fn);
  LOAD(submit_rst_stream, rst_fn);
  LOAD(submit_goaway, goaway_fn);
  LOAD(session_want_read, want_fn);
  LOAD(session_want_write, want_fn);
#undef LOAD
  p_set_user_data = p_session_set_user_data;
  p_set_header_cb = p_session_callbacks_set_on_header_callback;
  p_set_data_cb = p_session_callbacks_set_on_data_chunk_recv_callback;
  p_set_frame_cb = p_session_callbacks_set_on_frame_recv_callback;
  p_set_close_cb = p_session_callbacks_set_on_stream_close_callback;
  p_mem_send = p_session_mem_send;
  p_mem_recv = p_session_mem_recv;
  p_consume = p_session_consume;
  p_rst = p_submit_rst_stream;
  p_goaway = p_submit_goaway;
  p_want_read = p_session_want_read;
  p_want_write = p_session_want_write;
  h2_symbols_loaded = 1;
  return 1;
}

static ptr h2_error(const char *message) {
  ptr result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol("error"));
  Svector_set(result, 1, Sstring(message));
  return result;
}

static void free_event(h2_event *event) {
  if (event != NULL) { free(event->data); free(event); }
}

static void push_event(h2_session *state, int type, int32_t stream_id, uint32_t flags,
                       const uint8_t *data, size_t data_len) {
  h2_event *event = (h2_event *)calloc(1, sizeof(*event));
  if (event == NULL) return;
  event->type = type;
  event->stream_id = stream_id;
  event->flags = flags;
  if (data_len != 0) {
    event->data = (unsigned char *)malloc(data_len);
    if (event->data == NULL) { free(event); return; }
    memcpy(event->data, data, data_len);
    event->data_len = data_len;
  }
  if (state->events_tail == NULL) state->events = event;
  else state->events_tail->next = event;
  state->events_tail = event;
}

static int on_header(nghttp2_session *session, const nghttp2_frame *frame,
                     const uint8_t *name, size_t namelen, const uint8_t *value,
                     size_t valuelen, uint8_t flags, void *user_data) {
  h2_session *state = (h2_session *)user_data;
  size_t len = namelen + 1 + valuelen;
  unsigned char *joined = (unsigned char *)malloc(len);
  (void)session;
  (void)flags;
  if (joined == NULL) return NGHTTP2_ERR_TEMPORAL_CALLBACK_FAILURE;
  memcpy(joined, name, namelen);
  joined[namelen] = 0;
  memcpy(joined + namelen + 1, value, valuelen);
  push_event(state, 1, frame->hd.stream_id, frame->hd.flags, joined, len);
  free(joined);
  return 0;
}

static int on_data(nghttp2_session *session, uint8_t flags, int32_t stream_id,
                   const uint8_t *data, size_t len, void *user_data) {
  (void)session;
  push_event((h2_session *)user_data, 2, stream_id, flags, data, len);
  return 0;
}

static int on_frame(nghttp2_session *session, const nghttp2_frame *frame, void *user_data) {
  h2_session *state = (h2_session *)user_data;
  (void)session;
  if (frame->hd.type == NGHTTP2_HEADERS || frame->hd.type == NGHTTP2_DATA)
    push_event(state, 3, frame->hd.stream_id, frame->hd.flags, NULL, 0);
  return 0;
}

static int on_close(nghttp2_session *session, int32_t stream_id, uint32_t error_code,
                    void *user_data) {
  (void)session;
  push_event((h2_session *)user_data, 4, stream_id, error_code, NULL, 0);
  return 0;
}

static ssize_t read_body(nghttp2_session *session, int32_t stream_id, uint8_t *buf,
                         size_t length, uint32_t *data_flags,
                         nghttp2_data_source *source, void *user_data) {
  h2_stream *stream = (h2_stream *)source->ptr;
  size_t remaining = stream->body_len - stream->body_offset;
  size_t count = remaining < length ? remaining : length;
  (void)session;
  (void)stream_id;
  (void)user_data;
  if (count != 0) memcpy(buf, stream->body + stream->body_offset, count);
  stream->body_offset += count;
  if (stream->body_offset == stream->body_len) *data_flags |= NGHTTP2_DATA_FLAG_EOF;
  return (ssize_t)count;
}

static nghttp2_nv *make_headers(ptr headers, size_t *count) {
  size_t i, n = Svector_length(headers);
  nghttp2_nv *out;
  *count = n;
  out = (nghttp2_nv *)calloc(*count, sizeof(*out));
  if (out == NULL) return NULL;
  for (i = 0; i < n; i++) {
    ptr pair = Svector_ref(headers, i);
    ptr name = Svector_ref(pair, 0), value = Svector_ref(pair, 1);
    iptr nl = Sstring_length(name), vl = Sstring_length(value);
    char *nb = (char *)malloc((size_t)nl + 1), *vb = (char *)malloc((size_t)vl + 1);
    iptr j;
    if (nb == NULL || vb == NULL) { free(nb); free(vb); return out; }
    for (j = 0; j < nl; j++) nb[j] = (char)Sstring_ref(name, j);
    for (j = 0; j < vl; j++) vb[j] = (char)Sstring_ref(value, j);
    nb[nl] = 0; vb[vl] = 0;
    out[i].name = (uint8_t *)nb;
    out[i].value = (uint8_t *)vb;
    out[i].namelen = (size_t)nl;
    out[i].valuelen = (size_t)vl;
  }
  return out;
}

static void free_headers(nghttp2_nv *headers, size_t count) {
  size_t i;
  if (headers == NULL) return;
  for (i = 0; i < count; i++) { free(headers[i].name); free(headers[i].value); }
  free(headers);
}

ptr chezpp_net_http2_open(int server) {
  h2_session *state;
  if (!load_h2_symbols()) return h2_error("nghttp2 adapter symbols are unavailable");
  state = (h2_session *)calloc(1, sizeof(*state));
  if (state == NULL) return h2_error("out of memory");
  if (p_callbacks_new(&state->callbacks) != 0) { free(state); return h2_error("failed to create nghttp2 callbacks"); }
  p_set_header_cb(state->callbacks, on_header);
  p_set_data_cb(state->callbacks, on_data);
  p_set_frame_cb(state->callbacks, on_frame);
  p_set_close_cb(state->callbacks, on_close);
  if ((server ? p_server_new : p_client_new)(&state->session, state->callbacks, state) != 0) {
    p_callbacks_del(state->callbacks); free(state); return h2_error("failed to create nghttp2 session");
  }
  p_set_user_data(state->session, state);
  if (p_submit_settings(state->session, NGHTTP2_FLAG_NONE, NULL, 0) != 0) {
    p_session_del(state->session);
    p_callbacks_del(state->callbacks);
    free(state);
    return h2_error("failed to submit initial HTTP/2 settings");
  }
  return Sunsigned((uptr)state);
}

ptr chezpp_net_http2_close(uptr handle) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  h2_stream *stream;
  h2_event *event;
  if (state == NULL) return Strue;
  p_session_del(state->session); p_callbacks_del(state->callbacks);
  while ((stream = state->streams) != NULL) { state->streams = stream->next; free(stream->body); free(stream); }
  while ((event = state->events) != NULL) { state->events = event->next; free_event(event); }
  free(state);
  return Strue;
}

ptr chezpp_net_http2_submit_request(uptr handle, ptr headers, ptr body) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  h2_stream *stream;
  nghttp2_nv *nva;
  nghttp2_data_provider provider;
  size_t count;
  int32_t id;
  if (state == NULL || !Svectorp(headers) || !Sbytevectorp(body)) return h2_error("invalid HTTP/2 request");
  stream = (h2_stream *)calloc(1, sizeof(*stream));
  if (stream == NULL) return h2_error("out of memory");
  stream->body_len = (size_t)Sbytevector_length(body);
  stream->body = (unsigned char *)malloc(stream->body_len == 0 ? 1 : stream->body_len);
  if (stream->body_len != 0) memcpy(stream->body, Sbytevector_data(body), stream->body_len);
  nva = make_headers(headers, &count);
  if (nva == NULL) { free(stream->body); free(stream); return h2_error("out of memory"); }
  memset(&provider, 0, sizeof(provider)); provider.source.ptr = stream; provider.read_callback = read_body;
  id = p_submit_request(state->session, NULL, nva, count, stream->body_len ? &provider : NULL, stream);
  free_headers(nva, count);
  if (id < 0) { free(stream->body); free(stream); return h2_error("failed to submit HTTP/2 request"); }
  stream->id = id; stream->next = state->streams; state->streams = stream;
  return Sinteger(id);
}

ptr chezpp_net_http2_submit_response(uptr handle, int stream_id, ptr headers, ptr body) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  nghttp2_nv *nva; nghttp2_data_provider provider; size_t count; int rc;
  h2_stream *stream;
  if (state == NULL || !Svectorp(headers) || !Sbytevectorp(body)) return h2_error("invalid HTTP/2 response");
  stream = (h2_stream *)calloc(1, sizeof(*stream));
  if (stream == NULL) return h2_error("out of memory");
  stream->id = stream_id; stream->body_len = (size_t)Sbytevector_length(body);
  stream->body = (unsigned char *)malloc(stream->body_len == 0 ? 1 : stream->body_len);
  if (stream->body_len != 0) memcpy(stream->body, Sbytevector_data(body), stream->body_len);
  nva = make_headers(headers, &count); memset(&provider, 0, sizeof(provider));
  provider.source.ptr = stream; provider.read_callback = read_body;
  rc = p_submit_response(state->session, stream_id, nva, count, stream->body_len ? &provider : NULL);
  free_headers(nva, count);
  if (rc != 0) { free(stream->body); free(stream); return h2_error("failed to submit HTTP/2 response"); }
  stream->next = state->streams; state->streams = stream;
  return Strue;
}

ptr chezpp_net_http2_mem_send(uptr handle) {
  h2_session *state = (h2_session *)TO_VOIDP(handle); const uint8_t *data; ssize_t n;
  if (state == NULL) return h2_error("invalid HTTP/2 session");
  n = p_mem_send(state->session, &data);
  if (n < 0) return h2_error("HTTP/2 session send failed");
  if (n == 0) return Sfalse;
  { ptr out = Smake_bytevector((iptr)n, 0); memcpy(Sbytevector_data(out), data, (size_t)n); return out; }
}

ptr chezpp_net_http2_mem_recv(uptr handle, ptr bytes, int start, int stop) {
  h2_session *state = (h2_session *)TO_VOIDP(handle); ssize_t n;
  if (state == NULL || !Sbytevectorp(bytes) || start < 0 || stop < start || stop > Sbytevector_length(bytes))
    return h2_error("invalid HTTP/2 input");
  n = p_mem_recv(state->session, Sbytevector_data(bytes) + start, (size_t)(stop - start));
  return n < 0 ? h2_error("HTTP/2 session receive failed") : Sinteger(n);
}

ptr chezpp_net_http2_next_event(uptr handle) {
  h2_session *state = (h2_session *)TO_VOIDP(handle); h2_event *event; ptr out;
  if (state == NULL || (event = state->events) == NULL) return Sfalse;
  state->events = event->next; if (state->events == NULL) state->events_tail = NULL;
  out = Smake_vector(4, Sfalse); Svector_set(out, 0, Sinteger(event->type));
  Svector_set(out, 1, Sinteger(event->stream_id)); Svector_set(out, 2, Sinteger(event->flags));
  if (event->type == 1) {
    size_t split = 0;
    ptr pair = Smake_vector(2, Sfalse);
    while (split < event->data_len && event->data[split] != 0) split++;
    Svector_set(pair, 0, Sstring_utf8((char *)event->data, (iptr)split));
    Svector_set(pair, 1,
                Sstring_utf8((char *)event->data + (split < event->data_len ? split + 1 : split),
                             (iptr)(event->data_len - (split < event->data_len ? split + 1 : split))));
    Svector_set(out, 3, pair);
  }
  else {
    ptr bytes = Smake_bytevector((iptr)event->data_len, 0);
    if (event->data_len != 0) memcpy(Sbytevector_data(bytes), event->data, event->data_len);
    Svector_set(out, 3, bytes);
  }
  free_event(event); return out;
}

ptr chezpp_net_http2_consume(uptr handle, int stream_id, int count) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  return state == NULL ? h2_error("invalid HTTP/2 session") : Sinteger(p_consume(state->session, stream_id, (size_t)count));
}

ptr chezpp_net_http2_rst(uptr handle, int stream_id, int error_code) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  return state == NULL ? h2_error("invalid HTTP/2 session") : Sinteger(p_rst(state->session, 0, stream_id, (uint32_t)error_code));
}

ptr chezpp_net_http2_goaway(uptr handle, int stream_id, int error_code) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  return state == NULL ? h2_error("invalid HTTP/2 session") : Sinteger(p_goaway(state->session, NGHTTP2_FLAG_NONE, stream_id, (uint32_t)error_code, NULL, 0));
}

ptr chezpp_net_http2_want_read(uptr handle) { h2_session *s = (h2_session *)TO_VOIDP(handle); return s ? (p_want_read(s->session) ? Strue : Sfalse) : Sfalse; }
ptr chezpp_net_http2_want_write(uptr handle) { h2_session *s = (h2_session *)TO_VOIDP(handle); return s ? (p_want_write(s->session) ? Strue : Sfalse) : Sfalse; }
