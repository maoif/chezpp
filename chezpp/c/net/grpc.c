#include "../common.h"

#include <dlfcn.h>

#include <grpc/grpc.h>
#include <grpc/byte_buffer.h>
#include <grpc/byte_buffer_reader.h>
#include <grpc/credentials.h>
#include <grpc/slice.h>
#include <grpc/support/alloc.h>
#include <grpc/support/time.h>

typedef struct chezpp_grpc_server chezpp_grpc_server;
typedef struct chezpp_grpc_server_call chezpp_grpc_server_call;
typedef struct chezpp_grpc_unary_call chezpp_grpc_unary_call;

struct chezpp_grpc_server {
  grpc_server *server;
  grpc_completion_queue *cq;
  int closed;
  int port;
};

struct chezpp_grpc_server_call {
  chezpp_grpc_server *server;
  grpc_call *call;
  grpc_call_details details;
  grpc_metadata_array request_metadata;
  grpc_byte_buffer *request_payload;
  int cancelled;
};

struct chezpp_grpc_unary_call {
  grpc_completion_queue *cq;
  grpc_call *call;
  grpc_metadata *send_metadata;
  size_t send_metadata_count;
  grpc_metadata_array recv_initial_metadata;
  grpc_metadata_array recv_trailing_metadata;
  grpc_byte_buffer *recv_message;
  grpc_status_code status;
  grpc_slice status_details;
  const char *error_string;
};

typedef void (*grpc_init_fn)(void);
typedef void (*grpc_shutdown_fn)(void);
typedef grpc_completion_queue *(*grpc_completion_queue_create_for_pluck_fn)(void *);
typedef grpc_event (*grpc_completion_queue_pluck_fn)(grpc_completion_queue *, void *,
                                                     gpr_timespec, void *);
typedef grpc_event (*grpc_completion_queue_next_fn)(grpc_completion_queue *, gpr_timespec, void *);
typedef void (*grpc_completion_queue_shutdown_fn)(grpc_completion_queue *);
typedef void (*grpc_completion_queue_destroy_fn)(grpc_completion_queue *);
typedef grpc_channel_credentials *(*grpc_insecure_credentials_create_fn)(void);
typedef grpc_server_credentials *(*grpc_insecure_server_credentials_create_fn)(void);
typedef void (*grpc_channel_credentials_release_fn)(grpc_channel_credentials *);
typedef void (*grpc_server_credentials_release_fn)(grpc_server_credentials *);
typedef grpc_channel *(*grpc_channel_create_fn)(const char *, grpc_channel_credentials *,
                                                const grpc_channel_args *);
typedef void (*grpc_channel_destroy_fn)(grpc_channel *);
typedef grpc_call *(*grpc_channel_create_call_fn)(grpc_channel *, grpc_call *, uint32_t,
                                                  grpc_completion_queue *, grpc_slice,
                                                  const grpc_slice *, gpr_timespec, void *);
typedef grpc_call_error (*grpc_call_start_batch_fn)(grpc_call *, const grpc_op *, size_t, void *,
                                                    void *);
typedef grpc_call_error (*grpc_call_cancel_fn)(grpc_call *, void *);
typedef void (*grpc_call_unref_fn)(grpc_call *);
typedef const char *(*grpc_call_error_to_string_fn)(grpc_call_error);
typedef grpc_server *(*grpc_server_create_fn)(const grpc_channel_args *, void *);
typedef void (*grpc_server_register_completion_queue_fn)(grpc_server *, grpc_completion_queue *,
                                                         void *);
typedef grpc_call_error (*grpc_server_request_call_fn)(grpc_server *, grpc_call **,
                                                       grpc_call_details *,
                                                       grpc_metadata_array *,
                                                       grpc_completion_queue *,
                                                       grpc_completion_queue *, void *);
typedef int (*grpc_server_add_http2_port_fn)(grpc_server *, const char *,
                                             grpc_server_credentials *);
typedef void (*grpc_server_start_fn)(grpc_server *);
typedef void (*grpc_server_shutdown_and_notify_fn)(grpc_server *, grpc_completion_queue *, void *);
typedef void (*grpc_server_destroy_fn)(grpc_server *);
typedef void (*grpc_call_details_init_fn)(grpc_call_details *);
typedef void (*grpc_call_details_destroy_fn)(grpc_call_details *);
typedef void (*grpc_metadata_array_init_fn)(grpc_metadata_array *);
typedef void (*grpc_metadata_array_destroy_fn)(grpc_metadata_array *);
typedef grpc_byte_buffer *(*grpc_raw_byte_buffer_create_fn)(grpc_slice *, size_t);
typedef void (*grpc_byte_buffer_destroy_fn)(grpc_byte_buffer *);
typedef int (*grpc_byte_buffer_reader_init_fn)(grpc_byte_buffer_reader *, grpc_byte_buffer *);
typedef void (*grpc_byte_buffer_reader_destroy_fn)(grpc_byte_buffer_reader *);
typedef grpc_slice (*grpc_byte_buffer_reader_readall_fn)(grpc_byte_buffer_reader *);
typedef grpc_slice (*grpc_slice_from_copied_buffer_fn)(const char *, size_t);
typedef grpc_slice (*grpc_slice_from_copied_string_fn)(const char *);
typedef grpc_slice (*grpc_empty_slice_fn)(void);
typedef void (*grpc_slice_unref_fn)(grpc_slice);
typedef gpr_timespec (*gpr_inf_future_fn)(gpr_clock_type);
typedef gpr_timespec (*gpr_time_from_millis_fn)(int64_t, gpr_clock_type);
typedef gpr_timespec (*gpr_now_fn)(gpr_clock_type);
typedef gpr_timespec (*gpr_time_add_fn)(gpr_timespec, gpr_timespec);
typedef void (*gpr_free_fn)(void *);

static void *grpc_handle = NULL;
static void *gpr_handle = NULL;

static grpc_init_fn p_grpc_init = NULL;
static grpc_shutdown_fn p_grpc_shutdown = NULL;
static grpc_completion_queue_create_for_pluck_fn p_grpc_completion_queue_create_for_pluck = NULL;
static grpc_completion_queue_pluck_fn p_grpc_completion_queue_pluck = NULL;
static grpc_completion_queue_next_fn p_grpc_completion_queue_next = NULL;
static grpc_completion_queue_shutdown_fn p_grpc_completion_queue_shutdown = NULL;
static grpc_completion_queue_destroy_fn p_grpc_completion_queue_destroy = NULL;
static grpc_insecure_credentials_create_fn p_grpc_insecure_credentials_create = NULL;
static grpc_insecure_server_credentials_create_fn p_grpc_insecure_server_credentials_create = NULL;
static grpc_channel_credentials_release_fn p_grpc_channel_credentials_release = NULL;
static grpc_server_credentials_release_fn p_grpc_server_credentials_release = NULL;
static grpc_channel_create_fn p_grpc_channel_create = NULL;
static grpc_channel_destroy_fn p_grpc_channel_destroy = NULL;
static grpc_channel_create_call_fn p_grpc_channel_create_call = NULL;
static grpc_call_start_batch_fn p_grpc_call_start_batch = NULL;
static grpc_call_cancel_fn p_grpc_call_cancel = NULL;
static grpc_call_unref_fn p_grpc_call_unref = NULL;
static grpc_call_error_to_string_fn p_grpc_call_error_to_string = NULL;
static grpc_server_create_fn p_grpc_server_create = NULL;
static grpc_server_register_completion_queue_fn p_grpc_server_register_completion_queue = NULL;
static grpc_server_request_call_fn p_grpc_server_request_call = NULL;
static grpc_server_add_http2_port_fn p_grpc_server_add_http2_port = NULL;
static grpc_server_start_fn p_grpc_server_start = NULL;
static grpc_server_shutdown_and_notify_fn p_grpc_server_shutdown_and_notify = NULL;
static grpc_server_destroy_fn p_grpc_server_destroy = NULL;
static grpc_call_details_init_fn p_grpc_call_details_init = NULL;
static grpc_call_details_destroy_fn p_grpc_call_details_destroy = NULL;
static grpc_metadata_array_init_fn p_grpc_metadata_array_init = NULL;
static grpc_metadata_array_destroy_fn p_grpc_metadata_array_destroy = NULL;
static grpc_raw_byte_buffer_create_fn p_grpc_raw_byte_buffer_create = NULL;
static grpc_byte_buffer_destroy_fn p_grpc_byte_buffer_destroy = NULL;
static grpc_byte_buffer_reader_init_fn p_grpc_byte_buffer_reader_init = NULL;
static grpc_byte_buffer_reader_destroy_fn p_grpc_byte_buffer_reader_destroy = NULL;
static grpc_byte_buffer_reader_readall_fn p_grpc_byte_buffer_reader_readall = NULL;
static grpc_slice_from_copied_buffer_fn p_grpc_slice_from_copied_buffer = NULL;
static grpc_slice_from_copied_string_fn p_grpc_slice_from_copied_string = NULL;
static grpc_empty_slice_fn p_grpc_empty_slice = NULL;
static grpc_slice_unref_fn p_grpc_slice_unref = NULL;
static gpr_inf_future_fn p_gpr_inf_future = NULL;
static gpr_time_from_millis_fn p_gpr_time_from_millis = NULL;
static gpr_now_fn p_gpr_now = NULL;
static gpr_time_add_fn p_gpr_time_add = NULL;
static gpr_free_fn p_gpr_free = NULL;

static int grpc_initialized = 0;

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) {
  return make_status("error", msg == NULL ? Sstring("gRPC error") : Sstring(msg));
}

static ptr make_handle(uptr handle) { return Sunsigned(handle); }

static int load_symbol(void **out, void *handle, const char *name) {
  *out = dlsym(handle, name);
  return *out != NULL;
}

static int ensure_grpc_loaded(void) {
  const char *grpc_names[] = {"libgrpc.so.52", "libgrpc.so", NULL};
  const char *gpr_names[] = {"libgpr.so.52", "libgpr.so", NULL};
  int i;

  if (grpc_handle != NULL && gpr_handle != NULL) return 1;

  for (i = 0; grpc_names[i] != NULL; i += 1) {
    grpc_handle = dlopen(grpc_names[i], RTLD_NOW | RTLD_LOCAL);
    if (grpc_handle != NULL) break;
  }
  if (grpc_handle == NULL) return 0;

  for (i = 0; gpr_names[i] != NULL; i += 1) {
    gpr_handle = dlopen(gpr_names[i], RTLD_NOW | RTLD_LOCAL);
    if (gpr_handle != NULL) break;
  }
  if (gpr_handle == NULL) {
    dlclose(grpc_handle);
    grpc_handle = NULL;
    return 0;
  }

  if (!load_symbol((void **)&p_grpc_init, grpc_handle, "grpc_init") ||
      !load_symbol((void **)&p_grpc_shutdown, grpc_handle, "grpc_shutdown") ||
      !load_symbol((void **)&p_grpc_completion_queue_create_for_pluck, grpc_handle,
                   "grpc_completion_queue_create_for_pluck") ||
      !load_symbol((void **)&p_grpc_completion_queue_pluck, grpc_handle,
                   "grpc_completion_queue_pluck") ||
      !load_symbol((void **)&p_grpc_completion_queue_next, grpc_handle,
                   "grpc_completion_queue_next") ||
      !load_symbol((void **)&p_grpc_completion_queue_shutdown, grpc_handle,
                   "grpc_completion_queue_shutdown") ||
      !load_symbol((void **)&p_grpc_completion_queue_destroy, grpc_handle,
                   "grpc_completion_queue_destroy") ||
      !load_symbol((void **)&p_grpc_insecure_credentials_create, grpc_handle,
                   "grpc_insecure_credentials_create") ||
      !load_symbol((void **)&p_grpc_insecure_server_credentials_create, grpc_handle,
                   "grpc_insecure_server_credentials_create") ||
      !load_symbol((void **)&p_grpc_channel_credentials_release, grpc_handle,
                   "grpc_channel_credentials_release") ||
      !load_symbol((void **)&p_grpc_server_credentials_release, grpc_handle,
                   "grpc_server_credentials_release") ||
      !load_symbol((void **)&p_grpc_channel_create, grpc_handle, "grpc_channel_create") ||
      !load_symbol((void **)&p_grpc_channel_destroy, grpc_handle, "grpc_channel_destroy") ||
      !load_symbol((void **)&p_grpc_channel_create_call, grpc_handle, "grpc_channel_create_call") ||
      !load_symbol((void **)&p_grpc_call_start_batch, grpc_handle, "grpc_call_start_batch") ||
      !load_symbol((void **)&p_grpc_call_cancel, grpc_handle, "grpc_call_cancel") ||
      !load_symbol((void **)&p_grpc_call_unref, grpc_handle, "grpc_call_unref") ||
      !load_symbol((void **)&p_grpc_call_error_to_string, grpc_handle,
                   "grpc_call_error_to_string") ||
      !load_symbol((void **)&p_grpc_server_create, grpc_handle, "grpc_server_create") ||
      !load_symbol((void **)&p_grpc_server_register_completion_queue, grpc_handle,
                   "grpc_server_register_completion_queue") ||
      !load_symbol((void **)&p_grpc_server_request_call, grpc_handle, "grpc_server_request_call") ||
      !load_symbol((void **)&p_grpc_server_add_http2_port, grpc_handle,
                   "grpc_server_add_http2_port") ||
      !load_symbol((void **)&p_grpc_server_start, grpc_handle, "grpc_server_start") ||
      !load_symbol((void **)&p_grpc_server_shutdown_and_notify, grpc_handle,
                   "grpc_server_shutdown_and_notify") ||
      !load_symbol((void **)&p_grpc_server_destroy, grpc_handle, "grpc_server_destroy") ||
      !load_symbol((void **)&p_grpc_call_details_init, grpc_handle, "grpc_call_details_init") ||
      !load_symbol((void **)&p_grpc_call_details_destroy, grpc_handle,
                   "grpc_call_details_destroy") ||
      !load_symbol((void **)&p_grpc_metadata_array_init, grpc_handle,
                   "grpc_metadata_array_init") ||
      !load_symbol((void **)&p_grpc_metadata_array_destroy, grpc_handle,
                   "grpc_metadata_array_destroy") ||
      !load_symbol((void **)&p_grpc_raw_byte_buffer_create, grpc_handle,
                   "grpc_raw_byte_buffer_create") ||
      !load_symbol((void **)&p_grpc_byte_buffer_destroy, grpc_handle,
                   "grpc_byte_buffer_destroy") ||
      !load_symbol((void **)&p_grpc_byte_buffer_reader_init, grpc_handle,
                   "grpc_byte_buffer_reader_init") ||
      !load_symbol((void **)&p_grpc_byte_buffer_reader_destroy, grpc_handle,
                   "grpc_byte_buffer_reader_destroy") ||
      !load_symbol((void **)&p_grpc_byte_buffer_reader_readall, grpc_handle,
                   "grpc_byte_buffer_reader_readall") ||
      !load_symbol((void **)&p_grpc_slice_from_copied_buffer, grpc_handle,
                   "grpc_slice_from_copied_buffer") ||
      !load_symbol((void **)&p_grpc_slice_from_copied_string, grpc_handle,
                   "grpc_slice_from_copied_string") ||
      !load_symbol((void **)&p_grpc_empty_slice, grpc_handle, "grpc_empty_slice") ||
      !load_symbol((void **)&p_grpc_slice_unref, grpc_handle, "grpc_slice_unref") ||
      !load_symbol((void **)&p_gpr_inf_future, gpr_handle, "gpr_inf_future") ||
      !load_symbol((void **)&p_gpr_time_from_millis, gpr_handle, "gpr_time_from_millis") ||
      !load_symbol((void **)&p_gpr_now, gpr_handle, "gpr_now") ||
      !load_symbol((void **)&p_gpr_time_add, gpr_handle, "gpr_time_add") ||
      !load_symbol((void **)&p_gpr_free, gpr_handle, "gpr_free")) {
    dlclose(gpr_handle);
    dlclose(grpc_handle);
    gpr_handle = NULL;
    grpc_handle = NULL;
    return 0;
  }

  if (!grpc_initialized) {
    p_grpc_init();
    grpc_initialized = 1;
  }

  return 1;
}

static gpr_timespec make_deadline(int timeout_ms) {
  if (timeout_ms <= 0) return p_gpr_inf_future(GPR_CLOCK_REALTIME);
  return p_gpr_time_add(p_gpr_now(GPR_CLOCK_REALTIME),
                        p_gpr_time_from_millis((int64_t)timeout_ms, GPR_TIMESPAN));
}

static gpr_timespec make_immediate_deadline(void) {
  return p_gpr_time_add(p_gpr_now(GPR_CLOCK_REALTIME),
                        p_gpr_time_from_millis(0, GPR_TIMESPAN));
}

static char *copy_scheme_string(ptr str) {
  iptr n;
  char *out;
  iptr i;
  if (!Sstringp(str)) return NULL;
  n = Sstring_length(str);
  out = (char *)malloc((size_t)n + 1);
  if (out == NULL) return NULL;
  for (i = 0; i < n; i += 1) {
    uint32_t c = (uint32_t)Sstring_ref(str, i);
    if (c > 255) {
      free(out);
      return NULL;
    }
    out[i] = (char)c;
  }
  out[n] = '\0';
  return out;
}

static ptr bytevector_from_slice(grpc_slice slice) {
  ptr out = Smake_bytevector((iptr)GRPC_SLICE_LENGTH(slice), 0);
  if (GRPC_SLICE_LENGTH(slice) > 0) {
    memcpy(Sbytevector_data(out), GRPC_SLICE_START_PTR(slice), GRPC_SLICE_LENGTH(slice));
  }
  return out;
}

static ptr maybe_bytevector_from_buffer(grpc_byte_buffer *buffer) {
  grpc_byte_buffer_reader reader;
  grpc_slice slice;
  ptr out;

  if (buffer == NULL) return Sfalse;
  if (!p_grpc_byte_buffer_reader_init(&reader, buffer)) return Sfalse;
  slice = p_grpc_byte_buffer_reader_readall(&reader);
  out = bytevector_from_slice(slice);
  p_grpc_slice_unref(slice);
  p_grpc_byte_buffer_reader_destroy(&reader);
  return out;
}

static ptr maybe_string_from_slice(grpc_slice slice) {
  size_t n = GRPC_SLICE_LENGTH(slice);
  char *buf = (char *)malloc(n + 1);
  ptr out;
  if (buf == NULL) return Sstring("");
  if (n > 0) memcpy(buf, GRPC_SLICE_START_PTR(slice), n);
  buf[n] = '\0';
  out = Sstring(buf);
  free(buf);
  return out;
}

static ptr metadata_array_to_scheme(const grpc_metadata_array *array) {
  ptr head = Snil;
  ptr tail = Snil;
  size_t i;
  for (i = 0; i < array->count; i += 1) {
    ptr key = maybe_string_from_slice(array->metadata[i].key);
    ptr value = bytevector_from_slice(array->metadata[i].value);
    ptr entry = Scons(key, value);
    ptr cell = Scons(entry, Snil);
    if (Snullp(head)) {
      head = cell;
      tail = cell;
    } else {
      Scdr(tail) = cell;
      tail = cell;
    }
  }
  return head;
}

static void free_metadata_array(grpc_metadata *metadata, size_t count) {
  size_t i;
  if (metadata == NULL) return;
  for (i = 0; i < count; i += 1) {
    p_grpc_slice_unref(metadata[i].key);
    p_grpc_slice_unref(metadata[i].value);
  }
  free(metadata);
}

static int scheme_metadata_to_grpc(ptr metadata_ls, grpc_metadata **out, size_t *count,
                                   const char **error_message) {
  size_t n = 0;
  ptr ls = metadata_ls;
  grpc_metadata *metadata;
  size_t i = 0;

  while (!Snullp(ls)) {
    if (!Spairp(ls)) {
      *error_message = "invalid gRPC metadata list";
      return 0;
    }
    n += 1;
    ls = Scdr(ls);
  }

  if (n == 0) {
    *out = NULL;
    *count = 0;
    return 1;
  }

  metadata = (grpc_metadata *)calloc(n, sizeof(grpc_metadata));
  if (metadata == NULL) {
    *error_message = "out of memory";
    return 0;
  }

  ls = metadata_ls;
  while (!Snullp(ls)) {
    ptr entry = Scar(ls);
    char *key_buf;
    if (!Spairp(entry) || !Sstringp(Scar(entry)) || !Sbytevectorp(Scdr(entry))) {
      free_metadata_array(metadata, i);
      *error_message = "metadata entries must be (string . bytevector) pairs";
      return 0;
    }
    key_buf = copy_scheme_string(Scar(entry));
    if (key_buf == NULL) {
      free_metadata_array(metadata, i);
      *error_message = "invalid gRPC metadata key";
      return 0;
    }
    metadata[i].key = p_grpc_slice_from_copied_string(key_buf);
    metadata[i].value = p_grpc_slice_from_copied_buffer((const char *)Sbytevector_data(Scdr(entry)),
                                                        (size_t)Sbytevector_length(Scdr(entry)));
    free(key_buf);
    i += 1;
    ls = Scdr(ls);
  }

  *out = metadata;
  *count = n;
  return 1;
}

static grpc_byte_buffer *make_request_buffer(ptr payload, int start, int stop) {
  grpc_slice slice;
  grpc_byte_buffer *buffer;
  if (payload == Sfalse) return NULL;
  slice = p_grpc_slice_from_copied_buffer((const char *)Sbytevector_data(payload) + start,
                                          (size_t)(stop - start));
  buffer = p_grpc_raw_byte_buffer_create(&slice, 1);
  p_grpc_slice_unref(slice);
  return buffer;
}

static ptr make_response_vector(ptr payload, ptr metadata, grpc_status_code status,
                                const char *message) {
  ptr out = Smake_vector(4, Sfalse);
  Svector_set(out, 0, payload);
  Svector_set(out, 1, metadata);
  Svector_set(out, 2, Sinteger((iptr)status));
  Svector_set(out, 3, Sstring(message == NULL ? "" : message));
  return out;
}

static ptr make_request_vector(uptr handle, ptr method, ptr payload, ptr metadata) {
  ptr out = Smake_vector(4, Sfalse);
  Svector_set(out, 0, make_handle(handle));
  Svector_set(out, 1, method);
  Svector_set(out, 2, payload);
  Svector_set(out, 3, metadata);
  return out;
}

static void cleanup_unary_call(chezpp_grpc_unary_call *op) {
  if (op == NULL) return;
  if (op->call != NULL && p_grpc_call_cancel != NULL) {
    (void)p_grpc_call_cancel(op->call, NULL);
  }
  if (op->recv_message != NULL) {
    p_grpc_byte_buffer_destroy(op->recv_message);
    op->recv_message = NULL;
  }
  p_grpc_metadata_array_destroy(&op->recv_initial_metadata);
  p_grpc_metadata_array_destroy(&op->recv_trailing_metadata);
  if (GRPC_SLICE_LENGTH(op->status_details) > 0) {
    p_grpc_slice_unref(op->status_details);
  }
  if (op->error_string != NULL) {
    p_gpr_free((void *)op->error_string);
    op->error_string = NULL;
  }
  free_metadata_array(op->send_metadata, op->send_metadata_count);
  op->send_metadata = NULL;
  op->send_metadata_count = 0;
  if (op->call != NULL) {
    p_grpc_call_unref(op->call);
    op->call = NULL;
  }
  if (op->cq != NULL) {
    p_grpc_completion_queue_shutdown(op->cq);
    p_grpc_completion_queue_destroy(op->cq);
    op->cq = NULL;
  }
  free(op);
}

static ptr finalize_unary_call(chezpp_grpc_unary_call *op) {
  ptr payload = maybe_bytevector_from_buffer(op->recv_message);
  ptr metadata = metadata_array_to_scheme(&op->recv_trailing_metadata);
  ptr message = Sstring("");
  ptr response;

  if (GRPC_SLICE_LENGTH(op->status_details) > 0) {
    message = maybe_string_from_slice(op->status_details);
  } else if (op->error_string != NULL) {
    message = Sstring(op->error_string);
  }

  response = make_response_vector(payload, metadata, op->status, "");
  Svector_set(response, 3, message);
  cleanup_unary_call(op);
  return response;
}

static ptr make_call_error_status(grpc_call_error error) {
  const char *msg = p_grpc_call_error_to_string == NULL ? NULL : p_grpc_call_error_to_string(error);
  return make_error_status_message(msg == NULL ? "gRPC call error" : msg);
}

static ptr start_unary_call(grpc_channel *channel, const char *method, ptr payload, int start,
                            int stop, ptr metadata_ls, int timeout_ms, int blocking) {
  grpc_completion_queue *cq;
  grpc_slice method_slice;
  grpc_call *call;
  grpc_op ops[6];
  grpc_byte_buffer *send_message;
  grpc_call_error err;
  chezpp_grpc_unary_call *op;
  const char *metadata_error = NULL;
  size_t nops = 0;

  if (!ensure_grpc_loaded()) return make_error_status_message("failed to load gRPC runtime");
  if (channel == NULL) return make_error_status_message("invalid gRPC channel");

  cq = p_grpc_completion_queue_create_for_pluck(NULL);
  if (cq == NULL) return make_error_status_message("failed to create gRPC completion queue");

  method_slice = p_grpc_slice_from_copied_string(method);
  call = p_grpc_channel_create_call(channel, NULL, GRPC_PROPAGATE_DEFAULTS, cq, method_slice, NULL,
                                    make_deadline(timeout_ms), NULL);
  p_grpc_slice_unref(method_slice);
  if (call == NULL) {
    p_grpc_completion_queue_shutdown(cq);
    p_grpc_completion_queue_destroy(cq);
    return make_error_status_message("failed to create gRPC call");
  }

  op = (chezpp_grpc_unary_call *)calloc(1, sizeof(chezpp_grpc_unary_call));
  if (op == NULL) {
    p_grpc_call_unref(call);
    p_grpc_completion_queue_shutdown(cq);
    p_grpc_completion_queue_destroy(cq);
    return make_error_status_message("out of memory");
  }

  op->cq = cq;
  op->call = call;
  p_grpc_metadata_array_init(&op->recv_initial_metadata);
  p_grpc_metadata_array_init(&op->recv_trailing_metadata);
  op->recv_message = NULL;
  op->status = GRPC_STATUS_UNKNOWN;
  op->status_details = p_grpc_empty_slice();
  op->error_string = NULL;

  if (!scheme_metadata_to_grpc(metadata_ls, &op->send_metadata, &op->send_metadata_count,
                               &metadata_error)) {
    cleanup_unary_call(op);
    return make_error_status_message(metadata_error);
  }

  memset(ops, 0, sizeof(ops));
  send_message = make_request_buffer(payload, start, stop);

  ops[nops].op = GRPC_OP_SEND_INITIAL_METADATA;
  ops[nops].data.send_initial_metadata.count = op->send_metadata_count;
  ops[nops].data.send_initial_metadata.metadata = op->send_metadata;
  nops += 1;
  if (send_message != NULL) {
    ops[nops].op = GRPC_OP_SEND_MESSAGE;
    ops[nops].data.send_message.send_message = send_message;
    nops += 1;
  }
  ops[nops].op = GRPC_OP_SEND_CLOSE_FROM_CLIENT;
  nops += 1;
  ops[nops].op = GRPC_OP_RECV_INITIAL_METADATA;
  ops[nops].data.recv_initial_metadata.recv_initial_metadata = &op->recv_initial_metadata;
  nops += 1;
  ops[nops].op = GRPC_OP_RECV_MESSAGE;
  ops[nops].data.recv_message.recv_message = &op->recv_message;
  nops += 1;
  ops[nops].op = GRPC_OP_RECV_STATUS_ON_CLIENT;
  ops[nops].data.recv_status_on_client.trailing_metadata = &op->recv_trailing_metadata;
  ops[nops].data.recv_status_on_client.status = &op->status;
  ops[nops].data.recv_status_on_client.status_details = &op->status_details;
  ops[nops].data.recv_status_on_client.error_string = &op->error_string;
  nops += 1;

  err = p_grpc_call_start_batch(call, ops, nops, op, NULL);
  if (send_message != NULL) p_grpc_byte_buffer_destroy(send_message);
  if (err != GRPC_CALL_OK) {
    cleanup_unary_call(op);
    return make_call_error_status(err);
  }

  if (blocking) {
    grpc_event ev = p_grpc_completion_queue_pluck(cq, op, p_gpr_inf_future(GPR_CLOCK_REALTIME), NULL);
    if (ev.type != GRPC_OP_COMPLETE) {
      cleanup_unary_call(op);
      return make_error_status_message("unexpected gRPC completion event");
    }
    return finalize_unary_call(op);
  }

  return make_handle((uptr)op);
}

static void cleanup_server_call(chezpp_grpc_server_call *call) {
  if (call == NULL) return;
  if (call->request_payload != NULL) {
    p_grpc_byte_buffer_destroy(call->request_payload);
    call->request_payload = NULL;
  }
  if (call->call != NULL) {
    p_grpc_call_unref(call->call);
    call->call = NULL;
  }
  p_grpc_metadata_array_destroy(&call->request_metadata);
  p_grpc_call_details_destroy(&call->details);
  free(call);
}

ptr chezpp_net_grpc_channel_open(const char *target) {
  grpc_channel_credentials *creds;
  grpc_channel *channel;

  if (!ensure_grpc_loaded()) return make_error_status_message("failed to load gRPC runtime");
  creds = p_grpc_insecure_credentials_create();
  if (creds == NULL) return make_error_status_message("failed to create insecure gRPC credentials");
  channel = p_grpc_channel_create(target, creds, NULL);
  p_grpc_channel_credentials_release(creds);
  if (channel == NULL) return make_error_status_message("failed to create gRPC channel");
  return make_handle((uptr)channel);
}

ptr chezpp_net_grpc_channel_close(uptr handle) {
  grpc_channel *channel = (grpc_channel *)handle;
  if (channel == NULL) return make_error_status_message("invalid gRPC channel");
  p_grpc_channel_destroy(channel);
  return Strue;
}

ptr chezpp_net_grpc_server_open(const char *host, int port) {
  chezpp_grpc_server *server;
  grpc_server_credentials *creds;
  char endpoint[256];
  int bound_port;
  ptr out;

  if (!ensure_grpc_loaded()) return make_error_status_message("failed to load gRPC runtime");
  server = (chezpp_grpc_server *)calloc(1, sizeof(chezpp_grpc_server));
  if (server == NULL) return make_error_status_message("out of memory");

  server->cq = p_grpc_completion_queue_create_for_pluck(NULL);
  server->server = p_grpc_server_create(NULL, NULL);
  if (server->cq == NULL || server->server == NULL) {
    if (server->server != NULL) p_grpc_server_destroy(server->server);
    if (server->cq != NULL) p_grpc_completion_queue_destroy(server->cq);
    free(server);
    return make_error_status_message("failed to create gRPC server");
  }

  p_grpc_server_register_completion_queue(server->server, server->cq, NULL);
  creds = p_grpc_insecure_server_credentials_create();
  if (creds == NULL) {
    p_grpc_server_destroy(server->server);
    p_grpc_completion_queue_destroy(server->cq);
    free(server);
    return make_error_status_message("failed to create insecure gRPC server credentials");
  }
  snprintf(endpoint, sizeof(endpoint), "%s:%d", host, port);
  bound_port = p_grpc_server_add_http2_port(server->server, endpoint, creds);
  p_grpc_server_credentials_release(creds);
  if (bound_port == 0) {
    p_grpc_server_destroy(server->server);
    p_grpc_completion_queue_destroy(server->cq);
    free(server);
    return make_error_status_message("failed to bind gRPC server");
  }
  p_grpc_server_start(server->server);
  server->port = bound_port;
  out = Smake_vector(2, Sfalse);
  Svector_set(out, 0, make_handle((uptr)server));
  Svector_set(out, 1, Sinteger(bound_port));
  return out;
}

ptr chezpp_net_grpc_server_close(uptr handle) {
  chezpp_grpc_server *server = (chezpp_grpc_server *)handle;
  if (server == NULL) return make_error_status_message("invalid gRPC server");
  if (!server->closed) {
    p_grpc_server_shutdown_and_notify(server->server, server->cq, server);
    (void)p_grpc_completion_queue_pluck(server->cq, server, p_gpr_inf_future(GPR_CLOCK_REALTIME),
                                        NULL);
    p_grpc_server_destroy(server->server);
    p_grpc_completion_queue_shutdown(server->cq);
    p_grpc_completion_queue_destroy(server->cq);
    server->closed = 1;
  }
  free(server);
  return Strue;
}

ptr chezpp_net_grpc_unary_call(uptr handle, const char *method, ptr payload, int start, int stop,
                               ptr metadata, int timeout_ms) {
  return start_unary_call((grpc_channel *)handle, method, payload, start, stop, metadata,
                          timeout_ms, 1);
}

ptr chezpp_net_grpc_unary_start(uptr handle, const char *method, ptr payload, int start, int stop,
                                ptr metadata, int timeout_ms) {
  return start_unary_call((grpc_channel *)handle, method, payload, start, stop, metadata,
                          timeout_ms, 0);
}

ptr chezpp_net_grpc_unary_poll(uptr handle) {
  chezpp_grpc_unary_call *op = (chezpp_grpc_unary_call *)handle;
  grpc_event ev;
  if (op == NULL) return make_error_status_message("invalid gRPC call handle");
  ev = p_grpc_completion_queue_pluck(op->cq, op, make_immediate_deadline(), NULL);
  if (ev.type == GRPC_QUEUE_TIMEOUT) return Sfalse;
  if (ev.type != GRPC_OP_COMPLETE) {
    cleanup_unary_call(op);
    return make_error_status_message("unexpected gRPC completion event");
  }
  return finalize_unary_call(op);
}

ptr chezpp_net_grpc_unary_close(uptr handle) {
  cleanup_unary_call((chezpp_grpc_unary_call *)handle);
  return Strue;
}

ptr chezpp_net_grpc_server_request(uptr handle) {
  chezpp_grpc_server *server = (chezpp_grpc_server *)handle;
  chezpp_grpc_server_call *call;
  grpc_call_error err;
  grpc_event ev;
  grpc_op ops[1];
  ptr method;
  ptr payload;
  ptr metadata;

  if (server == NULL || server->closed) return make_error_status_message("invalid gRPC server");

  call = (chezpp_grpc_server_call *)calloc(1, sizeof(chezpp_grpc_server_call));
  if (call == NULL) return make_error_status_message("out of memory");

  call->server = server;
  p_grpc_call_details_init(&call->details);
  p_grpc_metadata_array_init(&call->request_metadata);

  err = p_grpc_server_request_call(server->server, &call->call, &call->details,
                                   &call->request_metadata, server->cq, server->cq, call);
  if (err != GRPC_CALL_OK) {
    cleanup_server_call(call);
    return make_call_error_status(err);
  }

  ev = p_grpc_completion_queue_pluck(server->cq, call, p_gpr_inf_future(GPR_CLOCK_REALTIME), NULL);
  if (ev.type != GRPC_OP_COMPLETE) {
    cleanup_server_call(call);
    return make_error_status_message("unexpected gRPC server accept event");
  }

  memset(ops, 0, sizeof(ops));
  ops[0].op = GRPC_OP_RECV_MESSAGE;
  ops[0].data.recv_message.recv_message = &call->request_payload;

  err = p_grpc_call_start_batch(call->call, ops, 1, call->call, NULL);
  if (err != GRPC_CALL_OK) {
    cleanup_server_call(call);
    return make_call_error_status(err);
  }

  ev = p_grpc_completion_queue_pluck(server->cq, call->call, p_gpr_inf_future(GPR_CLOCK_REALTIME),
                                     NULL);
  if (ev.type != GRPC_OP_COMPLETE) {
    cleanup_server_call(call);
    return make_error_status_message("unexpected gRPC request receive event");
  }

  method = maybe_string_from_slice(call->details.method);
  payload = maybe_bytevector_from_buffer(call->request_payload);
  metadata = metadata_array_to_scheme(&call->request_metadata);
  if (call->request_payload != NULL) {
    p_grpc_byte_buffer_destroy(call->request_payload);
    call->request_payload = NULL;
  }
  return make_request_vector((uptr)call, method, payload, metadata);
}

ptr chezpp_net_grpc_server_respond(uptr handle, ptr payload, int start, int stop, int status_code,
                                   const char *status_message, ptr metadata_ls) {
  chezpp_grpc_server_call *call = (chezpp_grpc_server_call *)handle;
  grpc_metadata *metadata = NULL;
  size_t metadata_count = 0;
  grpc_byte_buffer *send_message = NULL;
  grpc_slice status_slice;
  grpc_op ops[3];
  grpc_call_error err;
  grpc_event ev;
  const char *metadata_error = NULL;
  size_t nops = 0;

  if (call == NULL || call->call == NULL) return make_error_status_message("invalid gRPC call");
  if (!scheme_metadata_to_grpc(metadata_ls, &metadata, &metadata_count, &metadata_error)) {
    cleanup_server_call(call);
    return make_error_status_message(metadata_error);
  }

  if (payload != Sfalse) send_message = make_request_buffer(payload, start, stop);
  status_slice = p_grpc_slice_from_copied_string(status_message == NULL ? "" : status_message);

  memset(ops, 0, sizeof(ops));
  ops[nops].op = GRPC_OP_SEND_INITIAL_METADATA;
  ops[nops].data.send_initial_metadata.count = 0;
  ops[nops].data.send_initial_metadata.metadata = NULL;
  nops += 1;
  if (send_message != NULL) {
    ops[nops].op = GRPC_OP_SEND_MESSAGE;
    ops[nops].data.send_message.send_message = send_message;
    nops += 1;
  }
  ops[nops].op = GRPC_OP_SEND_STATUS_FROM_SERVER;
  ops[nops].data.send_status_from_server.trailing_metadata_count = metadata_count;
  ops[nops].data.send_status_from_server.trailing_metadata = metadata;
  ops[nops].data.send_status_from_server.status = (grpc_status_code)status_code;
  ops[nops].data.send_status_from_server.status_details = &status_slice;
  nops += 1;

  err = p_grpc_call_start_batch(call->call, ops, nops, call, NULL);
  p_grpc_slice_unref(status_slice);
  if (send_message != NULL) p_grpc_byte_buffer_destroy(send_message);
  if (err != GRPC_CALL_OK) {
    free_metadata_array(metadata, metadata_count);
    cleanup_server_call(call);
    return make_call_error_status(err);
  }

  ev = p_grpc_completion_queue_pluck(call->server->cq, call, p_gpr_inf_future(GPR_CLOCK_REALTIME),
                                     NULL);
  free_metadata_array(metadata, metadata_count);
  if (ev.type != GRPC_OP_COMPLETE) {
    cleanup_server_call(call);
    return make_error_status_message("unexpected gRPC response completion event");
  }
  cleanup_server_call(call);
  return Strue;
}
