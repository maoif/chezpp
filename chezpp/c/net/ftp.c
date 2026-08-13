#include "../common.h"
#include "../optional_library.h"

#include <curl/curl.h>
#include <pthread.h>

typedef CURLcode (*curl_global_init_fn)(long);
typedef void (*curl_global_cleanup_fn)(void);
typedef CURL *(*curl_easy_init_fn)(void);
typedef void (*curl_easy_cleanup_fn)(CURL *);
typedef CURLcode (*curl_easy_setopt_fn)(CURL *, CURLoption, ...);
typedef CURLcode (*curl_easy_perform_fn)(CURL *);
typedef CURLcode (*curl_easy_pause_fn)(CURL *, int);
typedef const char *(*curl_easy_strerror_fn)(CURLcode);
typedef struct curl_slist *(*curl_slist_append_fn)(struct curl_slist *, const char *);
typedef void (*curl_slist_free_all_fn)(struct curl_slist *);
typedef curl_version_info_data *(*curl_version_info_fn)(CURLversion);
typedef CURLM *(*curl_multi_init_fn)(void);
typedef CURLMcode (*curl_multi_cleanup_fn)(CURLM *);
typedef CURLMcode (*curl_multi_add_handle_fn)(CURLM *, CURL *);
typedef CURLMcode (*curl_multi_remove_handle_fn)(CURLM *, CURL *);
typedef CURLMcode (*curl_multi_socket_action_fn)(CURLM *, curl_socket_t, int, int *);
typedef CURLMsg *(*curl_multi_info_read_fn)(CURLM *, int *);
typedef CURLMcode (*curl_multi_setopt_fn)(CURLM *, CURLMoption, ...);
typedef const char *(*curl_multi_strerror_fn)(CURLMcode);

typedef struct {
  unsigned char *data;
  size_t len;
  size_t cap;
} memory_buffer;

typedef struct ftp_socket_entry {
  curl_socket_t fd;
  int action;
  struct ftp_socket_entry *next;
} ftp_socket_entry;

typedef struct {
  ftp_socket_entry *sockets;
  long timeout_ms;
} ftp_driver;

typedef enum {
  FTP_TRANSFER_LIST = 0,
  FTP_TRANSFER_DOWNLOAD = 1,
  FTP_TRANSFER_UPLOAD = 2
} ftp_transfer_kind;

typedef struct {
  ftp_driver driver;
  CURLM *multi;
  CURL *easy;
  memory_buffer buffer;
  FILE *file;
  char *local_path;
  CURLcode result;
  ftp_transfer_kind kind;
  int added;
  int completed;
  int cancelled;
  int download_succeeded;
} ftp_transfer;

typedef struct ftp_file ftp_file;

typedef struct {
  CURLM *multi;
  ftp_file *active;
  int closed;
} ftp_session;

struct ftp_file {
  ftp_driver driver;
  ftp_session *session;
  CURL *easy;
  memory_buffer buffer;
  size_t buffer_offset;
  CURLcode result;
  int direction;
  int added;
  int completed;
  int cancelled;
  int eof;
  int paused;
};

static int ftp_socket_cb(CURL *easy, curl_socket_t s, int what, void *userp,
                         void *socketp);
static int ftp_timer_cb(CURLM *multi, long timeout_ms, void *userp);

static const char *const curl_names[] = {"libcurl.so.4", NULL};
static chezpp_optional_library curl_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("curl", curl_names);
static pthread_once_t curl_once = PTHREAD_ONCE_INIT;
static int curl_available;
static curl_global_init_fn p_curl_global_init = NULL;
static curl_global_cleanup_fn p_curl_global_cleanup = NULL;
static curl_easy_init_fn p_curl_easy_init = NULL;
static curl_easy_cleanup_fn p_curl_easy_cleanup = NULL;
static curl_easy_setopt_fn p_curl_easy_setopt = NULL;
static curl_easy_perform_fn p_curl_easy_perform = NULL;
static curl_easy_pause_fn p_curl_easy_pause = NULL;
static curl_easy_strerror_fn p_curl_easy_strerror = NULL;
static curl_slist_append_fn p_curl_slist_append = NULL;
static curl_slist_free_all_fn p_curl_slist_free_all = NULL;
static curl_multi_init_fn p_curl_multi_init = NULL;
static curl_multi_cleanup_fn p_curl_multi_cleanup = NULL;
static curl_multi_add_handle_fn p_curl_multi_add_handle = NULL;
static curl_multi_remove_handle_fn p_curl_multi_remove_handle = NULL;
static curl_multi_socket_action_fn p_curl_multi_socket_action = NULL;
static curl_multi_info_read_fn p_curl_multi_info_read = NULL;
static curl_multi_setopt_fn p_curl_multi_setopt = NULL;
static curl_multi_strerror_fn p_curl_multi_strerror = NULL;

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) {
  return make_status("error", msg == NULL ? Sstring("FTP error") : Sstring(msg));
}

static ptr make_errno_status(void) { return make_status("error", errno_str()); }

static ptr make_handle_status(const char *tag, uptr handle) {
  return make_status(tag, Sunsigned(handle));
}

static void ftp_socket_set(ftp_driver *driver, curl_socket_t fd, int action) {
  ftp_socket_entry *p = driver->sockets;
  while (p != NULL && p->fd != fd) p = p->next;
  if (action == CURL_POLL_REMOVE) {
    ftp_socket_entry **q = &driver->sockets;
    while (*q != NULL) {
      if ((*q)->fd == fd) {
        ftp_socket_entry *dead = *q;
        *q = dead->next;
        free(dead);
        return;
      }
      q = &(*q)->next;
    }
    return;
  }
  if (p == NULL) {
    p = (ftp_socket_entry *)calloc(1, sizeof(*p));
    if (p == NULL) return;
    p->fd = fd;
    p->next = driver->sockets;
    driver->sockets = p;
  }
  p->action = action;
}

static size_t ftp_read_file_cb(char *ptr, size_t size, size_t nmemb, void *userdata) {
  ftp_transfer *t = (ftp_transfer *)userdata;
  return fread(ptr, size, nmemb, t->file);
}

static int ftp_socket_cb(CURL *easy, curl_socket_t s, int what, void *userp,
                         void *socketp) {
  (void)easy; (void)socketp;
  ftp_socket_set((ftp_driver *)userp, s, what);
  return 0;
}

static int ftp_timer_cb(CURLM *multi, long timeout_ms, void *userp) {
  ftp_driver *driver = (ftp_driver *)userp;
  (void)multi;
  driver->timeout_ms = timeout_ms;
  return 0;
}

static void memory_buffer_init(memory_buffer *buf) {
  buf->data = NULL;
  buf->len = 0;
  buf->cap = 0;
}

static void memory_buffer_free(memory_buffer *buf) {
  if (buf->data != NULL)
    free(buf->data);
  buf->data = NULL;
  buf->len = 0;
  buf->cap = 0;
}

static void memory_buffer_consume(memory_buffer *buf, size_t count) {
  if (count >= buf->len) {
    buf->len = 0;
    return;
  }
  memmove(buf->data, buf->data + count, buf->len - count);
  buf->len -= count;
}

static int memory_buffer_append(memory_buffer *buf, const unsigned char *data,
                                size_t count) {
  size_t need;
  size_t cap;
  unsigned char *next;
  if (count == 0) return 1;
  need = buf->len + count;
  if (need > buf->cap) {
    cap = buf->cap == 0 ? 65536 : buf->cap;
    while (cap < need) cap *= 2;
    next = (unsigned char *)realloc(buf->data, cap);
    if (next == NULL) return 0;
    buf->data = next;
    buf->cap = cap;
  }
  memcpy(buf->data + buf->len, data, count);
  buf->len += count;
  return 1;
}

static size_t write_memory_cb(char *ptr, size_t size, size_t nmemb, void *userdata) {
  memory_buffer *buf = (memory_buffer *)userdata;
  size_t n = size * nmemb;
  size_t need;
  unsigned char *next;

  if (n == 0)
    return 0;
  need = buf->len + n;
  if (need > buf->cap) {
    size_t cap = buf->cap == 0 ? 4096 : buf->cap;
    while (cap < need)
      cap *= 2;
    next = (unsigned char *)realloc(buf->data, cap);
    if (next == NULL)
      return 0;
    buf->data = next;
    buf->cap = cap;
  }
  memcpy(buf->data + buf->len, ptr, n);
  buf->len += n;
  return n;
}

static size_t ftp_file_write_cb(char *ptr, size_t size, size_t nmemb,
                                void *userdata) {
  ftp_file *file = (ftp_file *)userdata;
  size_t count = size * nmemb;
  if (file->buffer.len != 0) {
    file->paused = 1;
    return CURL_WRITEFUNC_PAUSE;
  }
  if (!memory_buffer_append(&file->buffer, (const unsigned char *)ptr, count))
    return 0;
  return count;
}

static size_t ftp_file_read_cb(char *ptr, size_t size, size_t nmemb,
                               void *userdata) {
  ftp_file *file = (ftp_file *)userdata;
  size_t capacity = size * nmemb;
  size_t count;
  if (file->buffer.len == 0) {
    if (file->eof) return 0;
    file->paused = 1;
    return CURL_READFUNC_PAUSE;
  }
  count = file->buffer.len < capacity ? file->buffer.len : capacity;
  memcpy(ptr, file->buffer.data, count);
  memory_buffer_consume(&file->buffer, count);
  return count;
}

#define CURL_LOAD(pointer, name)                                              \
  chezpp_optional_library_symbol(&curl_library, name, (void **)&pointer)

static void initialize_curl(void) {
  curl_version_info_fn version_info = NULL;
  curl_version_info_data *data;
  unsigned major;
  unsigned minor;
  unsigned patch;

  if (!chezpp_optional_library_open(&curl_library)) return;
  if (!CURL_LOAD(version_info, "curl_version_info")) return;
  data = version_info(CURLVERSION_NOW);
  if (data == NULL || data->version == NULL) {
    chezpp_optional_library_fail(&curl_library,
                                 "curl: curl_version_info returned no version");
    return;
  }
  chezpp_optional_library_set_version(&curl_library, data->version);
  major = (unsigned)((data->version_num >> 16) & 0xffU);
  minor = (unsigned)((data->version_num >> 8) & 0xffU);
  patch = (unsigned)(data->version_num & 0xffU);
  if (major < 8) {
    chezpp_optional_library_fail(
        &curl_library, "curl: runtime version %u.%u.%u requires >= 8.0.0",
        major, minor, patch);
    return;
  }
  if (!CURL_LOAD(p_curl_global_init, "curl_global_init") ||
      !CURL_LOAD(p_curl_global_cleanup, "curl_global_cleanup") ||
      !CURL_LOAD(p_curl_easy_init, "curl_easy_init") ||
      !CURL_LOAD(p_curl_easy_cleanup, "curl_easy_cleanup") ||
      !CURL_LOAD(p_curl_easy_setopt, "curl_easy_setopt") ||
      !CURL_LOAD(p_curl_easy_perform, "curl_easy_perform") ||
      !CURL_LOAD(p_curl_easy_pause, "curl_easy_pause") ||
      !CURL_LOAD(p_curl_easy_strerror, "curl_easy_strerror") ||
      !CURL_LOAD(p_curl_slist_append, "curl_slist_append") ||
      !CURL_LOAD(p_curl_slist_free_all, "curl_slist_free_all") ||
      !CURL_LOAD(p_curl_multi_init, "curl_multi_init") ||
      !CURL_LOAD(p_curl_multi_cleanup, "curl_multi_cleanup") ||
      !CURL_LOAD(p_curl_multi_add_handle, "curl_multi_add_handle") ||
      !CURL_LOAD(p_curl_multi_remove_handle, "curl_multi_remove_handle") ||
      !CURL_LOAD(p_curl_multi_socket_action, "curl_multi_socket_action") ||
      !CURL_LOAD(p_curl_multi_info_read, "curl_multi_info_read") ||
      !CURL_LOAD(p_curl_multi_setopt, "curl_multi_setopt") ||
      !CURL_LOAD(p_curl_multi_strerror, "curl_multi_strerror")) return;
  if (p_curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK) {
    chezpp_optional_library_fail(&curl_library,
                                 "curl: runtime initialization failed");
    return;
  }
  curl_available = 1;
}

static int ensure_curl_loaded(void) {
  pthread_once(&curl_once, initialize_curl);
  return curl_available;
}

const chezpp_optional_library *chezpp_net_curl_library(void) {
  (void)ensure_curl_loaded();
  return &curl_library;
}

unsigned chezpp_net_curl_capabilities(void) { return 0; }

static ptr curl_error_status(CURLcode code) {
  const char *msg;
  if (p_curl_easy_strerror == NULL)
    return make_error_status_message("libcurl error");
  msg = p_curl_easy_strerror(code);
  return make_error_status_message(msg == NULL ? "libcurl error" : msg);
}

static ptr apply_common_options(CURL *curl, const char *url, const char *user, const char *pass,
                                int passive, int timeout_ms, int use_tls, int verify_peer,
                                int verify_host) {
  CURLcode rc;

  rc = p_curl_easy_setopt(curl, CURLOPT_URL, url);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  rc = p_curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1L);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  rc = p_curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, (long)timeout_ms);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  rc = p_curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT_MS, (long)timeout_ms);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  rc = p_curl_easy_setopt(curl, CURLOPT_USERNAME, user == NULL ? "" : user);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  rc = p_curl_easy_setopt(curl, CURLOPT_PASSWORD, pass == NULL ? "" : pass);
  if (rc != CURLE_OK)
    return curl_error_status(rc);
  if (!passive) {
    rc = p_curl_easy_setopt(curl, CURLOPT_FTPPORT, "-");
    if (rc != CURLE_OK)
      return curl_error_status(rc);
  }
  if (use_tls) {
    rc = p_curl_easy_setopt(curl, CURLOPT_USE_SSL, (long)CURLUSESSL_ALL);
    if (rc != CURLE_OK)
      return curl_error_status(rc);
    rc = p_curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, verify_peer ? 1L : 0L);
    if (rc != CURLE_OK)
      return curl_error_status(rc);
    rc = p_curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, verify_host ? 2L : 0L);
    if (rc != CURLE_OK)
      return curl_error_status(rc);
  }
  return Strue;
}

static ptr perform_fetch(const char *url, const char *user, const char *pass, int passive,
                         int timeout_ms, int use_tls, int verify_peer, int verify_host,
                         long dirlistonly) {
  CURL *curl;
  CURLcode rc;
  ptr result;
  memory_buffer buf;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));

  curl = p_curl_easy_init();
  if (curl == NULL)
    return make_error_status_message("failed to initialize libcurl easy handle");

  memory_buffer_init(&buf);
  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue)
    goto cleanup;

  rc = p_curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_memory_cb);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buf);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_DIRLISTONLY, dirlistonly);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  rc = p_curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  result = Smake_bytevector((iptr)buf.len, 0);
  if (buf.len > 0)
    memcpy(Sbytevector_data(result), buf.data, buf.len);

cleanup:
  memory_buffer_free(&buf);
  p_curl_easy_cleanup(curl);
  return result;
}

ptr chezpp_net_ftp_list(const char *url, const char *user, const char *pass, int passive,
                        int timeout_ms, int use_tls, int verify_peer, int verify_host) {
  return perform_fetch(url, user, pass, passive, timeout_ms, use_tls, verify_peer, verify_host,
                       1L);
}

ptr chezpp_net_ftp_stat(const char *url, const char *user, const char *pass, int passive,
                        int timeout_ms, int use_tls, int verify_peer, int verify_host,
                        const char *path) {
  CURL *curl;
  CURLcode rc;
  ptr result;
  memory_buffer buf;
  char *command;
  struct curl_slist *quote = NULL;
  size_t command_len = strlen(path) + 6;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));
  curl = p_curl_easy_init();
  command = (char *)malloc(command_len);
  if (curl == NULL || command == NULL) {
    if (curl != NULL) p_curl_easy_cleanup(curl);
    if (command != NULL) free(command);
    return make_errno_status();
  }
  snprintf(command, command_len, "MLST %s", path);
  memory_buffer_init(&buf);
  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue) goto cleanup;
  quote = p_curl_slist_append(quote, command);
  if (quote == NULL) {
    result = make_errno_status();
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_QUOTE, quote);
  if (rc == CURLE_OK) rc = p_curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);
  if (rc == CURLE_OK) rc = p_curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, write_memory_cb);
  if (rc == CURLE_OK) rc = p_curl_easy_setopt(curl, CURLOPT_HEADERDATA, &buf);
  if (rc == CURLE_OK) rc = p_curl_easy_perform(curl);
  if (rc == CURLE_QUOTE_ERROR) {
    result = Sfalse;
    goto cleanup;
  }
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  result = Smake_bytevector((iptr)buf.len, 0);
  if (buf.len > 0) memcpy(Sbytevector_data(result), buf.data, buf.len);

cleanup:
  if (quote != NULL) p_curl_slist_free_all(quote);
  memory_buffer_free(&buf);
  free(command);
  p_curl_easy_cleanup(curl);
  return result;
}

ptr chezpp_net_ftp_download(const char *url, const char *dest, const char *user, const char *pass,
                            int passive, int timeout_ms, int use_tls, int verify_peer,
                            int verify_host) {
  CURL *curl;
  CURLcode rc;
  FILE *fp = NULL;
  ptr result;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));

  curl = p_curl_easy_init();
  if (curl == NULL)
    return make_error_status_message("failed to initialize libcurl easy handle");

  fp = fopen(dest, "wb");
  if (fp == NULL) {
    p_curl_easy_cleanup(curl);
    return make_errno_status();
  }

  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue)
    goto cleanup;

  rc = p_curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  rc = p_curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  result = Strue;

cleanup:
  fclose(fp);
  p_curl_easy_cleanup(curl);
  return result;
}

ptr chezpp_net_ftp_upload(const char *url, const char *src, const char *user, const char *pass,
                          int passive, int timeout_ms, int use_tls, int verify_peer,
                          int verify_host) {
  CURL *curl;
  CURLcode rc;
  FILE *fp = NULL;
  long size;
  ptr result;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));

  curl = p_curl_easy_init();
  if (curl == NULL)
    return make_error_status_message("failed to initialize libcurl easy handle");

  fp = fopen(src, "rb");
  if (fp == NULL) {
    p_curl_easy_cleanup(curl);
    return make_errno_status();
  }
  if (fseek(fp, 0, SEEK_END) != 0) {
    result = make_errno_status();
    goto cleanup;
  }
  size = ftell(fp);
  if (size < 0) {
    result = make_errno_status();
    goto cleanup;
  }
  if (fseek(fp, 0, SEEK_SET) != 0) {
    result = make_errno_status();
    goto cleanup;
  }

  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue)
    goto cleanup;

  rc = p_curl_easy_setopt(curl, CURLOPT_UPLOAD, 1L);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_READDATA, fp);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  rc = p_curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  result = Strue;

cleanup:
  fclose(fp);
  p_curl_easy_cleanup(curl);
  return result;
}

ptr chezpp_net_ftp_command(const char *url, const char *user, const char *pass, int passive,
                           int timeout_ms, int use_tls, int verify_peer, int verify_host,
                           const char *cmd) {
  CURL *curl;
  CURLcode rc;
  ptr result;
  struct curl_slist *quote = NULL;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));

  curl = p_curl_easy_init();
  if (curl == NULL)
    return make_error_status_message("failed to initialize libcurl easy handle");

  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue)
    goto cleanup;

  quote = p_curl_slist_append(quote, cmd);
  if (quote == NULL) {
    result = make_errno_status();
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_QUOTE, quote);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  rc = p_curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  result = Strue;

cleanup:
  if (quote != NULL)
    p_curl_slist_free_all(quote);
  p_curl_easy_cleanup(curl);
  return result;
}

ptr chezpp_net_ftp_rename(const char *url, const char *user, const char *pass, int passive,
                          int timeout_ms, int use_tls, int verify_peer, int verify_host,
                          const char *from_path, const char *to_path) {
  CURL *curl;
  CURLcode rc;
  ptr result;
  struct curl_slist *quote = NULL;
  char *rnfr = NULL;
  char *rnto = NULL;
  size_t rnfr_len;
  size_t rnto_len;

  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));

  curl = p_curl_easy_init();
  if (curl == NULL)
    return make_error_status_message("failed to initialize libcurl easy handle");

  rnfr_len = strlen(from_path) + 6;
  rnto_len = strlen(to_path) + 6;
  rnfr = (char *)malloc(rnfr_len);
  rnto = (char *)malloc(rnto_len);
  if (rnfr == NULL || rnto == NULL) {
    result = make_errno_status();
    goto cleanup;
  }
  snprintf(rnfr, rnfr_len, "RNFR %s", from_path);
  snprintf(rnto, rnto_len, "RNTO %s", to_path);

  result = apply_common_options(curl, url, user, pass, passive, timeout_ms, use_tls,
                                verify_peer, verify_host);
  if (result != Strue)
    goto cleanup;

  quote = p_curl_slist_append(quote, rnfr);
  quote = quote == NULL ? NULL : p_curl_slist_append(quote, rnto);
  if (quote == NULL) {
    result = make_errno_status();
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_QUOTE, quote);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }
  rc = p_curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  rc = p_curl_easy_perform(curl);
  if (rc != CURLE_OK) {
    result = curl_error_status(rc);
    goto cleanup;
  }

  result = Strue;

cleanup:
  if (quote != NULL)
    p_curl_slist_free_all(quote);
  if (rnfr != NULL)
    free(rnfr);
  if (rnto != NULL)
    free(rnto);
  p_curl_easy_cleanup(curl);
  return result;
}

static void ftp_transfer_release(ftp_transfer *t) {
  ftp_socket_entry *p;
  if (t == NULL) return;
  if (t->added && t->multi != NULL && t->easy != NULL)
    (void)p_curl_multi_remove_handle(t->multi, t->easy);
  if (t->easy != NULL) p_curl_easy_cleanup(t->easy);
  if (t->multi != NULL) (void)p_curl_multi_cleanup(t->multi);
  if (t->file != NULL) fclose(t->file);
  if (t->kind == FTP_TRANSFER_DOWNLOAD && !t->download_succeeded &&
      t->local_path != NULL)
    (void)unlink(t->local_path);
  memory_buffer_free(&t->buffer);
  if (t->local_path != NULL) free(t->local_path);
  while (t->driver.sockets != NULL) {
    p = t->driver.sockets;
    t->driver.sockets = p->next;
    free(p);
  }
  free(t);
}

static ptr ftp_transfer_configure(ftp_transfer *t, const char *url, const char *path,
                                  const char *user, const char *pass, int passive,
                                  int timeout_ms, int use_tls, int verify_peer,
                                  int verify_host) {
  ptr out;
  CURLcode rc;
  long size;

  out = apply_common_options(t->easy, url, user, pass, passive, timeout_ms, use_tls,
                             verify_peer, verify_host);
  if (out != Strue) return out;
  if (t->kind == FTP_TRANSFER_LIST) {
    rc = p_curl_easy_setopt(t->easy, CURLOPT_WRITEFUNCTION, write_memory_cb);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_WRITEDATA, &t->buffer);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_DIRLISTONLY, 0L);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_CUSTOMREQUEST, "MLSD");
  } else if (t->kind == FTP_TRANSFER_DOWNLOAD) {
    t->local_path = strdup(path);
    if (t->local_path == NULL) return make_errno_status();
    t->file = fopen(path, "wb");
    if (t->file == NULL) return make_errno_status();
    rc = p_curl_easy_setopt(t->easy, CURLOPT_WRITEDATA, t->file);
  } else {
    t->file = fopen(path, "rb");
    if (t->file == NULL) return make_errno_status();
    if (fseek(t->file, 0, SEEK_END) != 0 || (size = ftell(t->file)) < 0 ||
        fseek(t->file, 0, SEEK_SET) != 0)
      return make_errno_status();
    rc = p_curl_easy_setopt(t->easy, CURLOPT_UPLOAD, 1L);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_READFUNCTION, ftp_read_file_cb);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_READDATA, t);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(t->easy, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
  }
  return rc == CURLE_OK ? Strue : curl_error_status(rc);
}

ptr chezpp_net_ftp_transfer_start(int kind, const char *url, const char *path,
                                  const char *user, const char *pass, int passive,
                                  int timeout_ms, int use_tls, int verify_peer,
                                  int verify_host) {
  ftp_transfer *t;
  ptr out;
  CURLMcode mrc;

  if (kind < FTP_TRANSFER_LIST || kind > FTP_TRANSFER_UPLOAD)
    return make_error_status_message("invalid FTP transfer kind");
  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));
  t = (ftp_transfer *)calloc(1, sizeof(*t));
  if (t == NULL) return make_errno_status();
  t->kind = (ftp_transfer_kind)kind;
  t->driver.timeout_ms = -1;
  memory_buffer_init(&t->buffer);
  t->easy = p_curl_easy_init();
  t->multi = p_curl_multi_init();
  if (t->easy == NULL || t->multi == NULL) {
    ftp_transfer_release(t);
    return make_error_status_message("failed to initialize libcurl transfer");
  }
  out = ftp_transfer_configure(t, url, path, user, pass, passive, timeout_ms,
                               use_tls, verify_peer, verify_host);
  if (out != Strue) {
    ftp_transfer_release(t);
    return out;
  }
  mrc = p_curl_multi_setopt(t->multi, CURLMOPT_SOCKETFUNCTION, ftp_socket_cb);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(t->multi, CURLMOPT_SOCKETDATA, t);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(t->multi, CURLMOPT_TIMERFUNCTION, ftp_timer_cb);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(t->multi, CURLMOPT_TIMERDATA, t);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_add_handle(t->multi, t->easy);
  if (mrc != CURLM_OK) {
    out = make_error_status_message(p_curl_multi_strerror(mrc));
    ftp_transfer_release(t);
    return out;
  }
  t->added = 1;
  return make_handle_status("ok", (uptr)t);
}

static ptr ftp_pending_status(ftp_transfer *t) {
  ftp_socket_entry *p;
  iptr count = 0;
  iptr i = 0;
  ptr targets;
  ptr out = Smake_vector(3, Sfalse);
  for (p = t->driver.sockets; p != NULL; p = p->next) count++;
  targets = Smake_vector(count, Sfalse);
  for (p = t->driver.sockets; p != NULL; p = p->next) {
    ptr target = Smake_vector(2, Sfalse);
    int events = 0;
    if (p->action == CURL_POLL_IN || p->action == CURL_POLL_INOUT) events |= POLLIN;
    if (p->action == CURL_POLL_OUT || p->action == CURL_POLL_INOUT) events |= POLLOUT;
    Svector_set(target, 0, Sinteger((iptr)p->fd));
    Svector_set(target, 1, Sinteger(events));
    Svector_set(targets, i++, target);
  }
  Svector_set(out, 0, Sstring_to_symbol("pending"));
  Svector_set(out, 1, targets);
  Svector_set(out, 2, t->driver.timeout_ms < 0 ? Sfalse : Sinteger(t->driver.timeout_ms));
  return out;
}

ptr chezpp_net_ftp_transfer_step(uptr handle, ptr ready, int timer_expired) {
  ftp_transfer *t = (ftp_transfer *)handle;
  CURLMcode mrc = CURLM_OK;
  CURLMsg *msg;
  int running = 0;
  int queued = 0;
  ptr value;

  if (t == NULL || t->cancelled)
    return make_error_status_message("FTP transfer is closed or cancelled");
  if (t->completed) return make_error_status_message("FTP transfer is already complete");
  if (!Svectorp(ready))
    return make_error_status_message("FTP ready events must be a vector");
  if (timer_expired) {
    mrc = p_curl_multi_socket_action(t->multi, CURL_SOCKET_TIMEOUT, 0, &running);
  }
  for (iptr i = 0; i < Svector_length(ready) && mrc == CURLM_OK; i++) {
    ptr event = Svector_ref(ready, i);
    if (!Svectorp(event) || Svector_length(event) != 2 ||
        !Sfixnump(Svector_ref(event, 0)) || !Sfixnump(Svector_ref(event, 1)))
      return make_error_status_message("invalid FTP ready descriptor/event pair");
    {
      int events = (int)Sfixnum_value(Svector_ref(event, 1));
      int action = 0;
      if (events & POLLIN) action |= CURL_CSELECT_IN;
      if (events & POLLOUT) action |= CURL_CSELECT_OUT;
      if (events & (POLLERR | POLLHUP | POLLNVAL)) action |= CURL_CSELECT_ERR;
      mrc = p_curl_multi_socket_action(
          t->multi, (curl_socket_t)Sfixnum_value(Svector_ref(event, 0)), action, &running);
    }
  }
  if (mrc != CURLM_OK)
    return make_error_status_message(p_curl_multi_strerror(mrc));
  while ((msg = p_curl_multi_info_read(t->multi, &queued)) != NULL) {
    if (msg->msg == CURLMSG_DONE && msg->easy_handle == t->easy) {
      t->completed = 1;
      t->result = msg->data.result;
      break;
    }
  }
  if (!t->completed) return ftp_pending_status(t);
  if (t->result != CURLE_OK) return curl_error_status(t->result);
  if (t->kind == FTP_TRANSFER_DOWNLOAD) t->download_succeeded = 1;
  if (t->kind == FTP_TRANSFER_LIST) {
    value = Smake_bytevector((iptr)t->buffer.len, 0);
    if (t->buffer.len > 0) memcpy(Sbytevector_data(value), t->buffer.data, t->buffer.len);
  } else {
    value = Strue;
  }
  return make_status("completed", value);
}

ptr chezpp_net_ftp_transfer_cancel(uptr handle) {
  ftp_transfer *t = (ftp_transfer *)handle;
  if (t == NULL) return Strue;
  t->cancelled = 1;
  if (t->added) {
    (void)p_curl_easy_pause(t->easy, CURLPAUSE_ALL);
    (void)p_curl_multi_remove_handle(t->multi, t->easy);
    t->added = 0;
  }
  return Strue;
}

void chezpp_net_ftp_transfer_close(uptr handle) {
  ftp_transfer_release((ftp_transfer *)handle);
}

static void ftp_driver_clear(ftp_driver *driver) {
  ftp_socket_entry *entry;
  while (driver->sockets != NULL) {
    entry = driver->sockets;
    driver->sockets = entry->next;
    free(entry);
  }
  driver->timeout_ms = -1;
}

static void ftp_file_release(ftp_file *file) {
  if (file == NULL) return;
  if (file->added && file->session != NULL && file->session->multi != NULL)
    (void)p_curl_multi_remove_handle(file->session->multi, file->easy);
  if (file->easy != NULL) p_curl_easy_cleanup(file->easy);
  if (file->session != NULL && file->session->active == file)
    file->session->active = NULL;
  memory_buffer_free(&file->buffer);
  ftp_driver_clear(&file->driver);
  free(file);
}

static ptr ftp_file_pending_status(ftp_file *file) {
  ftp_socket_entry *entry;
  iptr count = 0;
  iptr index = 0;
  ptr targets;
  ptr out = Smake_vector(3, Sfalse);
  for (entry = file->driver.sockets; entry != NULL; entry = entry->next) count++;
  targets = Smake_vector(count, Sfalse);
  for (entry = file->driver.sockets; entry != NULL; entry = entry->next) {
    ptr target = Smake_vector(2, Sfalse);
    int events = 0;
    if (entry->action == CURL_POLL_IN || entry->action == CURL_POLL_INOUT)
      events |= POLLIN;
    if (entry->action == CURL_POLL_OUT || entry->action == CURL_POLL_INOUT)
      events |= POLLOUT;
    Svector_set(target, 0, Sinteger((iptr)entry->fd));
    Svector_set(target, 1, Sinteger(events));
    Svector_set(targets, index++, target);
  }
  Svector_set(out, 0, Sstring_to_symbol("pending"));
  Svector_set(out, 1, targets);
  Svector_set(out, 2, file->driver.timeout_ms < 0
                              ? Sfalse
                              : Sinteger(file->driver.timeout_ms));
  return out;
}

static ptr ftp_file_drive(ftp_file *file, ptr ready, int timer_expired) {
  CURLMcode mrc = CURLM_OK;
  CURLMsg *msg;
  int running = 0;
  int queued = 0;
  if (file == NULL || file->cancelled || file->session == NULL ||
      file->session->closed)
    return make_error_status_message("FTP file is closed or cancelled");
  if (!Svectorp(ready))
    return make_error_status_message("FTP ready events must be a vector");
  if (timer_expired)
    mrc = p_curl_multi_socket_action(file->session->multi, CURL_SOCKET_TIMEOUT,
                                     0, &running);
  for (iptr i = 0; i < Svector_length(ready) && mrc == CURLM_OK; i++) {
    ptr event = Svector_ref(ready, i);
    int action = 0;
    int events;
    if (!Svectorp(event) || Svector_length(event) != 2 ||
        !Sfixnump(Svector_ref(event, 0)) ||
        !Sfixnump(Svector_ref(event, 1)))
      return make_error_status_message("invalid FTP ready descriptor/event pair");
    events = (int)Sfixnum_value(Svector_ref(event, 1));
    if (events & POLLIN) action |= CURL_CSELECT_IN;
    if (events & POLLOUT) action |= CURL_CSELECT_OUT;
    if (events & (POLLERR | POLLHUP | POLLNVAL)) action |= CURL_CSELECT_ERR;
    mrc = p_curl_multi_socket_action(file->session->multi,
                                     (curl_socket_t)Sfixnum_value(Svector_ref(event, 0)),
                                     action, &running);
  }
  if (mrc != CURLM_OK)
    return make_error_status_message(p_curl_multi_strerror(mrc));
  while ((msg = p_curl_multi_info_read(file->session->multi, &queued)) != NULL) {
    if (msg->msg == CURLMSG_DONE && msg->easy_handle == file->easy) {
      file->completed = 1;
      file->result = msg->data.result;
      break;
    }
  }
  if (file->completed && file->result != CURLE_OK)
    return curl_error_status(file->result);
  return file->completed ? make_status("completed", Strue)
                         : ftp_file_pending_status(file);
}

ptr chezpp_net_ftp_session_open(void) {
  ftp_session *session;
  if (!ensure_curl_loaded())
    return make_error_status_message(chezpp_optional_library_error(&curl_library));
  session = (ftp_session *)calloc(1, sizeof(*session));
  if (session == NULL) return make_errno_status();
  session->multi = p_curl_multi_init();
  if (session->multi == NULL) {
    free(session);
    return make_error_status_message("failed to initialize FTP session");
  }
  return make_handle_status("ok", (uptr)session);
}

ptr chezpp_net_ftp_session_close(uptr handle) {
  ftp_session *session = (ftp_session *)handle;
  if (session == NULL) return Strue;
  session->closed = 1;
  if (session->active != NULL) {
    session->active->cancelled = 1;
    ftp_file_release(session->active);
  }
  if (session->multi != NULL) (void)p_curl_multi_cleanup(session->multi);
  free(session);
  return Strue;
}

ptr chezpp_net_ftp_file_open(uptr session_handle, int direction,
                             const char *url, const char *user, const char *pass,
                             int passive, int timeout_ms, int use_tls,
                             int verify_peer, int verify_host, iptr offset) {
  ftp_session *session = (ftp_session *)session_handle;
  ftp_file *file;
  ptr out;
  CURLcode rc;
  CURLMcode mrc;
  if (session == NULL || session->closed)
    return make_error_status_message("FTP session is closed");
  if (session->active != NULL)
    return make_error_status_message("FTP session already has an active file");
  if (direction != 0 && direction != 1)
    return make_error_status_message("invalid FTP file direction");
  if (offset < 0) return make_error_status_message("invalid FTP resume offset");
  file = (ftp_file *)calloc(1, sizeof(*file));
  if (file == NULL) return make_errno_status();
  file->session = session;
  file->direction = direction;
  file->driver.timeout_ms = -1;
  memory_buffer_init(&file->buffer);
  file->easy = p_curl_easy_init();
  if (file->easy == NULL) {
    ftp_file_release(file);
    return make_error_status_message("failed to initialize FTP file");
  }
  out = apply_common_options(file->easy, url, user, pass, passive, timeout_ms,
                             use_tls, verify_peer, verify_host);
  if (out != Strue) {
    ftp_file_release(file);
    return out;
  }
  if (direction == 0) {
    rc = p_curl_easy_setopt(file->easy, CURLOPT_WRITEFUNCTION, ftp_file_write_cb);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(file->easy, CURLOPT_WRITEDATA, file);
  } else {
    rc = p_curl_easy_setopt(file->easy, CURLOPT_UPLOAD, 1L);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(file->easy, CURLOPT_READFUNCTION, ftp_file_read_cb);
    if (rc == CURLE_OK)
      rc = p_curl_easy_setopt(file->easy, CURLOPT_READDATA, file);
  }
  if (rc == CURLE_OK && offset != 0)
    rc = p_curl_easy_setopt(file->easy, CURLOPT_RESUME_FROM_LARGE, (curl_off_t)offset);
  if (rc != CURLE_OK) {
    out = curl_error_status(rc);
    ftp_file_release(file);
    return out;
  }
  mrc = p_curl_multi_setopt(session->multi, CURLMOPT_SOCKETFUNCTION, ftp_socket_cb);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(session->multi, CURLMOPT_SOCKETDATA, &file->driver);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(session->multi, CURLMOPT_TIMERFUNCTION, ftp_timer_cb);
  if (mrc == CURLM_OK)
    mrc = p_curl_multi_setopt(session->multi, CURLMOPT_TIMERDATA, &file->driver);
  if (mrc == CURLM_OK) mrc = p_curl_multi_add_handle(session->multi, file->easy);
  if (mrc != CURLM_OK) {
    out = make_error_status_message(p_curl_multi_strerror(mrc));
    ftp_file_release(file);
    return out;
  }
  file->added = 1;
  session->active = file;
  return make_handle_status("ok", (uptr)file);
}

ptr chezpp_net_ftp_file_step(uptr handle, ptr ready, int timer_expired) {
  return ftp_file_drive((ftp_file *)handle, ready, timer_expired);
}

ptr chezpp_net_ftp_file_read(uptr handle, int count) {
  ftp_file *file = (ftp_file *)handle;
  ptr out;
  size_t amount;
  if (file == NULL || file->direction != 0 || count < 0)
    return make_error_status_message("invalid FTP file read");
  if (count == 0) return Smake_bytevector(0, 0);
  if (file->buffer.len == 0) {
    if (file->completed) return Seof_object;
    return make_status("would-block", Sfalse);
  }
  amount = file->buffer.len < (size_t)count ? file->buffer.len : (size_t)count;
  out = Smake_bytevector((iptr)amount, 0);
  if (amount > 0) memcpy(Sbytevector_data(out), file->buffer.data, amount);
  memory_buffer_consume(&file->buffer, amount);
  if (file->paused) {
    file->paused = 0;
    (void)p_curl_easy_pause(file->easy, CURLPAUSE_CONT);
  }
  return out;
}

ptr chezpp_net_ftp_file_read_into(uptr handle, ptr bytevector, int length,
                                  int start, int count) {
  ftp_file *file = (ftp_file *)handle;
  size_t amount;
  if (file == NULL || file->direction != 0 || !Sbytevectorp(bytevector) || start < 0 ||
      count < 0 || start > length || count > length - start)
    return make_error_status_message("invalid FTP file read buffer");
  if (count == 0) return Sinteger(0);
  if (file->buffer.len == 0) {
    if (file->completed) return Seof_object;
    return make_status("would-block", Sfalse);
  }
  amount = file->buffer.len < (size_t)count ? file->buffer.len : (size_t)count;
  memcpy(Sbytevector_data(bytevector) + start, file->buffer.data, amount);
  memory_buffer_consume(&file->buffer, amount);
  if (file->paused) {
    file->paused = 0;
    (void)p_curl_easy_pause(file->easy, CURLPAUSE_CONT);
  }
  return Sinteger((iptr)amount);
}

ptr chezpp_net_ftp_file_write(uptr handle, ptr bytevector, int length,
                              int start, int count) {
  ftp_file *file = (ftp_file *)handle;
  if (file == NULL || file->direction != 1 || !Sbytevectorp(bytevector) || start < 0 ||
      count < 0 || start > length || count > length - start)
    return make_error_status_message("invalid FTP file write buffer");
  if (count == 0) return Sinteger(0);
  if (file->completed)
    return make_error_status_message("FTP upload is already complete");
  if (file->buffer.len != 0) return make_status("would-block", Sfalse);
  if (!memory_buffer_append(&file->buffer, Sbytevector_data(bytevector) + start,
                            (size_t)count))
    return make_errno_status();
  if (file->paused) {
    file->paused = 0;
    (void)p_curl_easy_pause(file->easy, CURLPAUSE_CONT);
  }
  return Sinteger(count);
}

ptr chezpp_net_ftp_file_finish(uptr handle) {
  ftp_file *file = (ftp_file *)handle;
  if (file == NULL) return make_error_status_message("FTP file is closed");
  if (file->direction == 1) {
    file->eof = 1;
    if (file->paused) {
      file->paused = 0;
      (void)p_curl_easy_pause(file->easy, CURLPAUSE_CONT);
    }
  }
  return Strue;
}

ptr chezpp_net_ftp_file_cancel(uptr handle) {
  ftp_file *file = (ftp_file *)handle;
  if (file == NULL) return Strue;
  file->cancelled = 1;
  if (file->added) {
    (void)p_curl_easy_pause(file->easy, CURLPAUSE_ALL);
    (void)p_curl_multi_remove_handle(file->session->multi, file->easy);
    file->added = 0;
  }
  return Strue;
}

void chezpp_net_ftp_file_close(uptr handle) {
  ftp_file_release((ftp_file *)handle);
}
