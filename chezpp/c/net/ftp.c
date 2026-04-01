#include "../common.h"

#include <curl/curl.h>
#include <dlfcn.h>

typedef CURLcode (*curl_global_init_fn)(long);
typedef void (*curl_global_cleanup_fn)(void);
typedef CURL *(*curl_easy_init_fn)(void);
typedef void (*curl_easy_cleanup_fn)(CURL *);
typedef CURLcode (*curl_easy_setopt_fn)(CURL *, CURLoption, ...);
typedef CURLcode (*curl_easy_perform_fn)(CURL *);
typedef const char *(*curl_easy_strerror_fn)(CURLcode);
typedef struct curl_slist *(*curl_slist_append_fn)(struct curl_slist *, const char *);
typedef void (*curl_slist_free_all_fn)(struct curl_slist *);

typedef struct {
  unsigned char *data;
  size_t len;
  size_t cap;
} memory_buffer;

static void *curl_handle = NULL;
static int curl_initialized = 0;
static curl_global_init_fn p_curl_global_init = NULL;
static curl_global_cleanup_fn p_curl_global_cleanup = NULL;
static curl_easy_init_fn p_curl_easy_init = NULL;
static curl_easy_cleanup_fn p_curl_easy_cleanup = NULL;
static curl_easy_setopt_fn p_curl_easy_setopt = NULL;
static curl_easy_perform_fn p_curl_easy_perform = NULL;
static curl_easy_strerror_fn p_curl_easy_strerror = NULL;
static curl_slist_append_fn p_curl_slist_append = NULL;
static curl_slist_free_all_fn p_curl_slist_free_all = NULL;

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

static int load_symbol(void **out, const char *name) {
  *out = dlsym(curl_handle, name);
  return *out != NULL;
}

static int ensure_curl_loaded(void) {
  const char *names[] = {"libcurl.so.4", "libcurl.so", NULL};
  int i;

  if (curl_handle != NULL)
    return 1;

  for (i = 0; names[i] != NULL; ++i) {
    curl_handle = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (curl_handle != NULL)
      break;
  }
  if (curl_handle == NULL)
    return 0;

  if (!load_symbol((void **)&p_curl_global_init, "curl_global_init") ||
      !load_symbol((void **)&p_curl_global_cleanup, "curl_global_cleanup") ||
      !load_symbol((void **)&p_curl_easy_init, "curl_easy_init") ||
      !load_symbol((void **)&p_curl_easy_cleanup, "curl_easy_cleanup") ||
      !load_symbol((void **)&p_curl_easy_setopt, "curl_easy_setopt") ||
      !load_symbol((void **)&p_curl_easy_perform, "curl_easy_perform") ||
      !load_symbol((void **)&p_curl_easy_strerror, "curl_easy_strerror") ||
      !load_symbol((void **)&p_curl_slist_append, "curl_slist_append") ||
      !load_symbol((void **)&p_curl_slist_free_all, "curl_slist_free_all")) {
    dlclose(curl_handle);
    curl_handle = NULL;
    return 0;
  }

  if (!curl_initialized) {
    if (p_curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK) {
      dlclose(curl_handle);
      curl_handle = NULL;
      return 0;
    }
    curl_initialized = 1;
  }

  return 1;
}

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
    return make_error_status_message("failed to load libcurl");

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

ptr chezpp_net_ftp_download(const char *url, const char *dest, const char *user, const char *pass,
                            int passive, int timeout_ms, int use_tls, int verify_peer,
                            int verify_host) {
  CURL *curl;
  CURLcode rc;
  FILE *fp = NULL;
  ptr result;

  if (!ensure_curl_loaded())
    return make_error_status_message("failed to load libcurl");

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
    return make_error_status_message("failed to load libcurl");

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
    return make_error_status_message("failed to load libcurl");

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
    return make_error_status_message("failed to load libcurl");

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
