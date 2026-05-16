#include "../common.h"

#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>

typedef struct {
  SSL_CTX *ctx;
  int mode;
  unsigned char *alpn;
  unsigned int alpn_len;
} chezpp_tls_context;

typedef struct {
  SSL *ssl;
  int fd;
  int mode;
} chezpp_tls_session;

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) { return make_status("error", Sstring(msg)); }

static ptr make_errno_status(const char *tag) { return make_status(tag, errno_str()); }

static void ensure_tls_init() {
  static int initialized = 0;
  if (!initialized) {
    OPENSSL_init_ssl(0, NULL);
    initialized = 1;
  }
}

static ptr openssl_error_status(const char *fallback) {
  char buffer[256];
  unsigned long err = ERR_get_error();
  if (err != 0) {
    ERR_error_string_n(err, buffer, sizeof(buffer));
    return make_status("error", Sstring(buffer));
  }
  return make_error_status_message(fallback);
}

static ptr ssl_result_status(SSL *ssl, int rc, const char *fallback) {
  int err = SSL_get_error(ssl, rc);
  switch (err) {
  case SSL_ERROR_WANT_READ:
    return make_status("would-block-read", Sfalse);
  case SSL_ERROR_WANT_WRITE:
    return make_status("would-block-write", Sfalse);
  case SSL_ERROR_ZERO_RETURN:
    return make_status("closed", Sfalse);
  default:
    return openssl_error_status(fallback);
  }
}

static int current_time_ms(long long *out) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0;
  *out = ((long long)ts.tv_sec * 1000LL) + ((long long)ts.tv_nsec / 1000000LL);
  return 1;
}

static int remaining_timeout_ms(int timeout_ms, long long start_ms) {
  long long now;
  long long elapsed;
  long long remaining;

  if (timeout_ms < 0) return -1;
  if (!current_time_ms(&now)) return -2;
  elapsed = now - start_ms;
  if (elapsed < 0) elapsed = 0;
  remaining = (long long)timeout_ms - elapsed;
  if (remaining <= 0) return 0;
  if (remaining > (long long)INT_MAX) return INT_MAX;
  return (int)remaining;
}

static int set_socket_nonblocking_temporarily(int fd, int *saved_flags, int *changed) {
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0) return 0;
  *saved_flags = flags;
  *changed = (flags & O_NONBLOCK) == 0;
  if (*changed && fcntl(fd, F_SETFL, flags | O_NONBLOCK) != 0) return 0;
  return 1;
}

static void restore_socket_flags(int fd, int saved_flags, int changed) {
  if (changed) (void)fcntl(fd, F_SETFL, saved_flags);
}

static int wait_for_fd_event(int fd, int want_write, int timeout_ms) {
  struct pollfd pfd;
  int rc;

  memset(&pfd, 0, sizeof(pfd));
  pfd.fd = fd;
  pfd.events = want_write ? POLLOUT : POLLIN;
  rc = poll(&pfd, 1, timeout_ms);
  if (rc > 0) return 1;
  if (rc == 0) return 0;
  if (errno == EINTR) return wait_for_fd_event(fd, want_write, timeout_ms);
  return -1;
}

static ptr make_timeout_status(const char *message) { return make_error_status_message(message); }

static int is_vector_status(ptr value, const char *tag) {
  ptr sym;
  if (!Svectorp(value) || Svector_length(value) != 2) return 0;
  sym = Sstring_to_symbol(tag);
  return Svector_ref(value, 0) == sym;
}

static int is_would_block_status(ptr value) {
  return is_vector_status(value, "would-block-read") || is_vector_status(value, "would-block-write");
}

static ptr x509_to_der_bytevector(X509 *cert) {
  int len;
  unsigned char *buf = NULL;
  unsigned char *p = NULL;
  ptr out;

  if (cert == NULL) return Sfalse;
  len = i2d_X509(cert, NULL);
  if (len <= 0) return Sfalse;
  buf = (unsigned char *)malloc((size_t)len);
  if (buf == NULL) return make_errno_status("error");
  p = buf;
  if (i2d_X509(cert, &p) != len) {
    free(buf);
    return Sfalse;
  }
  out = Smake_bytevector((iptr)len, 0);
  memcpy(Sbytevector_data(out), buf, (size_t)len);
  free(buf);
  return out;
}

static int server_alpn_select_cb(SSL *ssl, const unsigned char **out, unsigned char *outlen,
                                 const unsigned char *in, unsigned int inlen, void *arg) {
  chezpp_tls_context *ctx = (chezpp_tls_context *)arg;
  unsigned char *selected = NULL;
  (void)ssl;

  if (ctx == NULL || ctx->alpn == NULL || ctx->alpn_len == 0) return SSL_TLSEXT_ERR_NOACK;
  if (SSL_select_next_proto(&selected, outlen, ctx->alpn, ctx->alpn_len, in, inlen) !=
      OPENSSL_NPN_NEGOTIATED)
    return SSL_TLSEXT_ERR_NOACK;
  *out = selected;
  return SSL_TLSEXT_ERR_OK;
}

static X509 *load_x509_from_memory(unsigned char *data, int len, int format) {
  BIO *bio = BIO_new_mem_buf(data, len);
  X509 *cert = NULL;
  if (bio == NULL) return NULL;
  if (format == 0)
    cert = PEM_read_bio_X509(bio, NULL, NULL, NULL);
  else
    cert = d2i_X509_bio(bio, NULL);
  BIO_free(bio);
  return cert;
}

static EVP_PKEY *load_pkey_from_memory(unsigned char *data, int len, int format) {
  BIO *bio = BIO_new_mem_buf(data, len);
  EVP_PKEY *pkey = NULL;
  if (bio == NULL) return NULL;
  if (format == 0)
    pkey = PEM_read_bio_PrivateKey(bio, NULL, NULL, NULL);
  else
    pkey = d2i_PrivateKey_bio(bio, NULL);
  BIO_free(bio);
  return pkey;
}

static chezpp_tls_context *ctx_from_handle(uptr handle) {
  return (chezpp_tls_context *)TO_VOIDP(handle);
}

static chezpp_tls_session *session_from_handle(uptr handle) {
  return (chezpp_tls_session *)TO_VOIDP(handle);
}

uptr chezpp_net_tls_context_create(int mode) {
  const SSL_METHOD *method;
  SSL_CTX *ctx;
  chezpp_tls_context *wrapper;

  ensure_tls_init();
  method = mode == 1 ? TLS_server_method() : TLS_client_method();
  ctx = SSL_CTX_new(method);
  if (ctx == NULL) return 0;
  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
  if (mode == 0) {
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
    (void)SSL_CTX_set_default_verify_paths(ctx);
  } else {
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
  }

  wrapper = (chezpp_tls_context *)calloc(1, sizeof(chezpp_tls_context));
  if (wrapper == NULL) {
    SSL_CTX_free(ctx);
    return 0;
  }
  wrapper->ctx = ctx;
  wrapper->mode = mode;
  return (uptr)wrapper;
}

void chezpp_net_tls_context_free(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL) return;
  if (ctx->ctx != NULL) SSL_CTX_free(ctx->ctx);
  if (ctx->alpn != NULL) free(ctx->alpn);
  free(ctx);
}

ptr chezpp_net_tls_context_load_ca_file(uptr handle, const char *path) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_load_verify_locations(ctx->ctx, path, NULL) != 1)
    return openssl_error_status("failed to load CA file");
  return Strue;
}

ptr chezpp_net_tls_context_load_ca_path(uptr handle, const char *path) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_load_verify_locations(ctx->ctx, NULL, path) != 1)
    return openssl_error_status("failed to load CA path");
  return Strue;
}

ptr chezpp_net_tls_context_load_default_ca(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_set_default_verify_paths(ctx->ctx) != 1)
    return openssl_error_status("failed to load default TLS verify paths");
  return Strue;
}

ptr chezpp_net_tls_context_load_cert_file(uptr handle, const char *path, int format) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  int rc;
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (format == 0)
    rc = SSL_CTX_use_certificate_chain_file(ctx->ctx, path);
  else
    rc = SSL_CTX_use_certificate_file(ctx->ctx, path, SSL_FILETYPE_ASN1);
  if (rc != 1) return openssl_error_status("failed to load TLS certificate");
  return Strue;
}

ptr chezpp_net_tls_context_load_cert_bytes(uptr handle, ptr bv, int start, int stop, int format) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  X509 *cert;
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  cert = load_x509_from_memory(Sbytevector_data(bv) + start, stop - start, format);
  if (cert == NULL) return openssl_error_status("failed to parse TLS certificate");
  if (SSL_CTX_use_certificate(ctx->ctx, cert) != 1) {
    X509_free(cert);
    return openssl_error_status("failed to install TLS certificate");
  }
  X509_free(cert);
  return Strue;
}

ptr chezpp_net_tls_context_load_key_file(uptr handle, const char *path, int format) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  int filetype = format == 0 ? SSL_FILETYPE_PEM : SSL_FILETYPE_ASN1;
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_use_PrivateKey_file(ctx->ctx, path, filetype) != 1)
    return openssl_error_status("failed to load TLS private key");
  return Strue;
}

ptr chezpp_net_tls_context_load_key_bytes(uptr handle, ptr bv, int start, int stop, int format) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  EVP_PKEY *pkey;
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  pkey = load_pkey_from_memory(Sbytevector_data(bv) + start, stop - start, format);
  if (pkey == NULL) return openssl_error_status("failed to parse TLS private key");
  if (SSL_CTX_use_PrivateKey(ctx->ctx, pkey) != 1) {
    EVP_PKEY_free(pkey);
    return openssl_error_status("failed to install TLS private key");
  }
  EVP_PKEY_free(pkey);
  return Strue;
}

ptr chezpp_net_tls_context_check_key(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_check_private_key(ctx->ctx) != 1)
    return openssl_error_status("TLS certificate/private-key mismatch");
  return Strue;
}

ptr chezpp_net_tls_context_set_verify(uptr handle, int verify_mode) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  SSL_CTX_set_verify(ctx->ctx, verify_mode ? SSL_VERIFY_PEER : SSL_VERIFY_NONE, NULL);
  return Sboolean(verify_mode);
}

ptr chezpp_net_tls_context_set_alpn(uptr handle, ptr bv, int start, int stop) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  unsigned char *copy = NULL;
  int len = stop - start;
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");

  if (ctx->alpn != NULL) {
    free(ctx->alpn);
    ctx->alpn = NULL;
    ctx->alpn_len = 0;
  }

  if (len == 0) return Strue;

  copy = (unsigned char *)malloc((size_t)len);
  if (copy == NULL) return make_errno_status("error");
  memcpy(copy, Sbytevector_data(bv) + start, (size_t)len);
  ctx->alpn = copy;
  ctx->alpn_len = (unsigned int)len;

  if (ctx->mode == 0) {
    if (SSL_CTX_set_alpn_protos(ctx->ctx, ctx->alpn, ctx->alpn_len) != 0)
      return openssl_error_status("failed to configure TLS ALPN");
  } else {
    SSL_CTX_set_alpn_select_cb(ctx->ctx, server_alpn_select_cb, ctx);
  }
  return Strue;
}

ptr chezpp_net_tls_connect(uptr handle, int fd, const char *server_name, int timeout_ms) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  SSL *ssl;
  chezpp_tls_session *session;
  int rc;
  int saved_flags = 0;
  int changed = 0;
  long long start_ms = 0;

  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return make_errno_status("error");
  if (!set_socket_nonblocking_temporarily(fd, &saved_flags, &changed)) return make_errno_status("error");
  ssl = SSL_new(ctx->ctx);
  if (ssl == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    return openssl_error_status("failed to create TLS session");
  }
  if (server_name != NULL && server_name[0] != '\0') {
    if (SSL_set_tlsext_host_name(ssl, server_name) != 1) {
      restore_socket_flags(fd, saved_flags, changed);
      SSL_free(ssl);
      return openssl_error_status("failed to configure TLS SNI");
    }
    if (X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(ssl), server_name) != 1 &&
        SSL_set1_host(ssl, server_name) != 1) {
      restore_socket_flags(fd, saved_flags, changed);
      SSL_free(ssl);
      return openssl_error_status("failed to configure TLS hostname verification");
    }
  }
  if (SSL_set_fd(ssl, fd) != 1) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return openssl_error_status("failed to attach TLS session to socket");
  }

  for (;;) {
    rc = SSL_connect(ssl);
    if (rc == 1) break;
    {
      int err = SSL_get_error(ssl, rc);
      int wait_rc;
      int remaining;
      switch (err) {
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
        if (timeout_ms >= 0) {
          remaining = remaining_timeout_ms(timeout_ms, start_ms);
          if (remaining < 0) {
            restore_socket_flags(fd, saved_flags, changed);
            SSL_free(ssl);
            return make_errno_status("error");
          }
          if (remaining == 0) {
            restore_socket_flags(fd, saved_flags, changed);
            SSL_free(ssl);
            return make_timeout_status("TLS client handshake timed out");
          }
        } else {
          remaining = -1;
        }
        wait_rc = wait_for_fd_event(fd, err == SSL_ERROR_WANT_WRITE, remaining);
        if (wait_rc > 0) continue;
        restore_socket_flags(fd, saved_flags, changed);
        SSL_free(ssl);
        if (wait_rc == 0) return make_timeout_status("TLS client handshake timed out");
        return make_errno_status("error");
      default: {
        long verify_result = SSL_get_verify_result(ssl);
        ptr status = verify_result == X509_V_OK
                         ? ssl_result_status(ssl, rc, "TLS client handshake failed")
                         : make_error_status_message(X509_verify_cert_error_string(verify_result));
        restore_socket_flags(fd, saved_flags, changed);
        SSL_free(ssl);
        return status;
      }
      }
    }
  }

  session = (chezpp_tls_session *)calloc(1, sizeof(chezpp_tls_session));
  if (session == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return make_errno_status("error");
  }
  session->ssl = ssl;
  session->fd = fd;
  session->mode = 0;
  restore_socket_flags(fd, saved_flags, changed);
  return Sunsigned((uptr)session);
}

ptr chezpp_net_tls_accept(uptr handle, int fd, int timeout_ms) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  SSL *ssl;
  chezpp_tls_session *session;
  int rc;
  int saved_flags = 0;
  int changed = 0;
  long long start_ms = 0;

  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return make_errno_status("error");
  if (!set_socket_nonblocking_temporarily(fd, &saved_flags, &changed)) return make_errno_status("error");
  ssl = SSL_new(ctx->ctx);
  if (ssl == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    return openssl_error_status("failed to create TLS session");
  }
  if (SSL_set_fd(ssl, fd) != 1) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return openssl_error_status("failed to attach TLS session to socket");
  }

  for (;;) {
    rc = SSL_accept(ssl);
    if (rc == 1) break;
    {
      int err = SSL_get_error(ssl, rc);
      int wait_rc;
      int remaining;
      switch (err) {
      case SSL_ERROR_WANT_READ:
      case SSL_ERROR_WANT_WRITE:
        if (timeout_ms >= 0) {
          remaining = remaining_timeout_ms(timeout_ms, start_ms);
          if (remaining < 0) {
            restore_socket_flags(fd, saved_flags, changed);
            SSL_free(ssl);
            return make_errno_status("error");
          }
          if (remaining == 0) {
            restore_socket_flags(fd, saved_flags, changed);
            SSL_free(ssl);
            return make_timeout_status("TLS server handshake timed out");
          }
        } else {
          remaining = -1;
        }
        wait_rc = wait_for_fd_event(fd, err == SSL_ERROR_WANT_WRITE, remaining);
        if (wait_rc > 0) continue;
        restore_socket_flags(fd, saved_flags, changed);
        SSL_free(ssl);
        if (wait_rc == 0) return make_timeout_status("TLS server handshake timed out");
        return make_errno_status("error");
      default: {
        ptr status = ssl_result_status(ssl, rc, "TLS server handshake failed");
        restore_socket_flags(fd, saved_flags, changed);
        SSL_free(ssl);
        return status;
      }
      }
    }
  }

  session = (chezpp_tls_session *)calloc(1, sizeof(chezpp_tls_session));
  if (session == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return make_errno_status("error");
  }
  session->ssl = ssl;
  session->fd = fd;
  session->mode = 1;
  restore_socket_flags(fd, saved_flags, changed);
  return Sunsigned((uptr)session);
}

ptr chezpp_net_tls_close(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  if (session == NULL) return Strue;
  if (session->ssl != NULL) SSL_free(session->ssl);
  free(session);
  return Strue;
}

ptr chezpp_net_tls_read(uptr handle, int size, int timeout_ms, int nonblocking) {
  chezpp_tls_session *session = session_from_handle(handle);
  ptr bv;
  size_t nread = 0;
  int rc;
  int saved_flags = 0;
  int changed = 0;
  long long start_ms = 0;

  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  if (size < 0) return make_error_status_message("invalid TLS read size");
  if (!nonblocking) {
    if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return make_errno_status("error");
    if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
      return make_errno_status("error");
  }

  bv = Smake_bytevector((iptr)size, 0);
  for (;;) {
    rc = SSL_read_ex(session->ssl, Sbytevector_data(bv), (size_t)size, &nread);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS read failed");
      if (Svectorp(status) && Svector_length(status) == 2 &&
          Svector_ref(status, 0) == Sstring_to_symbol("closed")) {
        if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
        return Seof_object;
      }
      if (nonblocking || !is_would_block_status(status)) {
        if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
        return status;
      }
      {
        int wait_rc;
        int remaining;
        int want_write = Svector_ref(status, 0) == Sstring_to_symbol("would-block-write");
        if (timeout_ms >= 0) {
          remaining = remaining_timeout_ms(timeout_ms, start_ms);
          if (remaining < 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_errno_status("error");
          }
          if (remaining == 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_timeout_status("TLS read timed out");
          }
        } else {
          remaining = -1;
        }
        wait_rc = wait_for_fd_event(session->fd, want_write, remaining);
        if (wait_rc > 0) continue;
        restore_socket_flags(session->fd, saved_flags, changed);
        if (wait_rc == 0) return make_timeout_status("TLS read timed out");
        return make_errno_status("error");
      }
    }
  }
  if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
  if (nread == 0) return Seof_object;
  if ((int)nread == size) return bv;

  {
    ptr out = Smake_bytevector((iptr)nread, 0);
    memcpy(Sbytevector_data(out), Sbytevector_data(bv), nread);
    return out;
  }
}

ptr chezpp_net_tls_read_into(uptr handle, ptr bv, int start, int stop, int timeout_ms,
                             int nonblocking) {
  chezpp_tls_session *session = session_from_handle(handle);
  size_t nread = 0;
  int rc;
  int saved_flags = 0;
  int changed = 0;
  long long start_ms = 0;

  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  if (!nonblocking) {
    if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return make_errno_status("error");
    if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
      return make_errno_status("error");
  }
  for (;;) {
    rc = SSL_read_ex(session->ssl, Sbytevector_data(bv) + start, (size_t)(stop - start), &nread);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS read failed");
      if (Svectorp(status) && Svector_length(status) == 2 &&
          Svector_ref(status, 0) == Sstring_to_symbol("closed")) {
        if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
        return Seof_object;
      }
      if (nonblocking || !is_would_block_status(status)) {
        if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
        return status;
      }
      {
        int wait_rc;
        int remaining;
        int want_write = Svector_ref(status, 0) == Sstring_to_symbol("would-block-write");
        if (timeout_ms >= 0) {
          remaining = remaining_timeout_ms(timeout_ms, start_ms);
          if (remaining < 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_errno_status("error");
          }
          if (remaining == 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_timeout_status("TLS read timed out");
          }
        } else {
          remaining = -1;
        }
        wait_rc = wait_for_fd_event(session->fd, want_write, remaining);
        if (wait_rc > 0) continue;
        restore_socket_flags(session->fd, saved_flags, changed);
        if (wait_rc == 0) return make_timeout_status("TLS read timed out");
        return make_errno_status("error");
      }
    }
  }
  if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
  if (nread == 0) return Seof_object;
  return Sfixnum((iptr)nread);
}

ptr chezpp_net_tls_write(uptr handle, ptr bv, int start, int stop, int timeout_ms,
                         int nonblocking) {
  chezpp_tls_session *session = session_from_handle(handle);
  size_t nwritten = 0;
  int rc;
  int saved_flags = 0;
  int changed = 0;
  long long start_ms = 0;

  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  if (!nonblocking) {
    if (timeout_ms >= 0 && !current_time_ms(&start_ms)) return make_errno_status("error");
    if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
      return make_errno_status("error");
  }
  for (;;) {
    rc = SSL_write_ex(session->ssl, Sbytevector_data(bv) + start, (size_t)(stop - start),
                      &nwritten);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS write failed");
      if (nonblocking || !is_would_block_status(status)) {
        if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
        return status;
      }
      {
        int wait_rc;
        int remaining;
        int want_write = Svector_ref(status, 0) == Sstring_to_symbol("would-block-write");
        if (timeout_ms >= 0) {
          remaining = remaining_timeout_ms(timeout_ms, start_ms);
          if (remaining < 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_errno_status("error");
          }
          if (remaining == 0) {
            restore_socket_flags(session->fd, saved_flags, changed);
            return make_timeout_status("TLS write timed out");
          }
        } else {
          remaining = -1;
        }
        wait_rc = wait_for_fd_event(session->fd, want_write, remaining);
        if (wait_rc > 0) continue;
        restore_socket_flags(session->fd, saved_flags, changed);
        if (wait_rc == 0) return make_timeout_status("TLS write timed out");
        return make_errno_status("error");
      }
    }
  }
  if (!nonblocking) restore_socket_flags(session->fd, saved_flags, changed);
  return Sfixnum((iptr)nwritten);
}

ptr chezpp_net_tls_shutdown(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  int rc;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  rc = SSL_shutdown(session->ssl);
  if (rc == 1) return Strue;
  if (rc == 0) {
    rc = SSL_shutdown(session->ssl);
    if (rc == 1 || rc == 0) return Strue;
  }
  return ssl_result_status(session->ssl, rc, "TLS shutdown failed");
}

ptr chezpp_net_tls_protocol_version(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  return Sstring(SSL_get_version(session->ssl));
}

ptr chezpp_net_tls_cipher_name(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  const char *name;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  name = SSL_get_cipher_name(session->ssl);
  return name == NULL ? Sfalse : Sstring(name);
}

ptr chezpp_net_tls_verified(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  long result;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  result = SSL_get_verify_result(session->ssl);
  return Sboolean(result == X509_V_OK);
}

ptr chezpp_net_tls_peer_certificate_der(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  X509 *cert;
  ptr out;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  cert = SSL_get1_peer_certificate(session->ssl);
  if (cert == NULL) return Sfalse;
  out = x509_to_der_bytevector(cert);
  X509_free(cert);
  return out;
}

ptr chezpp_net_tls_peer_certificate_chain_der(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  STACK_OF(X509) *chain;
  ptr head = Snil;
  ptr tail = Snil;
  int i;

  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  chain = SSL_get_peer_cert_chain(session->ssl);
  if (chain == NULL) return Snil;

  for (i = 0; i < sk_X509_num(chain); i += 1) {
    X509 *cert = sk_X509_value(chain, i);
    ptr der = x509_to_der_bytevector(cert);
    ptr cell = Scons(der, Snil);
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
