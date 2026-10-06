#include "../build-config.h"
#include "../common.h"
#include "../openssl_loader.h"

#if CHEZPP_WITH_OPENSSL
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#endif

typedef struct {
  SSL_CTX *ctx;
  int mode;
  int verify_peer;
  unsigned char *alpn;
  unsigned int alpn_len;
  SSL_SESSION *imported_session;
  int ocsp_policy;
  int sni_enabled;
} chezpp_tls_context;

typedef struct {
  SSL *ssl;
  int fd;
  int mode;
  int saved_flags;
  int flags_changed;
  int handshake_complete;
  int sni_resolved;
  char *pending_sni;
  int ocsp_policy;
} chezpp_tls_session;

static int client_hello_cb(SSL *ssl, int *alert, void *arg) {
  chezpp_tls_session *session =
      (chezpp_tls_session *)SSL_get_ex_data(ssl, 0);
  const unsigned char *extension = NULL;
  size_t extension_len = 0;
  size_t name_len;
  (void)alert;
  (void)arg;
  if (session == NULL || session->sni_resolved) return SSL_CLIENT_HELLO_SUCCESS;
  if (SSL_client_hello_get0_ext(
          ssl, TLSEXT_TYPE_server_name, &extension, &extension_len) != 1 ||
      extension_len < 5 || extension[2] != TLSEXT_NAMETYPE_host_name) {
    session->sni_resolved = 1;
    return SSL_CLIENT_HELLO_SUCCESS;
  }
  name_len = ((size_t)extension[3] << 8) | (size_t)extension[4];
  if (name_len == 0 || name_len > extension_len - 5) {
    session->sni_resolved = 1;
    return SSL_CLIENT_HELLO_SUCCESS;
  }
  if (session->pending_sni == NULL) {
    session->pending_sni = (char *)malloc(name_len + 1);
    if (session->pending_sni == NULL) return SSL_CLIENT_HELLO_ERROR;
    memcpy(session->pending_sni, extension + 5, name_len);
    session->pending_sni[name_len] = '\0';
  }
  return SSL_CLIENT_HELLO_RETRY;
}

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) { return make_status("error", Sstring(msg)); }

static ptr make_errno_status(const char *tag) { return make_status(tag, errno_str()); }

static int ensure_tls_init(void) {
  return chezpp_openssl_require();
}

ptr chezpp_net_tls_load_error(void) {
  const char *error;

  if (ensure_tls_init()) return Sfalse;
  error = chezpp_optional_library_error(
      (chezpp_optional_library *)chezpp_openssl_library());
  return error == NULL || error[0] == '\0' ? Sfalse : Sstring(error);
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

static long get_stapled_ocsp(SSL *ssl, const unsigned char **response) {
  *response = NULL;
  return SSL_ctrl(ssl,
                                 SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP,
                                 0, (void *)response);
}

static ptr validate_stapled_ocsp(chezpp_tls_session *session) {
  const unsigned char *response_bytes;
  const unsigned char *cursor;
  long response_len;
  OCSP_RESPONSE *response = NULL;
  OCSP_BASICRESP *basic = NULL;
  OCSP_CERTID *id = NULL;
  STACK_OF(X509) *chain;
  X509 *leaf = NULL;
  X509 *issuer = NULL;
  ASN1_GENERALIZEDTIME *revocation_time = NULL;
  ASN1_GENERALIZEDTIME *this_update = NULL;
  ASN1_GENERALIZEDTIME *next_update = NULL;
  int certificate_status;
  int revocation_reason;
  int signature_valid;
  int time_valid;
  ptr result = Sfalse;

  response_len = get_stapled_ocsp(session->ssl, &response_bytes);
  if (response_len <= 0 || response_bytes == NULL) return Sfalse;
  cursor = response_bytes;
  response = d2i_OCSP_RESPONSE(NULL, &cursor, response_len);
  if (response == NULL || cursor != response_bytes + response_len) {
    result = make_error_status_message("malformed stapled OCSP response");
    goto done;
  }
  if (OCSP_response_status(response) !=
      OCSP_RESPONSE_STATUS_SUCCESSFUL) {
    result = make_error_status_message("unsuccessful stapled OCSP response");
    goto done;
  }
  basic = OCSP_response_get1_basic(response);
  if (basic == NULL) {
    result = make_error_status_message("stapled OCSP response has no basic response");
    goto done;
  }
  leaf = SSL_get1_peer_certificate(session->ssl);
  chain = SSL_get_peer_cert_chain(session->ssl);
  if (leaf == NULL || chain == NULL) {
    result = make_error_status_message("cannot match stapled OCSP response to peer chain");
    goto done;
  }
  if (OPENSSL_sk_num((const OPENSSL_STACK *)chain) > 1)
    issuer = (X509 *)OPENSSL_sk_value(
        (const OPENSSL_STACK *)chain, 1);
  else
    issuer = leaf;
  id = OCSP_cert_to_id(NULL, leaf, issuer);
  if (id == NULL ||
      OCSP_resp_find_status(
          basic, id, &certificate_status, &revocation_reason,
          &revocation_time, &this_update, &next_update) != 1) {
    result = make_error_status_message("stapled OCSP response does not match peer certificate");
    goto done;
  }
  signature_valid = OCSP_basic_verify(
      basic, chain,
      SSL_CTX_get_cert_store(
          SSL_get_SSL_CTX(session->ssl)),
      0) == 1;
  time_valid = OCSP_check_validity(this_update, next_update,
                                                   300L, -1L) == 1;
  if (!signature_valid) {
    result = make_error_status_message("stapled OCSP signature verification failed");
    goto done;
  }
  if (!time_valid) {
    result = make_error_status_message("stapled OCSP response is outside its validity interval");
    goto done;
  }
  if (certificate_status != V_OCSP_CERTSTATUS_GOOD) {
    result = make_error_status_message(
        certificate_status == V_OCSP_CERTSTATUS_REVOKED
            ? "stapled OCSP response reports certificate revoked"
            : "stapled OCSP response reports certificate status unknown");
    goto done;
  }
  result = Smake_vector(3, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol("good"));
  Svector_set(result, 1, Strue);
  Svector_set(result, 2, Strue);

done:
  if (id != NULL) OCSP_CERTID_free(id);
  if (leaf != NULL) X509_free(leaf);
  if (basic != NULL) OCSP_BASICRESP_free(basic);
  if (response != NULL) OCSP_RESPONSE_free(response);
  return result;
}

void *chezpp_net_tls_context_native(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  return ctx == NULL ? NULL : ctx->ctx;
}

int chezpp_net_tls_context_verifies_peer(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  return ctx != NULL && ctx->verify_peer;
}

int chezpp_net_tls_context_copy_credentials(uptr handle, void *destination) {
  chezpp_tls_context *source = ctx_from_handle(handle);
  SSL_CTX *target = (SSL_CTX *)destination;
  X509 *certificate;
  EVP_PKEY *private_key;

  if (source == NULL || source->ctx == NULL || target == NULL) return 0;
  certificate = SSL_CTX_get0_certificate(source->ctx);
  private_key = SSL_CTX_get0_privatekey(source->ctx);
  if (certificate == NULL || private_key == NULL) return 0;
  return SSL_CTX_use_certificate(target, certificate) == 1 &&
         SSL_CTX_use_PrivateKey(target, private_key) == 1 &&
         SSL_CTX_check_private_key(target) == 1;
}

uptr chezpp_net_tls_context_create(int mode) {
  const SSL_METHOD *method;
  SSL_CTX *ctx;
  chezpp_tls_context *wrapper;

  if (!ensure_tls_init()) return 0;
  method = mode == 1 ? TLS_server_method() : TLS_client_method();
  ctx = SSL_CTX_new(method);
  if (ctx == NULL) return 0;
  SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION,
                              TLS1_2_VERSION, NULL);
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
  wrapper->verify_peer = mode == 0;
  return (uptr)wrapper;
}

void chezpp_net_tls_context_free(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL) return;
  if (ctx->ctx != NULL) SSL_CTX_free(ctx->ctx);
  if (ctx->imported_session != NULL)
    SSL_SESSION_free(ctx->imported_session);
  if (ctx->alpn != NULL) free(ctx->alpn);
  free(ctx);
}

ptr chezpp_net_tls_context_set_policy(uptr handle, int minimum_version,
                                      int maximum_version,
                                      const char *cipher_list,
                                      const char *ciphersuites,
                                      int ocsp_policy) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL)
    return make_error_status_message("invalid TLS context");
  if (SSL_CTX_ctrl(ctx->ctx, SSL_CTRL_SET_MIN_PROTO_VERSION,
                                 minimum_version, NULL) != 1)
    return openssl_error_status("failed to set minimum TLS version");
  if (SSL_CTX_ctrl(ctx->ctx, SSL_CTRL_SET_MAX_PROTO_VERSION,
                                 maximum_version, NULL) != 1)
    return openssl_error_status("failed to set maximum TLS version");
  if (cipher_list[0] != '\0' &&
      SSL_CTX_set_cipher_list(ctx->ctx, cipher_list) != 1)
    return openssl_error_status("failed to set TLS 1.2 cipher list");
  if (ciphersuites[0] != '\0' &&
      SSL_CTX_set_ciphersuites(ctx->ctx, ciphersuites) != 1)
    return openssl_error_status("failed to set TLS 1.3 ciphersuites");
  ctx->ocsp_policy = ocsp_policy;
  return Strue;
}

ptr chezpp_net_tls_context_enable_sni(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL)
    return make_error_status_message("invalid TLS context");
  if (ctx->mode != 1)
    return make_error_status_message("SNI selection requires a server TLS context");
  SSL_CTX_set_client_hello_cb(ctx->ctx, client_hello_cb, ctx);
  ctx->sni_enabled = 1;
  return Strue;
}

ptr chezpp_net_tls_context_import_session(uptr handle, ptr bv, int start,
                                          int stop) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  const unsigned char *cursor;
  SSL_SESSION *session;
  if (ctx == NULL || ctx->ctx == NULL)
    return make_error_status_message("invalid TLS context");
  cursor = Sbytevector_data(bv) + start;
  session = d2i_SSL_SESSION(NULL, &cursor, stop - start);
  if (session == NULL || cursor != Sbytevector_data(bv) + stop) {
    if (session != NULL) SSL_SESSION_free(session);
    return openssl_error_status("invalid serialized TLS session");
  }
  if (ctx->imported_session != NULL)
    SSL_SESSION_free(ctx->imported_session);
  ctx->imported_session = session;
  return Strue;
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
  ctx->verify_peer = verify_mode != 0;
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
  int saved_flags = 0;
  int changed = 0;

  (void)timeout_ms;

  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (!set_socket_nonblocking_temporarily(fd, &saved_flags, &changed)) return make_errno_status("error");
  ssl = SSL_new(ctx->ctx);
  if (ssl == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    return openssl_error_status("failed to create TLS session");
  }
  if (ctx->imported_session != NULL &&
      SSL_set_session(ssl, ctx->imported_session) != 1) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return openssl_error_status("failed to import TLS session");
  }
  if (ctx->ocsp_policy != 0 &&
      SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE,
                              TLSEXT_STATUSTYPE_ocsp, NULL) != 1) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return openssl_error_status("failed to request stapled OCSP response");
  }
  if (server_name != NULL && server_name[0] != '\0') {
    if (SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME,
                                TLSEXT_NAMETYPE_host_name,
                                (void *)server_name) != 1) {
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

  session = (chezpp_tls_session *)calloc(1, sizeof(chezpp_tls_session));
  if (session == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return make_errno_status("error");
  }
  session->ssl = ssl;
  session->fd = fd;
  session->mode = 0;
  session->saved_flags = saved_flags;
  session->flags_changed = changed;
  session->sni_resolved = 1;
  session->ocsp_policy = ctx->ocsp_policy;
  return Sunsigned((uptr)session);
}

ptr chezpp_net_tls_accept(uptr handle, int fd, int timeout_ms) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  SSL *ssl;
  chezpp_tls_session *session;
  int saved_flags = 0;
  int changed = 0;

  (void)timeout_ms;

  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
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

  session = (chezpp_tls_session *)calloc(1, sizeof(chezpp_tls_session));
  if (session == NULL) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return make_errno_status("error");
  }
  session->ssl = ssl;
  session->fd = fd;
  session->mode = 1;
  session->saved_flags = saved_flags;
  session->flags_changed = changed;
  if (ctx->sni_enabled)
    (void)SSL_set_ex_data(ssl, 0, session);
  return Sunsigned((uptr)session);
}

ptr chezpp_net_tls_handshake_step(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  int rc;
  ptr status;

  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  if (session->handshake_complete) return Strue;

  rc = session->mode == 1 ? SSL_accept(session->ssl)
                          : SSL_connect(session->ssl);
  if (rc == 1) {
    if (session->mode == 0 && session->ocsp_policy != 0) {
      const unsigned char *ocsp_response;
      long ocsp_len = get_stapled_ocsp(session->ssl, &ocsp_response);
      if (ocsp_len <= 0 || ocsp_response == NULL) {
        if (session->ocsp_policy == 2)
          return make_error_status_message("required stapled OCSP response is missing");
      } else {
        ptr ocsp_result = validate_stapled_ocsp(session);
        if (is_vector_status(ocsp_result, "error")) return ocsp_result;
      }
    }
    session->handshake_complete = 1;
    restore_socket_flags(session->fd, session->saved_flags, session->flags_changed);
    session->flags_changed = 0;
    return Strue;
  }

  if (session->mode == 1 &&
      SSL_get_error(session->ssl, rc) ==
          SSL_ERROR_WANT_CLIENT_HELLO_CB) {
    return make_status("sni", session->pending_sni == NULL
                                  ? Sfalse
                                  : Sstring_utf8(session->pending_sni,
                                                 (iptr)strlen(session->pending_sni)));
  }

  if (session->mode == 0) {
    long verify_result = SSL_get_verify_result(session->ssl);
    if (verify_result != X509_V_OK) {
      restore_socket_flags(session->fd, session->saved_flags, session->flags_changed);
      session->flags_changed = 0;
      return make_error_status_message(
          X509_verify_cert_error_string(verify_result));
    }
  }

  status = ssl_result_status(session->ssl, rc, session->mode == 1
                                                   ? "TLS server handshake failed"
                                                   : "TLS client handshake failed");
  if (!is_would_block_status(status)) {
    restore_socket_flags(session->fd, session->saved_flags, session->flags_changed);
    session->flags_changed = 0;
  }
  return status;
}

ptr chezpp_net_tls_close(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  if (session == NULL) return Strue;
  restore_socket_flags(session->fd, session->saved_flags, session->flags_changed);
  if (session->ssl != NULL) SSL_free(session->ssl);
  free(session->pending_sni);
  free(session);
  return Strue;
}

ptr chezpp_net_tls_session_select_context(uptr session_handle,
                                          uptr context_handle) {
  chezpp_tls_session *session = session_from_handle(session_handle);
  chezpp_tls_context *ctx = ctx_from_handle(context_handle);
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  if (ctx == NULL || ctx->ctx == NULL || ctx->mode != 1)
    return make_error_status_message("invalid selected server TLS context");
  if (SSL_set_SSL_CTX(session->ssl, ctx->ctx) == NULL)
    return openssl_error_status("failed to select SNI TLS context");
  session->sni_resolved = 1;
  free(session->pending_sni);
  session->pending_sni = NULL;
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
  if (!nonblocking && timeout_ms >= 0 && !current_time_ms(&start_ms))
    return make_errno_status("error");
  if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
    return make_errno_status("error");

  bv = Smake_bytevector((iptr)size, 0);
  for (;;) {
    rc = SSL_read_ex(session->ssl, Sbytevector_data(bv), (size_t)size, &nread);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS read failed");
      if (Svectorp(status) && Svector_length(status) == 2 &&
          Svector_ref(status, 0) == Sstring_to_symbol("closed")) {
        restore_socket_flags(session->fd, saved_flags, changed);
        return Seof_object;
      }
      if (nonblocking || !is_would_block_status(status)) {
        restore_socket_flags(session->fd, saved_flags, changed);
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
  restore_socket_flags(session->fd, saved_flags, changed);
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
  if (!nonblocking && timeout_ms >= 0 && !current_time_ms(&start_ms))
    return make_errno_status("error");
  if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
    return make_errno_status("error");
  for (;;) {
    rc = SSL_read_ex(session->ssl, Sbytevector_data(bv) + start, (size_t)(stop - start), &nread);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS read failed");
      if (Svectorp(status) && Svector_length(status) == 2 &&
          Svector_ref(status, 0) == Sstring_to_symbol("closed")) {
        restore_socket_flags(session->fd, saved_flags, changed);
        return Seof_object;
      }
      if (nonblocking || !is_would_block_status(status)) {
        restore_socket_flags(session->fd, saved_flags, changed);
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
  restore_socket_flags(session->fd, saved_flags, changed);
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
  if (!nonblocking && timeout_ms >= 0 && !current_time_ms(&start_ms))
    return make_errno_status("error");
  if (!set_socket_nonblocking_temporarily(session->fd, &saved_flags, &changed))
    return make_errno_status("error");
  for (;;) {
    rc = SSL_write_ex(session->ssl, Sbytevector_data(bv) + start, (size_t)(stop - start),
                      &nwritten);
    if (rc == 1) break;
    {
      ptr status = ssl_result_status(session->ssl, rc, "TLS write failed");
      if (nonblocking || !is_would_block_status(status)) {
        restore_socket_flags(session->fd, saved_flags, changed);
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
  restore_socket_flags(session->fd, saved_flags, changed);
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

ptr chezpp_net_tls_negotiated_alpn(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  const unsigned char *selected = NULL;
  unsigned int selected_len = 0;
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  SSL_get0_alpn_selected(session->ssl, &selected, &selected_len);
  if (selected == NULL || selected_len == 0) return Sfalse;
  return Sstring_utf8((const char *)selected, (iptr)selected_len);
}

ptr chezpp_net_tls_cipher_name(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  const char *name;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  name = SSL_CIPHER_get_name(
      SSL_get_current_cipher(session->ssl));
  return name == NULL ? Sfalse : Sstring(name);
}

ptr chezpp_net_tls_verified(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  long result;
  if (session == NULL || session->ssl == NULL) return make_error_status_message("invalid TLS session");
  result = SSL_get_verify_result(session->ssl);
  return Sboolean(result == X509_V_OK);
}

ptr chezpp_net_tls_session_export(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  SSL_SESSION *native_session;
  unsigned char *cursor;
  int length;
  ptr out;
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  native_session = SSL_get1_session(session->ssl);
  if (native_session == NULL)
    return make_error_status_message("TLS session is not resumable");
  length = i2d_SSL_SESSION(native_session, NULL);
  if (length <= 0) {
    SSL_SESSION_free(native_session);
    return openssl_error_status("failed to serialize TLS session");
  }
  out = Smake_bytevector((iptr)length, 0);
  cursor = Sbytevector_data(out);
  if (i2d_SSL_SESSION(native_session, &cursor) != length) {
    SSL_SESSION_free(native_session);
    return openssl_error_status("failed to serialize TLS session");
  }
  SSL_SESSION_free(native_session);
  return out;
}

ptr chezpp_net_tls_session_reused(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  return Sboolean(SSL_session_reused(session->ssl) == 1);
}

ptr chezpp_net_tls_stapled_ocsp(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  const unsigned char *response;
  long response_len;
  ptr out;
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  response_len = get_stapled_ocsp(session->ssl, &response);
  if (response_len <= 0 || response == NULL) return Sfalse;
  out = Smake_bytevector((iptr)response_len, 0);
  memcpy(Sbytevector_data(out), response, (size_t)response_len);
  return out;
}

ptr chezpp_net_tls_ocsp_result(uptr handle) {
  chezpp_tls_session *session = session_from_handle(handle);
  if (session == NULL || session->ssl == NULL)
    return make_error_status_message("invalid TLS session");
  return validate_stapled_ocsp(session);
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

  for (i = 0;
       i < OPENSSL_sk_num((const OPENSSL_STACK *)chain);
       i += 1) {
    X509 *cert = (X509 *)OPENSSL_sk_value(
        (const OPENSSL_STACK *)chain, i);
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
