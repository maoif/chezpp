#include "build-config.h"
#include "net/unavailable.h"
#if CHEZPP_WITH_BLAKE3
#include <blake3.h>
#endif
#include "openssl_loader.h"
#if CHEZPP_WITH_OPENSSL
#include <openssl/evp.h>
#endif
#include <stdlib.h>

//=======================================================================
// blake3
//=======================================================================

#if CHEZPP_WITH_BLAKE3
ptr digest_blake3_bv(ptr bv, int start, int stop) {
  ptr bvdata = Sbytevector_data(bv);
  uint8_t out[BLAKE3_OUT_LEN];

  blake3_hasher hasher;
  // TODO keyed
  blake3_hasher_init(&hasher);
  blake3_hasher_update(&hasher, bvdata + start, stop - start);
  blake3_hasher_finalize(&hasher, out, BLAKE3_OUT_LEN);

  ptr bv_out = Smake_bytevector(BLAKE3_OUT_LEN, 0);
  for (unsigned int i = 0; i < BLAKE3_OUT_LEN; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

ptr digest_blake3_str(ptr str, int start, int stop) {
  uint8_t out[BLAKE3_OUT_LEN];

  blake3_hasher hasher;
  // TODO keyed
  blake3_hasher_init(&hasher);
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(str, i);
    blake3_hasher_update(&hasher, &c, sizeof(c));
  }
  blake3_hasher_finalize(&hasher, out, BLAKE3_OUT_LEN);

  ptr bv_out = Smake_bytevector(BLAKE3_OUT_LEN, 0);
  for (unsigned int i = 0; i < BLAKE3_OUT_LEN; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

//=======================================================================
// openssl
//=======================================================================

//// bytevectors

#endif
#if CHEZPP_WITH_OPENSSL
static ptr do_digest_bv(const EVP_MD *md, ptr bv, int start, int stop) {
  EVP_MD_CTX *ctx;
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int outlen;
  ptr bvdata = Sbytevector_data(bv) + start;

  if (md == NULL) return Sfalse;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) return Sfalse;
  // TODO optimize?
  EVP_DigestInit_ex(ctx, md, NULL);
  EVP_DigestUpdate(ctx, bvdata, stop - start);
  EVP_DigestFinal_ex(ctx, out, &outlen);

  EVP_MD_CTX_free(ctx);

  ptr bv_out = Smake_bytevector(outlen, 0);
  for (unsigned int i = 0; i < outlen; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

ptr digest_md5_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_md5(), bv, start, stop);
}

ptr digest_sha224_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha224(), bv, start, stop);
}

ptr digest_sha256_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha256(), bv, start, stop);
}

ptr digest_sha384_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha384(), bv, start, stop);
}

ptr digest_sha512_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha512(), bv, start, stop);
}

ptr digest_sha512_224_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha512_224(), bv, start, stop);
}

ptr digest_sha512_256_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha512_256(), bv, start, stop);
}

ptr digest_sha3_224_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha3_224(), bv, start, stop);
}

ptr digest_sha3_256_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha3_256(), bv, start, stop);
}

ptr digest_sha3_384_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha3_384(), bv, start, stop);
}

ptr digest_sha3_512_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_sha3_512(), bv, start, stop);
}

ptr digest_blake2b512_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_blake2b512(), bv, start, stop);
}

ptr digest_blake2s256_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_blake2s256(), bv, start, stop);
}

ptr digest_shake128_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_shake128(), bv, start, stop);
}

ptr digest_shake256_bv(ptr bv, int start, int stop) {
  return do_digest_bv(EVP_shake256(), bv, start, stop);
}

//// strings (UTF-32)

static ptr do_digest_str(const EVP_MD *md, const ptr str, int start, int stop) {
  EVP_MD_CTX *ctx;
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int outlen;

  if (md == NULL) return Sfalse;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) return Sfalse;
  EVP_DigestInit_ex(ctx, md, NULL);
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(str, i);
    EVP_DigestUpdate(ctx, &c, sizeof(c));
  }
  EVP_DigestFinal_ex(ctx, out, &outlen);
  EVP_MD_CTX_free(ctx);

  ptr bv_out = Smake_bytevector(outlen, 0);
  for (unsigned int i = 0; i < outlen; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

ptr digest_md5_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_md5(), str, start, stop);
}

ptr digest_sha224_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha224(), str, start, stop);
}

ptr digest_sha256_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha256(), str, start, stop);
}

ptr digest_sha384_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha384(), str, start, stop);
}

ptr digest_sha512_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha512(), str, start, stop);
}

ptr digest_sha512_224_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha512_224(), str, start, stop);
}

ptr digest_sha512_256_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha512_256(), str, start, stop);
}

ptr digest_sha3_224_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha3_224(), str, start, stop);
}

ptr digest_sha3_256_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha3_256(), str, start, stop);
}

ptr digest_sha3_384_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha3_384(), str, start, stop);
}

ptr digest_sha3_512_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_sha3_512(), str, start, stop);
}

ptr digest_blake2b512_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_blake2b512(), str, start, stop);
}

ptr digest_blake2s256_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_blake2s256(), str, start, stop);
}

ptr digest_shake128_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_shake128(), str, start, stop);
}

ptr digest_shake256_str(ptr str, int start, int stop) {
  return do_digest_str(EVP_shake256(), str, start, stop);
}

//=======================================================================
// blake3 incremental
//=======================================================================

// TODO keyed
#endif
#if CHEZPP_WITH_BLAKE3
void *digester_blake3_create() {
  blake3_hasher *digester = (blake3_hasher *)malloc(sizeof(blake3_hasher));
  blake3_hasher_init(digester);
  return (void *)digester;
}

ptr digester_blake3_get(void *ptr_ctx) {
  blake3_hasher *ctx = (blake3_hasher *)ptr_ctx;
  uint8_t out[BLAKE3_OUT_LEN];
  blake3_hasher_finalize(ctx, out, sizeof(out));

  ptr bv_out = Smake_bytevector(BLAKE3_OUT_LEN, 0);
  for (unsigned int i = 0; i < BLAKE3_OUT_LEN; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

void digester_blake3_update_string(void *ptr_ctx, ptr str, int start,
                                   int stop) {
  blake3_hasher *ctx = (blake3_hasher *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    unsigned int c = Sstring_ref(str, i);
    blake3_hasher_update(ctx, &c, sizeof(c));
  }
}

void digester_blake3_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                       int stop) {
  blake3_hasher *ctx = (blake3_hasher *)ptr_ctx;
  blake3_hasher_update(ctx, Sbytevector_data(bv) + start, stop - start);
}

ptr digester_blake3_finalize(void *ptr_ctx) {
  blake3_hasher *ctx = (blake3_hasher *)ptr_ctx;
  uint8_t out[BLAKE3_OUT_LEN];
  blake3_hasher_finalize(ctx, out, sizeof(out));
  free(ctx);

  ptr bv_out = Smake_bytevector(BLAKE3_OUT_LEN, 0);
  for (unsigned int i = 0; i < BLAKE3_OUT_LEN; i++) {
    Sbytevector_u8_set(bv_out, i, out[i]);
  }
  return bv_out;
}

void digester_blake3_reset(void *ptr_ctx) { blake3_hasher_reset(ptr_ctx); }

//=======================================================================
// openssl incremental
//=======================================================================

#endif
#if CHEZPP_WITH_OPENSSL
void *digester_openssl_create(ptr which) {
  EVP_MD_CTX *ctx;

  if (!chezpp_openssl_require()) return NULL;
  ctx = EVP_MD_CTX_new();
  if (ctx == NULL) return NULL;
  // TODO optimize?
  const EVP_MD *md = NULL;
  if (which == Sstring_to_symbol("md5")) {
    md = EVP_md5();
  } else if (which == Sstring_to_symbol("sha224")) {
    md = EVP_sha224();
  } else if (which == Sstring_to_symbol("sha256")) {
    md = EVP_sha256();
  } else if (which == Sstring_to_symbol("sha384")) {
    md = EVP_sha384();
  } else if (which == Sstring_to_symbol("sha512")) {
    md = EVP_sha512();
  } else if (which == Sstring_to_symbol("sha512-224")) {
    md = EVP_sha512_224();
  } else if (which == Sstring_to_symbol("sha512-256")) {
    md = EVP_sha512_256();
  } else if (which == Sstring_to_symbol("sha3-224")) {
    md = EVP_sha3_224();
  } else if (which == Sstring_to_symbol("sha3-256")) {
    md = EVP_sha3_256();
  } else if (which == Sstring_to_symbol("sha3-384")) {
    md = EVP_sha3_384();
  } else if (which == Sstring_to_symbol("sha3-512")) {
    md = EVP_sha3_512();
  } else if (which == Sstring_to_symbol("blake2b-512")) {
    md = EVP_blake2b512();
  } else if (which == Sstring_to_symbol("blake2s-256")) {
    md = EVP_blake2s256();
  } else if (which == Sstring_to_symbol("shake128")) {
    md = EVP_shake128();
  } else if (which == Sstring_to_symbol("shake256")) {
    md = EVP_shake256();
  } else {
    __builtin_unreachable();
  }

  EVP_DigestInit_ex(ctx, md, NULL);
  return ctx;
}

ptr digester_openssl_get(void *ptr_ctx) {
  EVP_MD_CTX *ctx = (EVP_MD_CTX *)ptr_ctx;
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int outlen;
  if (ctx == NULL) return Sfalse;
  EVP_DigestFinal_ex(ctx, out, &outlen);

  ptr bv = Smake_bytevector(outlen, 0);
  for (unsigned int i = 0; i < outlen; i++) {
    Sbytevector_u8_set(bv, i, out[i]);
  }
  return bv;
}

void digester_openssl_update_string(void *ptr_ctx, ptr str, int start,
                                    int stop) {
  EVP_MD_CTX *ctx = (EVP_MD_CTX *)ptr_ctx;
  if (ctx == NULL) return;
  for (int i = start; i < stop; i++) {
    unsigned int c = Sstring_ref(str, i);
    EVP_DigestUpdate(ctx, &c, sizeof(c));
  }
}

void digester_openssl_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                        int stop) {
  EVP_MD_CTX *ctx = (EVP_MD_CTX *)ptr_ctx;
  if (ctx == NULL) return;
  EVP_DigestUpdate(ctx, Sbytevector_data(bv) + start, stop - start);
}

ptr digester_openssl_finalize(void *ptr_ctx) {
  EVP_MD_CTX *ctx = (EVP_MD_CTX *)ptr_ctx;
  unsigned char out[EVP_MAX_MD_SIZE];
  unsigned int outlen;
  if (ctx == NULL) return Sfalse;
  EVP_DigestFinal_ex(ctx, out, &outlen);

  EVP_MD_CTX_free(ctx);

  ptr bv = Smake_bytevector(outlen, 0);
  for (unsigned int i = 0; i < outlen; i++) {
    Sbytevector_u8_set(bv, i, out[i]);
  }
  return bv;
}

void digester_openssl_reset(void *ptr_ctx) {
  if (ptr_ctx != NULL) EVP_MD_CTX_free(ptr_ctx);
}
#endif


#if !CHEZPP_WITH_BLAKE3
ptr digest_blake3_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("blake3: disabled at build time");
}

ptr digest_blake3_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("blake3: disabled at build time");
}

void * digester_blake3_create(void) {
  return 0;
}

ptr digester_blake3_get(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("blake3: disabled at build time");
}

void digester_blake3_update_string(void *ptr_ctx, ptr str, int start,
                                   int stop) {
  (void)ptr_ctx;
  (void)str;
  (void)start;
  (void)stop;
}

void digester_blake3_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                       int stop) {
  (void)ptr_ctx;
  (void)bv;
  (void)start;
  (void)stop;
}

ptr digester_blake3_finalize(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("blake3: disabled at build time");
}

void digester_blake3_reset(void *ptr_ctx) {
  (void)ptr_ctx;
}
#endif

#if !CHEZPP_WITH_OPENSSL
ptr digest_md5_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha224_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha256_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha384_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_224_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_256_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_224_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_256_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_384_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_512_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_blake2b512_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_blake2s256_bv(ptr bv, int start, int stop) {
  (void)bv;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_md5_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha224_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha256_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha384_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_224_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha512_256_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_224_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_256_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_384_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_sha3_512_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_blake2b512_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

ptr digest_blake2s256_str(ptr str, int start, int stop) {
  (void)str;
  (void)start;
  (void)stop;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

void * digester_openssl_create(ptr which) {
  (void)which;
  return 0;
}

ptr digester_openssl_get(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

void digester_openssl_update_string(void *ptr_ctx, ptr str, int start,
                                    int stop) {
  (void)ptr_ctx;
  (void)str;
  (void)start;
  (void)stop;
}

void digester_openssl_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                        int stop) {
  (void)ptr_ctx;
  (void)bv;
  (void)start;
  (void)stop;
}

ptr digester_openssl_finalize(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("openssl: disabled at build time");
}

void digester_openssl_reset(void *ptr_ctx) {
  (void)ptr_ctx;
}
#endif
