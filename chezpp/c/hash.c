#include "common.h"

#include <xxhash.h>

//=======================================================================
// xxhash
//=======================================================================

uint32_t hash_XXH32(ptr bv, uint32_t seed) {
  ptr bvdata = Sbytevector_data(bv);
  int len = Sbytevector_length(bv);
  return XXH32(bvdata, len, seed);
}

ptr hash_XXH64(ptr bv, uint64_t seed) {
  ptr bvdata = Sbytevector_data(bv);
  int len = Sbytevector_length(bv);
  return Sunsigned64(XXH64(bvdata, len, seed));
}

ptr hash_XXH3_64(ptr bv, uint64_t seed) {
  ptr bvdata = Sbytevector_data(bv);
  int len = Sbytevector_length(bv);
  return Sunsigned64(XXH3_64bits_withSeed(bvdata, len, seed));
}

// ptr hash_XXH3_128(ptr bv, uint64_t seed) {
//     ptr bvdata = Sbytevector_data(bv);
//     int len = Sbytevector_length(bv);
//     XXH128_hash_t h128 = XXH3_128bits_withSeed(bvdata, len, seed);
// }

//// xxh32

uint32_t hash_XXH32_fixnum(int64_t x, uint32_t salt) {
  return XXH32(&x, sizeof(x), salt);
}

uint32_t hash_XXH32_flonum(double x, uint32_t salt) {
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  return XXH32(&buf, sizeof(buf), salt);
}

uint32_t hash_XXH32_ratnum(int64_t x, int64_t y, uint32_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return XXH32(buf, sizeof(buf), salt);
}

uint32_t hash_XXH32_cflonum(double x, double y, uint32_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return XXH32(buf, sizeof(buf), salt);
}

uint32_t hash_XXH32_string(ptr x, int start, int stop, uint32_t salt) {
  XXH32_state_t *ctx = XXH32_createState();
  XXH32_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(x, i);
    XXH32_update(ctx, &c, sizeof(c));
  }
  uint32_t hash = XXH32_digest(ctx);
  XXH32_freeState(ctx);
  return hash;
}

uint32_t hash_XXH32_fxvector(ptr x, int start, int stop, uint32_t salt) {
  XXH32_state_t *ctx = XXH32_createState();
  XXH32_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH32_update(ctx, &c, sizeof(c));
  }
  uint32_t hash = XXH32_digest(ctx);
  XXH32_freeState(ctx);
  return hash;
}

uint32_t hash_XXH32_flvector(ptr x, int start, int stop, uint32_t salt) {
  XXH32_state_t *ctx = XXH32_createState();
  XXH32_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH32_update(ctx, &c, sizeof(c));
  }
  uint32_t hash = XXH32_digest(ctx);
  XXH32_freeState(ctx);
  return hash;
}

uint32_t hash_XXH32_bytevector(ptr x, int start, int stop, uint32_t salt) {
  ptr bvdata = Sbytevector_data(x);
  return XXH32(bvdata + start, stop - start, salt);
}

//// xxh64
// return type is ptr because retval might be bignum

ptr hash_XXH64_fixnum(int64_t x, uint64_t salt) {
  return Sunsigned64(XXH64(&x, sizeof(x), salt));
}

ptr hash_XXH64_flonum(double x, uint64_t salt) {
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  return Sunsigned64(XXH64(&buf, sizeof(buf), salt));
}

ptr hash_XXH64_ratnum(int64_t x, int64_t y, uint64_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return Sunsigned64(XXH64(buf, sizeof(buf), salt));
}

ptr hash_XXH64_cflonum(double x, double y, uint64_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return Sunsigned64(XXH64(buf, sizeof(buf), salt));
}

ptr hash_XXH64_string(ptr x, int start, int stop, uint64_t salt) {
  XXH64_state_t *ctx = XXH64_createState();
  XXH64_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(x, i);
    XXH64_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH64_digest(ctx);
  XXH64_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH64_fxvector(ptr x, int start, int stop, uint64_t salt) {
  XXH64_state_t *ctx = XXH64_createState();
  XXH64_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH64_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH64_digest(ctx);
  XXH64_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH64_flvector(ptr x, int start, int stop, uint64_t salt) {
  XXH64_state_t *ctx = XXH64_createState();
  XXH64_reset(ctx, salt);
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH64_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH64_digest(ctx);
  XXH64_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH64_bytevector(ptr x, int start, int stop, uint64_t salt) {
  ptr bvdata = Sbytevector_data(x);
  return Sunsigned64(XXH64(bvdata + start, stop - start, salt));
}

//// xxh3-64

ptr hash_XXH3_64_fixnum(int64_t x, uint64_t salt) {
  return Sunsigned64(XXH3_64bits_withSeed(&x, sizeof(x), salt));
}

ptr hash_XXH3_64_flonum(double x, uint64_t salt) {
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  return Sunsigned64(XXH3_64bits_withSeed(&buf, sizeof(buf), salt));
}

ptr hash_XXH3_64_ratnum(int64_t x, int64_t y, uint64_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return Sunsigned64(XXH3_64bits_withSeed(buf, sizeof(buf), salt));
}

ptr hash_XXH3_64_cflonum(double x, double y, uint64_t salt) {
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  return Sunsigned64(XXH3_64bits_withSeed(buf, sizeof(buf), salt));
}

ptr hash_XXH3_64_string(ptr x, int start, int stop, uint64_t salt) {
  XXH3_state_t *ctx = XXH3_createState();
  XXH3_64bits_reset_withSeed(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(x, i);
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH3_64bits_digest(ctx);
  XXH3_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH3_64_fxvector(ptr x, int start, int stop, uint64_t salt) {
  XXH3_state_t *ctx = XXH3_createState();
  XXH3_64bits_reset_withSeed(ctx, salt);
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH3_64bits_digest(ctx);
  XXH3_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH3_64_flvector(ptr x, int start, int stop, uint64_t salt) {
  XXH3_state_t *ctx = XXH3_createState();
  XXH3_64bits_reset_withSeed(ctx, salt);
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  uint64_t hash = XXH3_64bits_digest(ctx);
  XXH3_freeState(ctx);
  return Sunsigned64(hash);
}

ptr hash_XXH3_64_bytevector(ptr x, int start, int stop, uint64_t salt) {
  ptr bvdata = Sbytevector_data(x);
  return Sunsigned64(XXH3_64bits_withSeed(bvdata + start, stop - start, salt));
}

//=======================================================================
// xxhash incremental
//=======================================================================

//// xxh32

void *hasher_XXH32_create(uint32_t seed) {
  XXH32_state_t *ctx = XXH32_createState();
  XXH32_reset(ctx, seed);
  return (void *)ctx;
}

uint32_t hasher_XXH32_get(void *ptr_ctx) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  return XXH32_digest(ctx);
}

void hasher_XXH32_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  XXH32_update(ctx, &x, sizeof(x));
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_flonum(void *ptr_ctx, double x, int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  XXH32_update(ctx, buf, sizeof(buf));
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_ratnum(void *ptr_ctx, int64_t x, int64_t y, int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH32_update(ctx, buf, sizeof(buf));
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH32_update(ctx, buf, sizeof(buf));
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH32_update(ctx, &c, sizeof(c));
  }
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH32_update(ctx, &c, sizeof(c));
  }
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(str, i);
    XXH32_update(ctx, &c, sizeof(c));
  }
  XXH32_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH32_update_bytevector(void *ptr_ctx, ptr bv, int start, int stop,
                                    int tag) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  XXH32_update(ctx, Sbytevector_data(bv) + start, stop - start);
  XXH32_update(ctx, &tag, sizeof(tag));
}

uint32_t hasher_XXH32_finalize(void *ptr_ctx) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  uint32_t hash = XXH32_digest(ctx);
  XXH32_freeState(ctx);
  return hash;
}

void hasher_XXH32_reset(void *ptr_ctx, uint32_t seed) {
  XXH32_state_t *ctx = (XXH32_state_t *)ptr_ctx;
  XXH32_reset(ctx, seed);
}

//// xxh64

void *hasher_XXH64_create(uint64_t seed) {
  XXH64_state_t *ctx = XXH64_createState();
  XXH64_reset(ctx, seed);
  return (void *)ctx;
}

ptr hasher_XXH64_get(void *ptr_ctx) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  return Sunsigned64(XXH64_digest(ctx));
}

void hasher_XXH64_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  XXH64_update(ctx, &x, sizeof(x));
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_flonum(void *ptr_ctx, double x, int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  XXH64_update(ctx, buf, sizeof(buf));
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_ratnum(void *ptr_ctx, int64_t x, int64_t y, int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH64_update(ctx, buf, sizeof(buf));
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH64_update(ctx, buf, sizeof(buf));
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH64_update(ctx, &c, sizeof(c));
  }
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH64_update(ctx, &c, sizeof(c));
  }
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(str, i);
    XXH64_update(ctx, &c, sizeof(c));
  }
  XXH64_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH64_update_bytevector(void *ptr_ctx, ptr bv, int start, int stop,
                                    int tag) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  XXH64_update(ctx, Sbytevector_data(bv) + start, stop - start);
  XXH64_update(ctx, &tag, sizeof(tag));
}

ptr hasher_XXH64_finalize(void *ptr_ctx) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  XXH64_hash_t hash = XXH64_digest(ctx);
  XXH64_freeState(ctx);
  return Sunsigned64(hash);
}

void hasher_XXH64_reset(void *ptr_ctx, uint64_t seed) {
  XXH64_state_t *ctx = (XXH64_state_t *)ptr_ctx;
  XXH64_reset(ctx, seed);
}

//// xxh3-64

void *hasher_XXH3_64_create(uint64_t seed) {
  XXH3_state_t *ctx = XXH3_createState();
  XXH3_64bits_reset_withSeed(ctx, seed);
  return (void *)ctx;
}

ptr hasher_XXH3_64_get(void *ptr_ctx) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  return Sunsigned64(XXH3_64bits_digest(ctx));
}

void hasher_XXH3_64_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  XXH3_64bits_update(ctx, &x, sizeof(x));
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_flonum(void *ptr_ctx, double x, int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  char buf[sizeof(x)];
  memcpy(&buf, &x, sizeof(buf));
  XXH3_64bits_update(ctx, buf, sizeof(buf));
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_ratnum(void *ptr_ctx, int64_t x, int64_t y,
                                  int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH3_64bits_update(ctx, buf, sizeof(buf));
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  char buf[sizeof(x) * 2];
  memcpy(buf, &x, sizeof(x));
  memcpy(buf + sizeof(x), &y, sizeof(y));
  XXH3_64bits_update(ctx, buf, sizeof(buf));
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                    int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint64_t c = Sfixnum_value(Sfxvector_ref(x, i));
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                    int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    double c = Sflvector_ref(x, i);
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                  int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  for (int i = start; i < stop; i++) {
    uint32_t c = Sstring_ref(str, i);
    XXH3_64bits_update(ctx, &c, sizeof(c));
  }
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

void hasher_XXH3_64_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                      int stop, int tag) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  XXH3_64bits_update(ctx, Sbytevector_data(bv) + start, stop - start);
  XXH3_64bits_update(ctx, &tag, sizeof(tag));
}

ptr hasher_XXH3_64_finalize(void *ptr_ctx) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  XXH64_hash_t hash = XXH3_64bits_digest(ctx);
  XXH3_freeState(ctx);
  return Sunsigned64(hash);
}

void hasher_XXH3_64_reset(void *ptr_ctx, uint64_t seed) {
  XXH3_state_t *ctx = (XXH3_state_t *)ptr_ctx;
  XXH3_64bits_reset_withSeed(ctx, seed);
}
