#include "net/unavailable.h"

uint32_t hash_XXH32(ptr bv, uint32_t seed) {
  (void)bv;
  (void)seed;
  return 0;
}

ptr hash_XXH64(ptr bv, uint64_t seed) {
  (void)bv;
  (void)seed;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64(ptr bv, uint64_t seed) {
  (void)bv;
  (void)seed;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

uint32_t hash_XXH32_fixnum(int64_t x, uint32_t salt) {
  (void)x;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_flonum(double x, uint32_t salt) {
  (void)x;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_ratnum(int64_t x, int64_t y, uint32_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_cflonum(double x, double y, uint32_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_string(ptr x, int start, int stop, uint32_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_fxvector(ptr x, int start, int stop, uint32_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_flvector(ptr x, int start, int stop, uint32_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return 0;
}

uint32_t hash_XXH32_bytevector(ptr x, int start, int stop, uint32_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return 0;
}

ptr hash_XXH64_fixnum(int64_t x, uint64_t salt) {
  (void)x;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_flonum(double x, uint64_t salt) {
  (void)x;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_ratnum(int64_t x, int64_t y, uint64_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_cflonum(double x, double y, uint64_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_string(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_fxvector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_flvector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH64_bytevector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_fixnum(int64_t x, uint64_t salt) {
  (void)x;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_flonum(double x, uint64_t salt) {
  (void)x;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_ratnum(int64_t x, int64_t y, uint64_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_cflonum(double x, double y, uint64_t salt) {
  (void)x;
  (void)y;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_string(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_fxvector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_flvector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hash_XXH3_64_bytevector(ptr x, int start, int stop, uint64_t salt) {
  (void)x;
  (void)start;
  (void)stop;
  (void)salt;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

void * hasher_XXH32_create(uint32_t seed) {
  (void)seed;
  return 0;
}

uint32_t hasher_XXH32_get(void *ptr_ctx) {
  (void)ptr_ctx;
  return 0;
}

uint32_t hasher_XXH32_finalize(void *ptr_ctx) {
  (void)ptr_ctx;
  return 0;
}

void hasher_XXH32_reset(void *ptr_ctx, uint32_t seed) {
  (void)ptr_ctx;
  (void)seed;
}

void hasher_XXH32_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH32_update_flonum(void *ptr_ctx, double x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH32_update_ratnum(void *ptr_ctx, int64_t x, int64_t y, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH32_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH32_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                int tag) {
  (void)ptr_ctx;
  (void)str;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH32_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH32_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH32_update_bytevector(void *ptr_ctx, ptr bv, int start, int stop,
                                    int tag) {
  (void)ptr_ctx;
  (void)bv;
  (void)start;
  (void)stop;
  (void)tag;
}

void * hasher_XXH64_create(uint64_t seed) {
  (void)seed;
  return 0;
}

ptr hasher_XXH64_get(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hasher_XXH64_finalize(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

void hasher_XXH64_reset(void *ptr_ctx, uint64_t seed) {
  (void)ptr_ctx;
  (void)seed;
}

void hasher_XXH64_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH64_update_flonum(void *ptr_ctx, double x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH64_update_ratnum(void *ptr_ctx, int64_t x, int64_t y, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH64_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH64_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                int tag) {
  (void)ptr_ctx;
  (void)str;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH64_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH64_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                  int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH64_update_bytevector(void *ptr_ctx, ptr bv, int start, int stop,
                                    int tag) {
  (void)ptr_ctx;
  (void)bv;
  (void)start;
  (void)stop;
  (void)tag;
}

void * hasher_XXH3_64_create(uint64_t seed) {
  (void)seed;
  return 0;
}

ptr hasher_XXH3_64_get(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

ptr hasher_XXH3_64_finalize(void *ptr_ctx) {
  (void)ptr_ctx;
  return chezpp_unavailable_status("xxhash: disabled at build time");
}

void hasher_XXH3_64_reset(void *ptr_ctx, uint64_t seed) {
  (void)ptr_ctx;
  (void)seed;
}

void hasher_XXH3_64_update_fixnum(void *ptr_ctx, int64_t x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH3_64_update_flonum(void *ptr_ctx, double x, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)tag;
}

void hasher_XXH3_64_update_ratnum(void *ptr_ctx, int64_t x, int64_t y,
                                  int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH3_64_update_cflonum(void *ptr_ctx, double x, double y, int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)y;
  (void)tag;
}

void hasher_XXH3_64_update_string(void *ptr_ctx, ptr str, int start, int stop,
                                  int tag) {
  (void)ptr_ctx;
  (void)str;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH3_64_update_fxvector(void *ptr_ctx, ptr x, int start, int stop,
                                    int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH3_64_update_flvector(void *ptr_ctx, ptr x, int start, int stop,
                                    int tag) {
  (void)ptr_ctx;
  (void)x;
  (void)start;
  (void)stop;
  (void)tag;
}

void hasher_XXH3_64_update_bytevector(void *ptr_ctx, ptr bv, int start,
                                      int stop, int tag) {
  (void)ptr_ctx;
  (void)bv;
  (void)start;
  (void)stop;
  (void)tag;
}
