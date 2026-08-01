#ifndef CHEZPP_XXHASH_0_8_ABI_H
#define CHEZPP_XXHASH_0_8_ABI_H

#include <stddef.h>
#include <stdint.h>

/*
 * Minimal xxHash 0.8.x ABI used by Chezpp's runtime loader.
 *
 * Keeping these declarations local lets libchezpp compile without the xxHash
 * development headers. optional_hash.c verifies the loaded library's major
 * and minor version before resolving or calling any of these functions.
 * Update this file and that version check together when supporting a new ABI.
 */
#define CHEZPP_XXHASH_VERSION_MAJOR 0
#define CHEZPP_XXHASH_VERSION_MINOR 8

typedef uint32_t XXH32_hash_t;
typedef uint64_t XXH64_hash_t;
typedef struct XXH32_state_s XXH32_state_t;
typedef struct XXH64_state_s XXH64_state_t;
typedef struct XXH3_state_s XXH3_state_t;

typedef struct {
  unsigned (*version_number)(void);
  XXH32_hash_t (*hash32)(const void *, size_t, XXH32_hash_t);
  XXH64_hash_t (*hash64)(const void *, size_t, XXH64_hash_t);
  XXH64_hash_t (*hash3_64_with_seed)(const void *, size_t, XXH64_hash_t);
  XXH32_state_t *(*hash32_create_state)(void);
  int (*hash32_free_state)(XXH32_state_t *);
  int (*hash32_reset)(XXH32_state_t *, XXH32_hash_t);
  int (*hash32_update)(XXH32_state_t *, const void *, size_t);
  XXH32_hash_t (*hash32_digest)(const XXH32_state_t *);
  XXH64_state_t *(*hash64_create_state)(void);
  int (*hash64_free_state)(XXH64_state_t *);
  int (*hash64_reset)(XXH64_state_t *, XXH64_hash_t);
  int (*hash64_update)(XXH64_state_t *, const void *, size_t);
  XXH64_hash_t (*hash64_digest)(const XXH64_state_t *);
  XXH3_state_t *(*hash3_create_state)(void);
  int (*hash3_free_state)(XXH3_state_t *);
  int (*hash3_64_reset_with_seed)(XXH3_state_t *, XXH64_hash_t);
  int (*hash3_64_update)(XXH3_state_t *, const void *, size_t);
  XXH64_hash_t (*hash3_64_digest)(const XXH3_state_t *);
} chezpp_xxhash_api;

extern chezpp_xxhash_api chezpp_xxhash;

#define XXH32 chezpp_xxhash.hash32
#define XXH64 chezpp_xxhash.hash64
#define XXH3_64bits_withSeed chezpp_xxhash.hash3_64_with_seed
#define XXH32_createState chezpp_xxhash.hash32_create_state
#define XXH32_freeState chezpp_xxhash.hash32_free_state
#define XXH32_reset chezpp_xxhash.hash32_reset
#define XXH32_update chezpp_xxhash.hash32_update
#define XXH32_digest chezpp_xxhash.hash32_digest
#define XXH64_createState chezpp_xxhash.hash64_create_state
#define XXH64_freeState chezpp_xxhash.hash64_free_state
#define XXH64_reset chezpp_xxhash.hash64_reset
#define XXH64_update chezpp_xxhash.hash64_update
#define XXH64_digest chezpp_xxhash.hash64_digest
#define XXH3_createState chezpp_xxhash.hash3_create_state
#define XXH3_freeState chezpp_xxhash.hash3_free_state
#define XXH3_64bits_reset_withSeed chezpp_xxhash.hash3_64_reset_with_seed
#define XXH3_64bits_update chezpp_xxhash.hash3_64_update
#define XXH3_64bits_digest chezpp_xxhash.hash3_64_digest

#endif
