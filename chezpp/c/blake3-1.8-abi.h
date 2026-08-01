#ifndef CHEZPP_BLAKE3_1_8_ABI_H
#define CHEZPP_BLAKE3_1_8_ABI_H

#include <stddef.h>
#include <stdint.h>

/*
 * Minimal BLAKE3 1.8.x ABI used by Chezpp's runtime loader.
 *
 * Keeping these declarations local lets libchezpp compile without the BLAKE3
 * development headers. optional_hash.c verifies the loaded library's major
 * and minor version before resolving or calling any of these functions.
 * Update this file and that version check together when supporting a new ABI.
 */
#define CHEZPP_BLAKE3_VERSION_MAJOR 1
#define CHEZPP_BLAKE3_VERSION_MINOR 8
#define BLAKE3_OUT_LEN 32
#define BLAKE3_BLOCK_LEN 64
#define BLAKE3_MAX_DEPTH 54

typedef struct {
  uint32_t cv[8];
  uint64_t chunk_counter;
  uint8_t buf[BLAKE3_BLOCK_LEN];
  uint8_t buf_len;
  uint8_t blocks_compressed;
  uint8_t flags;
} blake3_chunk_state;

/* This layout is part of the 1.8.x calling contract despite being private upstream. */
typedef struct {
  uint32_t key[8];
  blake3_chunk_state chunk;
  uint8_t cv_stack_len;
  uint8_t cv_stack[(BLAKE3_MAX_DEPTH + 1) * BLAKE3_OUT_LEN];
} blake3_hasher;

typedef struct {
  const char *(*version)(void);
  void (*hasher_init)(blake3_hasher *);
  void (*hasher_update)(blake3_hasher *, const void *, size_t);
  void (*hasher_finalize)(const blake3_hasher *, uint8_t *, size_t);
  void (*hasher_reset)(blake3_hasher *);
} chezpp_blake3_api;

extern chezpp_blake3_api chezpp_blake3;

#define blake3_hasher_init chezpp_blake3.hasher_init
#define blake3_hasher_update chezpp_blake3.hasher_update
#define blake3_hasher_finalize chezpp_blake3.hasher_finalize
#define blake3_hasher_reset chezpp_blake3.hasher_reset

#endif
