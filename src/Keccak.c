/**
 * @file Keccak.c
 * @brief Implementation of the Keccak hash function family (SHA3)
 *
 * This file implements the Keccak hash function family, including SHA3-256
 * and SHA3-512. It provides both single-call and incremental hashing APIs.
 * Fully compliant with FIPS 202 standard.
 *
 * @author Andy Wang
 * @date 2026
 */

#include "Keccak.h"
#include <string.h>
#include <stdint.h>

/* ============================================================================
 * Keccak Constants and Parameters
 * ============================================================================ */

#define KECCAK_STATE_SIZE 200  /* 25 * 8 bytes */
#define KECCAK_ROUNDS 24

/**
 * @brief Round constants for Keccak-f[1600] permutation
 */
static const uint64_t keccak_rc[KECCAK_ROUNDS] = {
    0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808aULL,
    0x8000000080008000ULL, 0x000000000000808bULL, 0x0000000080000001ULL,
    0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008aULL,
    0x0000000000000088ULL, 0x0000000080008009ULL, 0x0000000000008003ULL,
    0x0000000000008002ULL, 0x0000000000000080ULL, 0x000000000000800aULL,
    0x000000008000000aULL, 0x000000008000808bULL, 0x800000000000008bULL,
    0x8000000000008089ULL, 0x8000000000008003ULL, 0x8000000000008002ULL,
    0x8000000000000080ULL, 0x000000000000800aULL, 0x800000008000000aULL
};

/**
 * @brief Rotation offsets for Keccak-f[1600] Rho step
 */
static const int keccak_rho[25] = {
     0,  1, 62, 28, 27,
    36, 44,  6, 55, 20,
     3, 10, 43, 25, 39,
    41, 45, 15, 21,  8,
    18,  2, 61, 56, 14
};

/**
 * @brief Rotate a 64-bit value left by n bits
 */
static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

/**
 * @brief Load a 64-bit little-endian value from unaligned memory
 */
static inline uint64_t load64_le(const uint8_t *p) {
    return ((uint64_t)p[0]) |
           (((uint64_t)p[1]) << 8) |
           (((uint64_t)p[2]) << 16) |
           (((uint64_t)p[3]) << 24) |
           (((uint64_t)p[4]) << 32) |
           (((uint64_t)p[5]) << 40) |
           (((uint64_t)p[6]) << 48) |
           (((uint64_t)p[7]) << 56);
}

/**
 * @brief Store a 64-bit value to memory in little-endian format
 */
static inline void store64_le(uint8_t *p, uint64_t v) {
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
    p[2] = (v >> 16) & 0xFF;
    p[3] = (v >> 24) & 0xFF;
    p[4] = (v >> 32) & 0xFF;
    p[5] = (v >> 40) & 0xFF;
    p[6] = (v >> 48) & 0xFF;
    p[7] = (v >> 56) & 0xFF;
}

/* ============================================================================
 * Keccak-f[1600] Permutation
 * ============================================================================ */

/**
 * @brief Keccak-f[1600] permutation function
 */
static void keccak_f1600(uint64_t state[25]) {
    int round;

    for (round = 0; round < KECCAK_ROUNDS; round++) {
        uint64_t C[5], D[5], B[25];
        int x, y;

        /* Theta step */
        for (x = 0; x < 5; x++) {
            C[x] = state[x] ^ state[x + 5] ^ state[x + 10] ^ state[x + 15] ^ state[x + 20];
        }
        for (x = 0; x < 5; x++) {
            D[x] = C[(x + 4) % 5] ^ rotl64(C[(x + 1) % 5], 1);
        }
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                state[x + 5 * y] ^= D[x];
            }
        }

        /* Rho and Pi steps combined */
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                int index = x + 5 * y;
                B[(y + 2 * x) % 5 + 5 * x] = rotl64(state[index], keccak_rho[index]);
            }
        }

        /* Chi step */
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                int index = x + 5 * y;
                state[index] = B[index] ^ ((~B[(x + 1) % 5 + 5 * y]) & B[(x + 2) % 5 + 5 * y]);
            }
        }

        /* Iota step */
        state[0] ^= keccak_rc[round];
    }
}

/* ============================================================================
 * Public API Functions
 * ============================================================================ */

/**
 * @brief Generic Keccak hash function
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len) {
    uint64_t state[25] = {0};
    uint8_t state_bytes[KECCAK_STATE_SIZE];
    size_t rate = KECCAK_STATE_SIZE - 2 * output_len;  // Rate in bytes
    size_t i, j;

    /* Input validation */
    if (output == NULL || (input == NULL && input_len > 0) || output_len == 0 || output_len > 64) {
        return;
    }

    /* Absorb input */
    for (i = 0; i < input_len; ) {
        size_t remaining = input_len - i;
        size_t block_size = (remaining < rate) ? remaining : rate;

        // Convert state to bytes
        for (j = 0; j < 25; j++) {
            store64_le(&state_bytes[j * 8], state[j]);
        }

        // XOR input into state
        for (j = 0; j < block_size; j++) {
            state_bytes[j] ^= input[i + j];
        }

        // Convert back to state
        for (j = 0; j < 25; j++) {
            state[j] = load64_le(&state_bytes[j * 8]);
        }

        i += block_size;

        // Apply permutation if we filled a complete block
        if (block_size == rate) {
            keccak_f1600(state);
        }
    }

    /* Padding */
    // If the input completely fills one or more blocks, we need to apply the permutation
    // to those blocks first, then apply padding to a new block
    if (input_len > 0 && input_len % rate == 0) {
        // Apply permutation for the completely filled block
        keccak_f1600(state);
        // Now pad a new (zero-initialized) block
        memset(state, 0, sizeof(state));
    }

    // Convert state to bytes for padding
    for (j = 0; j < 25; j++) {
        store64_le(&state_bytes[j * 8], state[j]);
    }

    // Apply SHA3 padding: 0x06 at first available position after message, 0x80 at end of block
    size_t pos = input_len % rate;
    state_bytes[pos] ^= 0x06;
    state_bytes[rate - 1] ^= 0x80;

    // Convert back to state and apply final permutation
    for (j = 0; j < 25; j++) {
        state[j] = load64_le(&state_bytes[j * 8]);
    }
    keccak_f1600(state);

    /* Squeeze output */
    for (i = 0; i < output_len; ) {
        // Convert state to bytes
        for (j = 0; j < 25; j++) {
            store64_le(&state_bytes[j * 8], state[j]);
        }

        size_t remaining = output_len - i;
        size_t copy_size = (remaining < rate) ? remaining : rate;

        // Copy output
        for (j = 0; j < copy_size; j++) {
            output[i + j] = state_bytes[j];
        }

        i += copy_size;

        // Apply permutation if we need more output
        if (i < output_len) {
            keccak_f1600(state);
        }
    }
}

/**
 * @brief SHA3-256 hash function
 */
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 32);
}

/**
 * @brief SHA3-512 hash function
 */
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 64);
}

/**
 * @brief Initialize SHA3 incremental hash context
 */
void sha3_init(sha3_ctx *ctx, size_t output_len) {
    if (ctx == NULL || output_len == 0 || output_len > 64) {
        return;
    }

    memset(ctx, 0, sizeof(sha3_ctx));
    ctx->rate = KECCAK_STATE_SIZE - 2 * output_len;
    ctx->output_len = output_len;
    ctx->block_pos = 0;
    ctx->finalized = 0;
    
    // Initialize state to zero
    memset(ctx->state, 0, sizeof(ctx->state));
}

/**
 * @brief Update SHA3 hash with new data
 */
void sha3_update(sha3_ctx *ctx, const uint8_t *input, size_t input_len) {
    if (ctx == NULL || ctx->finalized || input == NULL || input_len == 0) {
        return;
    }

    size_t i;
    for (i = 0; i < input_len; i++) {
        ctx->state_bytes[ctx->block_pos] ^= input[i];
        ctx->block_pos++;

        if (ctx->block_pos == ctx->rate) {
            // Convert bytes to state
            for (int j = 0; j < 25; j++) {
                ctx->state[j] = load64_le(&ctx->state_bytes[j * 8]);
            }

            keccak_f1600(ctx->state);

            // Convert back to bytes
            for (int j = 0; j < 25; j++) {
                store64_le(&ctx->state_bytes[j * 8], ctx->state[j]);
            }

            ctx->block_pos = 0;
        }
    }
}

/**
 * @brief Finalize SHA3 hash and produce output
 */
void sha3_final(sha3_ctx *ctx, uint8_t *output, size_t output_len) {
    if (ctx == NULL || ctx->finalized || output == NULL || output_len != ctx->output_len) {
        return;
    }

    ctx->finalized = 1;

    // Apply padding
    ctx->state_bytes[ctx->block_pos] ^= 0x06;
    ctx->state_bytes[ctx->rate - 1] ^= 0x80;

    // Convert to state and apply final permutation
    for (int j = 0; j < 25; j++) {
        ctx->state[j] = load64_le(&ctx->state_bytes[j * 8]);
    }

    keccak_f1600(ctx->state);

    // Squeeze output
    size_t i;
    for (i = 0; i < output_len; ) {
        // Convert state to bytes
        for (int j = 0; j < 25; j++) {
            store64_le(&ctx->state_bytes[j * 8], ctx->state[j]);
        }

        size_t remaining = output_len - i;
        size_t copy_size = (remaining < ctx->rate) ? remaining : ctx->rate;

        // Copy output
        for (size_t j = 0; j < copy_size; j++) {
            output[i + j] = ctx->state_bytes[j];
        }

        i += copy_size;

        // Apply permutation if we need more output
        if (i < output_len) {
            keccak_f1600(ctx->state);
        }
    }
}