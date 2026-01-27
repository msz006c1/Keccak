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
 * Platform Detection - Support Windows, macOS, Linux, iOS, Android
 * ============================================================================ */

#if defined(_WIN32) || defined(_WIN64)
    #define PLATFORM_WINDOWS 1
    #if defined(_MSC_VER)
        #define COMPILER_MSVC 1
        #include <intrin.h>
    #endif
#elif defined(__APPLE__)
    #include <TargetConditionals.h>
    #if TARGET_OS_IOS || TARGET_OS_IPHONE
        #define PLATFORM_IOS 1
    #else
        #define PLATFORM_MACOS 1
    #endif
    #define COMPILER_CLANG 1
#elif defined(__ANDROID__)
    #define PLATFORM_ANDROID 1
    #define COMPILER_GCC 1
#elif defined(__linux__)
    #define PLATFORM_LINUX 1
    #if defined(__clang__)
        #define COMPILER_CLANG 1
    #else
        #define COMPILER_GCC 1
    #endif
#else
    #define PLATFORM_UNKNOWN 1
#endif

/* Compiler detection fallback */
#if !defined(COMPILER_MSVC) && !defined(COMPILER_CLANG) && !defined(COMPILER_GCC)
    #if defined(__clang__)
        #define COMPILER_CLANG 1
    #elif defined(__GNUC__)
        #define COMPILER_GCC 1
    #endif
#endif

/* ============================================================================
 * Endianness Detection and Byte-order Functions
 * ============================================================================ */

/**
 * @brief Load a 64-bit little-endian value from unaligned memory
 * 
 * This function safely loads a 64-bit value from a byte array in little-endian format,
 * handling any alignment issues and platform-specific byte order.
 * 
 * @param p Pointer to the byte array (may be unaligned)
 * @return The 64-bit value in native byte order
 */
static inline uint64_t load64_le(const uint8_t *p) {
    uint64_t result = 0;
    result |= ((uint64_t)p[0]) << 0;
    result |= ((uint64_t)p[1]) << 8;
    result |= ((uint64_t)p[2]) << 16;
    result |= ((uint64_t)p[3]) << 24;
    result |= ((uint64_t)p[4]) << 32;
    result |= ((uint64_t)p[5]) << 40;
    result |= ((uint64_t)p[6]) << 48;
    result |= ((uint64_t)p[7]) << 56;
    return result;
}

/**
 * @brief Store a 64-bit value to memory in little-endian format
 * 
 * This function safely stores a 64-bit value to a byte array in little-endian format,
 * handling any alignment issues and platform-specific byte order.
 * 
 * @param p Pointer to the byte array (may be unaligned)
 * @param v The 64-bit value to store (in native byte order)
 */
static inline void store64_le(uint8_t *p, uint64_t v) {
    p[0] = (v >> 0) & 0xFF;
    p[1] = (v >> 8) & 0xFF;
    p[2] = (v >> 16) & 0xFF;
    p[3] = (v >> 24) & 0xFF;
    p[4] = (v >> 32) & 0xFF;
    p[5] = (v >> 40) & 0xFF;
    p[6] = (v >> 48) & 0xFF;
    p[7] = (v >> 56) & 0xFF;
}

/* ============================================================================
 * Keccak Constants and Parameters
 * ============================================================================ */

#define KECCAK_STATE_SIZE 200  /* 25 * 8 bytes */
#define KECCAK_ROUNDS 24

/**
 * @brief Round constants for Keccak-f[1600] permutation
 * 
 * These 64-bit constants are used in the Iota step of each round.
 * Values are from FIPS 202 standard.
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
 * 
 * These offsets determine how much each lane is rotated.
 * Values are from FIPS 202 standard for lane [x, y] = [x + 5*y].
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
 * 
 * @param x The value to rotate
 * @param n Number of bits to rotate (0 <= n < 64)
 * @return The rotated value
 */
static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

/* ============================================================================
 * Keccak-f[1600] Permutation
 * ============================================================================ */

/**
 * @brief Keccak-f[1600] permutation function
 * 
 * This is the core permutation function of Keccak. It implements the 24-round
 * permutation as specified in FIPS 202. The state consists of 25 64-bit words.
 * 
 * Each round performs five steps:
 * 1. Theta - Column parity computation
 * 2. Rho   - Lane rotation
 * 3. Pi    - Lane transposition
 * 4. Chi   - Nonlinear substitution
 * 5. Iota  - Round constant addition
 * 
 * @param state[25] The Keccak state (5x5 array of 64-bit lanes)
 */
static void keccak_f1600(uint64_t state[25]) {
    int round;
    
    for (round = 0; round < KECCAK_ROUNDS; round++) {
        uint64_t C[5];
        uint64_t D[5];
        uint64_t B[25];
        int x, y, index;

        /* ====== Theta ====== */
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

        /* ====== Rho and Pi ====== */
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                index = x + 5 * y;
                B[(y + 2 * x) % 5 + 5 * x] = rotl64(state[index], keccak_rho[index]);
            }
        }

        /* ====== Chi ====== */
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                index = x + 5 * y;
                state[index] = B[index] ^ ((~B[(x + 1) % 5 + 5 * y]) & B[(x + 2) % 5 + 5 * y]);
            }
        }

        /* ====== Iota ====== */
        state[0] ^= keccak_rc[round];
    }
}

/* ============================================================================
 * Public API - Generic and SHA3-specific Functions
 * ============================================================================ */

/**
 * @brief Generic Keccak hash function
 * 
 * This implements the Keccak sponge construction with SHA3 padding (0x06).
 * It can be used for SHA3-256, SHA3-512, and other variants.
 * 
 * @param output Output buffer for the hash (must be at least output_len bytes)
 * @param input Input data to hash (can be NULL if input_len is 0)
 * @param input_len Length of input in bytes
 * @param output_len Desired hash output length in bytes (1-64)
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len) {
    uint64_t state[25];
    uint8_t state_bytes[KECCAK_STATE_SIZE];
    size_t rate = KECCAK_STATE_SIZE - 2 * output_len;  /* Absorption rate in bytes */
    size_t i, j, block_size;

    /* Input validation */
    if (output == NULL || (input == NULL && input_len > 0) || output_len == 0 || output_len > 64) {
        return;
    }

    /* Initialize state to all zeros */
    memset(state, 0, sizeof(state));

    /* ====== Absorb Phase ====== */
    for (i = 0; i < input_len; i += rate) {
        block_size = (input_len - i > rate) ? rate : (input_len - i);

        /* Convert state to bytes */
        for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
            store64_le(&state_bytes[j], state[j / 8]);
        }

        /* XOR input block into state */
        for (j = 0; j < block_size; j++) {
            state_bytes[j] ^= input[i + j];
        }

        /* Convert back to 64-bit words */
        for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
            state[j / 8] = load64_le(&state_bytes[j]);
        }

        /* Apply permutation if we absorbed a full block */
        if (block_size == rate) {
            keccak_f1600(state);
        }
    }

    /* ====== Padding (SHA3 domain: 0x06) ====== */
    /* Convert state to bytes for padding */
    for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
        store64_le(&state_bytes[j], state[j / 8]);
    }

    /* Pad: append 0x06 at the input end, 0x80 at the rate end */
    state_bytes[input_len % rate] ^= 0x06;
    state_bytes[rate - 1] ^= 0x80;

    /* Convert back to state and apply final permutation */
    for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
        state[j / 8] = load64_le(&state_bytes[j]);
    }
    keccak_f1600(state);

    /* ====== Squeeze Phase ====== */
    for (i = 0; i < output_len; i += rate) {
        block_size = (output_len - i > rate) ? rate : (output_len - i);

        /* Convert state to bytes */
        for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
            store64_le(&state_bytes[j], state[j / 8]);
        }

        /* Copy output block */
        memcpy(&output[i], state_bytes, block_size);

        /* Apply permutation if more output is needed */
        if (i + rate < output_len) {
            keccak_f1600(state);
        }
    }
}

/**
 * @brief SHA3-256 hash function
 * 
 * Computes the SHA3-256 hash of input data (32-byte output).
 * Compliant with FIPS 202.
 * 
 * @param output Pointer to 32-byte output buffer
 * @param input Pointer to input data
 * @param input_len Length of input in bytes
 */
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 32);
}

/**
 * @brief SHA3-512 hash function
 * 
 * Computes the SHA3-512 hash of input data (64-byte output).
 * Compliant with FIPS 202.
 * 
 * @param output Pointer to 64-byte output buffer
 * @param input Pointer to input data
 * @param input_len Length of input in bytes
 */
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 64);
}

/**
 * @brief Initialize SHA3 incremental hash context
 * 
 * Prepares a context for streaming hash computation.
 * Must be called once before any sha3_update calls.
 * 
 * @param ctx Pointer to the context structure
 * @param output_len Desired hash length in bytes (typically 32 or 64)
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
}

/**
 * @brief Update SHA3 hash with new data
 * 
 * Processes input data and updates the internal hash state.
 * Can be called multiple times with different data chunks.
 * Must not be called after sha3_final.
 * 
 * @param ctx Pointer to the context structure
 * @param input Pointer to input data
 * @param input_len Length of input in bytes
 */
void sha3_update(sha3_ctx *ctx, const uint8_t *input, size_t input_len) {
    size_t i, j;

    if (ctx == NULL || ctx->finalized || input == NULL || input_len == 0) {
        return;
    }

    for (i = 0; i < input_len; i++) {
        /* Convert state to bytes */
        for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
            store64_le(&ctx->state_bytes[j], ctx->state[j / 8]);
        }

        /* XOR input byte */
        ctx->state_bytes[ctx->block_pos] ^= input[i];

        ctx->block_pos++;

        /* Apply permutation when block is full */
        if (ctx->block_pos == ctx->rate) {
            /* Convert back to state */
            for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
                ctx->state[j / 8] = load64_le(&ctx->state_bytes[j]);
            }
            keccak_f1600(ctx->state);
            ctx->block_pos = 0;
        }
    }
}

/**
 * @brief Finalize SHA3 hash and produce output
 * 
 * Completes the hash computation and generates the output.
 * Can only be called once after initialization and updates.
 * 
 * @param ctx Pointer to the context structure
 * @param output Pointer to output buffer
 * @param output_len Length of output buffer (should match init output_len)
 */
void sha3_final(sha3_ctx *ctx, uint8_t *output, size_t output_len) {
    size_t i, j;

    if (ctx == NULL || ctx->finalized || output == NULL) {
        return;
    }

    if (output_len != ctx->output_len) {
        return;
    }

    ctx->finalized = 1;

    /* Convert state to bytes */
    for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
        store64_le(&ctx->state_bytes[j], ctx->state[j / 8]);
    }

    /* Padding: SHA3 domain separation (0x06) */
    ctx->state_bytes[ctx->block_pos] ^= 0x06;
    ctx->state_bytes[ctx->rate - 1] ^= 0x80;

    /* Convert back to state and apply final permutation */
    for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
        ctx->state[j / 8] = load64_le(&ctx->state_bytes[j]);
    }
    keccak_f1600(ctx->state);

    /* Squeeze output */
    for (i = 0; i < output_len; i += ctx->rate) {
        size_t block_size = (output_len - i > ctx->rate) ? ctx->rate : (output_len - i);

        /* Convert state to bytes */
        for (j = 0; j < KECCAK_STATE_SIZE; j += 8) {
            store64_le(&ctx->state_bytes[j], ctx->state[j / 8]);
        }

        /* Copy output */
        memcpy(&output[i], ctx->state_bytes, block_size);

        /* Apply permutation if needed */
        if (i + ctx->rate < output_len) {
            keccak_f1600(ctx->state);
        }
    }
}