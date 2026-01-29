/**
 * @file sha3.c
 * @brief Implementation of SHA3 hash functions (FIPS 202 compliant)
 *
 * This file implements the SHA3 hash functions as defined in FIPS 202,
 * including SHA3-256, SHA3-512, SHA3-1024, and streaming APIs.
 * It depends on the core Keccak implementation for the permutation function.
 *
 * @author Andy Wang
 * @date 2026
 * @version 1.0
 * @license Public Domain
 *
 * @see FIPS 202: SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions
 */

#include "sha3.h"
#include <string.h>
#include <stdint.h>

/**
 * @defgroup SHA3_Impl SHA3 Implementation
 * @brief Internal implementation details for SHA3 functions
 * @{
 */

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
 * @brief SHA3-1024 hash function (Extended Keccak variant)
 *
 * Computes the SHA3-1024 hash of input data using the Keccak sponge
 * construction with increased capacity to safely support 128-byte output.
 *
 * Output length: 128 bytes (1024 bits)
 * Capacity: 1024 bits
 * Rate: 576 bits (72 bytes)
 */
void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 128);
}

/* ============================================================================
 * Streaming/Incremental Hash Functions
 * ============================================================================ */

void sha3_init(sha3_ctx *ctx, size_t output_len) {
    if (ctx == NULL || output_len == 0) {
        return;
    }

    memset(ctx, 0, sizeof(sha3_ctx));
    ctx->rate = 200 - 2 * output_len;  // Rate in bytes (KECCAK_STATE_SIZE - 2*output_len)
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

/** @} */ /* End of SHA3_Impl group */