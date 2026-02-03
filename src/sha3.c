/**
 * @file sha3.c
 * @brief SHA3-256/512/1024 and streaming API implementation.
 *
 * Implements the SHA3-256, SHA3-512, and SHA3-1024 hash functions
 * as well as the streaming API by wrapping the core Keccak sponge construction.
 */

#include "sha3.h"
#include <string.h>
#include <stdint.h>

/**
 * @brief Computes a SHA3-256 hash.
 * @param[out] output Buffer to store the 32-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 32);
}

/**
 * @brief Computes a SHA3-512 hash.
 * @param[out] output Buffer to store the 64-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 64);
}

/**
 * @brief Computes a SHA3-1024 hash.
 * @param[out] output Buffer to store the 128-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 128);
}

/**
 * @brief Initializes a streaming SHA3 context.
 * @param[out] ctx The context to initialize
 * @param[in] output_len Desired output length in bytes
 */
void sha3_init(sha3_ctx *ctx, size_t output_len) {
    if (ctx == NULL || output_len == 0) {
        return;
    }

    memset(ctx, 0, sizeof(sha3_ctx));
    ctx->rate = 200 - 2 * output_len;
    ctx->output_len = output_len;
    ctx->block_pos = 0;
    ctx->finalized = 0;
    memset(ctx->state, 0, sizeof(ctx->state));
}

/**
 * @brief Updates a streaming SHA3 context with new input data.
 * @param[in,out] ctx The context to update
 * @param[in] input Input data to add to the hash
 * @param[in] input_len Length of the input data in bytes
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
            for (int j = 0; j < 25; j++)
                ctx->state[j] = load64_le(&ctx->state_bytes[j * 8]);

            keccak_f1600(ctx->state);

            for (int j = 0; j < 25; j++)
                store64_le(&ctx->state_bytes[j * 8], ctx->state[j]);

            ctx->block_pos = 0;
        }
    }
}

/**
 * @brief Finalizes a streaming SHA3 context and produces the hash output.
 * @param[in,out] ctx The context to finalize
 * @param[out] output Buffer to store the hash result
 * @param[in] output_len Length of the output buffer in bytes
 */
void sha3_final(sha3_ctx *ctx, uint8_t *output, size_t output_len) {
    if (ctx == NULL || ctx->finalized || output == NULL || output_len != ctx->output_len) {
        return;
    }

    ctx->finalized = 1;

    ctx->state_bytes[ctx->block_pos] ^= 0x06;
    ctx->state_bytes[ctx->rate - 1] ^= 0x80;

    for (int j = 0; j < 25; j++)
        ctx->state[j] = load64_le(&ctx->state_bytes[j * 8]);

    keccak_f1600(ctx->state);

    size_t i;
    for (i = 0; i < output_len; ) {
        for (int j = 0; j < 25; j++)
            store64_le(&ctx->state_bytes[j * 8], ctx->state[j]);

        size_t remaining = output_len - i;
        size_t copy_size = (remaining < ctx->rate) ? remaining : ctx->rate;

        for (size_t j = 0; j < copy_size; j++)
            output[i + j] = ctx->state_bytes[j];

        i += copy_size;

        if (i < output_len)
            keccak_f1600(ctx->state);
    }
}
