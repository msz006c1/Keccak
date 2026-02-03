/**
 * @file sha3.h
 * @brief SHA3-256/512/1024 single-call and streaming API.
 *
 * Defines the public interface for SHA3-256, SHA3-512, and SHA3-1024 hash functions
 * as well as the streaming API. FIPS 202 compliant.
 */

#ifndef SHA3_H
#define SHA3_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include "Keccak.h"

/**
 * @brief Computes a SHA3-256 hash.
 * @param[out] output Buffer to store the 32-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len);

/**
 * @brief Computes a SHA3-512 hash.
 * @param[out] output Buffer to store the 64-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len);

/**
 * @brief Computes a SHA3-1024 hash.
 * @param[out] output Buffer to store the 128-byte hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len);

/**
 * @struct sha3_ctx
 * @brief Streaming hash context for SHA3 operations.
 */
typedef struct {
    uint64_t state[25];          /**< Internal state of the hash function */
    uint8_t state_bytes[200];    /**< State represented as bytes for easier manipulation */
    size_t rate;                 /**< Rate parameter for the sponge construction */
    size_t block_pos;            /**< Current position in the current block */
    size_t output_len;           /**< Expected output length */
    int finalized;               /**< Flag indicating if the context has been finalized */
} sha3_ctx;

/**
 * @brief Initializes a streaming SHA3 context.
 * @param[out] ctx The context to initialize
 * @param[in] output_len Desired output length in bytes
 */
void sha3_init(sha3_ctx *ctx, size_t output_len);

/**
 * @brief Updates a streaming SHA3 context with new input data.
 * @param[in,out] ctx The context to update
 * @param[in] input Input data to add to the hash
 * @param[in] input_len Length of the input data in bytes
 */
void sha3_update(sha3_ctx *ctx, const uint8_t *input, size_t input_len);

/**
 * @brief Finalizes a streaming SHA3 context and produces the hash output.
 * @param[in,out] ctx The context to finalize
 * @param[out] output Buffer to store the hash result
 * @param[in] output_len Length of the output buffer in bytes
 */
void sha3_final(sha3_ctx *ctx, uint8_t *output, size_t output_len);

#ifdef __cplusplus
}
#endif

#endif /* SHA3_H */
