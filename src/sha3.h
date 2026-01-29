/**
 * @file sha3.h
 * @brief Public API for SHA3 hash functions (FIPS 202 compliant)
 *
 * This header provides the interface for SHA3 hash functions as defined
 * in FIPS 202 standard. It includes both single-call and streaming APIs
 * for SHA3-256, SHA3-512, and SHA3-1024.
 *
 * @author Andy Wang
 * @date 2026
 * @version 1.0
 * @license Public Domain
 *
 * @see FIPS 202: SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions
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
 * @defgroup SHA3_SingleCall SHA3 Single-Call Functions
 * @brief One-shot SHA3 hash computation functions
 *
 * These functions compute the SHA3 hash in a single call. They are most efficient
 * when the entire input is available at once.
 *
 * @{
 */

/**
 * @brief SHA3-256 hash function
 *
 * Computes the SHA3-256 hash of input data.
 * Output is always 32 bytes (256 bits).
 * Fully compliant with FIPS 202 standard.
 *
 * @param output Pointer to 32-byte output buffer (must not be NULL)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 *
 * @example
 *   uint8_t hash[32];
 *   sha3_256(hash, "Hello, World!", 13);
 *   // hash now contains the SHA3-256 hash
 */
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len);

/**
 * @brief SHA3-512 hash function
 *
 * Computes the SHA3-512 hash of input data.
 * Output is always 64 bytes (512 bits).
 * Fully compliant with FIPS 202 standard.
 *
 * @param output Pointer to 64-byte output buffer (must not be NULL)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 *
 * @example
 *   uint8_t hash[64];
 *   sha3_512(hash, "Hello, World!", 13);
 *   // hash now contains the SHA3-512 hash
 */
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len);

/**
 * @brief SHA3-1024 hash function (Extended Keccak variant)
 *
 * Computes the SHA3-1024 hash of input data.
 * Output is always 128 bytes (1024 bits).
 * Uses Keccak sponge with increased capacity for larger output.
 *
 * @param output Pointer to 128-byte output buffer (must not be NULL)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 *
 * @example
 *   uint8_t hash[128];
 *   sha3_1024(hash, "Hello, World!", 13);
 *   // hash now contains the SHA3-1024 hash
 */
void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len);

/** @} */ /* End of SHA3_SingleCall group */

/**
 * @defgroup SHA3_Streaming SHA3 Streaming Functions
 * @brief Incremental SHA3 hash computation functions
 *
 * These functions allow computing the SHA3 hash incrementally by:
 * 1. Initializing context with sha3_init()
 * 2. Updating with data chunks via sha3_update()
 * 3. Finalizing with sha3_final()
 *
 * @{
 */

/**
 * @brief SHA3 hash context for incremental hashing
 *
 * This opaque structure maintains the state for streaming hash computation.
 * It allows updating the hash with data in chunks, useful when:
 *   - Processing large files
 *   - Data arrives in packets
 *   - Memory is limited
 *
 * Usage:
 *   1. Call sha3_init() once
 *   2. Call sha3_update() one or more times
 *   3. Call sha3_final() once
 *
 * Do not access fields directly - use the API functions.
 */
typedef struct {
    uint64_t state[25];              /**< Keccak state (25 x 64-bit words) */
    uint8_t state_bytes[200];        /**< State as bytes for I/O operations */
    size_t rate;                     /**< Absorption rate in bytes */
    size_t block_pos;                /**< Current position in absorption block */
    size_t output_len;               /**< Desired output hash length */
    int finalized;                   /**< Flag: hash finalized? (1=yes, 0=no) */
} sha3_ctx;

/**
 * @brief Initialize SHA3 incremental hash context
 *
 * Prepares a context for streaming hash computation.
 * Must be called exactly once before calling sha3_update.
 *
 * @param ctx Pointer to uninitialized sha3_ctx structure (must not be NULL)
 * @param output_len Desired hash output length in bytes:
 *                   - 32 for SHA3-256
 *                   - 64 for SHA3-512
 *                   - 128 for SHA3-1024
 *                   - Any value 1+ for custom output length
 *
 * @return void (silently ignores invalid parameters)
 *
 * @note
 *   - output_len must be at least 1 byte
 *   - No upper limit on output_len (limited by available memory)
 *   - If output_len is 0, context is not initialized
 *
 * @example
 *   sha3_ctx ctx;
 *   sha3_init(&ctx, 32);  // Initialize for SHA3-256
 */
void sha3_init(sha3_ctx *ctx, size_t output_len);

/**
 * @brief Update SHA3 hash with new data
 *
 * Processes input data and updates the internal hash state.
 * Can be called multiple times with different data chunks.
 * Must not be called before sha3_init or after sha3_final.
 *
 * @param ctx Pointer to initialized sha3_ctx structure (must not be NULL)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 *
 * @return void (silently ignores invalid parameters)
 *
 * @note
 *   - Safe to call with input_len=0
 *   - Safe to call multiple times with different data
 *   - Do not modify ctx between calls
 *
 * @example
 *   sha3_ctx ctx;
 *   sha3_init(&ctx, 32);
 *   sha3_update(&ctx, "Hello, ", 7);
 *   sha3_update(&ctx, "World!", 6);
 *   // Hash is updated with "Hello, World!"
 */
void sha3_update(sha3_ctx *ctx, const uint8_t *input, size_t input_len);

/**
 * @brief Finalize SHA3 hash and produce output
 *
 * Completes the hash computation and generates the final output.
 * Can only be called exactly once after initialization and updates.
 * Subsequent calls are ignored.
 *
 * @param ctx Pointer to initialized sha3_ctx structure (must not be NULL)
 * @param output Pointer to output buffer (must not be NULL)
 * @param output_len Length of output buffer in bytes
 *                   Must match the length specified in sha3_init
 *
 * @return void (silently ignores invalid parameters or mismatched sizes)
 *
 * @note
 *   - output_len must exactly match the value from sha3_init
 *   - output buffer must be at least output_len bytes
 *   - After calling, ctx cannot be reused (use sha3_init to restart)
 *
 * @example
 *   sha3_ctx ctx;
 *   uint8_t hash[32];
 *   sha3_init(&ctx, 32);
 *   sha3_update(&ctx, "Hello, World!", 13);
 *   sha3_final(&ctx, hash, 32);
 *   // hash now contains the SHA3-256 hash
 */
void sha3_final(sha3_ctx *ctx, uint8_t *output, size_t output_len);

/** @} */ /* End of SHA3_Streaming group */

#ifdef __cplusplus
}
#endif

#endif /* SHA3_H */