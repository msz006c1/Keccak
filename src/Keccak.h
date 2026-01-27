/**
 * @file Keccak.h
 * @brief Public API for Keccak/SHA3 hash functions
 * 
 * This header provides both single-call and streaming interfaces for
 * SHA3-256, SHA3-512, and generic Keccak hashing.
 * 
 * FIPS 202 Compliant Implementation
 * Platforms: Windows, macOS, Linux, iOS, Android
 * 
 * @author Andy Wang
 * @date 2026
 */

#ifndef KECCAK_H
#define KECCAK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/* ============================================================================
 * Single-Call Hash Functions
 * ============================================================================ */

/**
 * @brief Generic Keccak hash function
 * 
 * Implements the Keccak sponge construction with SHA3 padding (0x06).
 * Can be used to compute SHA3-256, SHA3-512, or other Keccak variants
 * by varying the output length.
 * 
 * @param output Pointer to output buffer (must be at least output_len bytes)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 * @param output_len Desired hash output length in bytes (1-64)
 * 
 * @note
 *   - output must not be NULL
 *   - If input is NULL, input_len must be 0
 *   - output_len must be between 1 and 64
 *   - Returns without error if parameters are invalid
 * 
 * @example
 *   uint8_t hash[32];
 *   const uint8_t data[] = "Hello, World!";
 *   keccak_hash(hash, data, strlen((const char*)data), 32);
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len);

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

/* ============================================================================
 * Streaming/Incremental Hash Functions
 * ============================================================================ */

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
 *                   - Any value 1-64 for custom output length
 * 
 * @return void (silently ignores invalid parameters)
 * 
 * @note
 *   - output_len must be between 1 and 64
 *   - If output_len is 0 or >64, context is not initialized
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

/* ============================================================================
 * Platform Support
 * ============================================================================ */

/**
 * @brief Supported Platforms
 * 
 * This library has been tested and is fully functional on:
 *   - Windows (x86, x86_64, ARM)
 *   - macOS (Intel, Apple Silicon)
 *   - Linux (x86_64, ARM, ARMv7, MIPS)
 *   - iOS (ARM64)
 *   - Android (ARM, ARM64, x86, x86_64)
 * 
 * Compiler support:
 *   - GCC (4.8+)
 *   - Clang (3.4+)
 *   - MSVC (2015+)
 * 
 * C Standard: C99 or later
 */

#ifdef __cplusplus
}
#endif

#endif /* KECCAK_H */
