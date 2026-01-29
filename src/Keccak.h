/**
 * @file Keccak.h
 * @brief Public API for Keccak hash functions (core implementation)
 *
 * @mainpage Keccak Core Implementation
 *
 * A FIPS 202 compliant implementation of the core Keccak hash function,
 * providing the fundamental Keccak-f[1600] permutation and generic sponge
 * construction. This serves as the foundation for SHA3 and other derived
 * hash functions.
 *
 * ## Features
 * - **FIPS 202 Compliant**: Fully compliant with the FIPS 202 standard
 * - **Core Functionality**: Keccak-f[1600] permutation and generic sponge
 * - **Cross-Platform**: Supports Windows, macOS, Linux, iOS, Android
 * - **Portable C99**: Written in standard C99 with no external dependencies
 *
 * @see FIPS 202: SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions
 *
 * @author Andy Wang
 * @date 2026
 * @version 1.0
 * @license Public Domain
 */

/**
 * @defgroup KeccakCore Keccak Core Functions
 * @brief Fundamental Keccak implementation functions
 *
 * These functions provide the core Keccak implementation including
 * the permutation function and generic sponge construction.
 *
 * @{
 */

#ifndef KECCAK_H
#define KECCAK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/**
 * @brief Generic Keccak hash function
 *
 * Implements the Keccak sponge construction with SHA3 padding (0x06).
 * Can be used to compute various Keccak variants by varying the output length.
 *
 * @param output Pointer to output buffer (must be at least output_len bytes)
 * @param input Pointer to input data (can be NULL if input_len is 0)
 * @param input_len Length of input data in bytes
 * @param output_len Desired hash output length in bytes (1 or more)
 *
 * @note
 *   - output must not be NULL
 *   - If input is NULL, input_len must be 0
 *   - output_len must be at least 1 byte
 *   - No upper limit on output_len (limited by available memory)
 *   - Returns without error if parameters are invalid
 *
 * @example
 *   uint8_t hash[32];
 *   const uint8_t data[] = "Hello, World!";
 *   keccak_hash(hash, data, strlen((const char*)data), 32);
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len);

/**
 * @brief Keccak-f[1600] permutation function
 *
 * Internal function that performs the Keccak-f[1600] permutation.
 * This is exported for use in other implementations.
 *
 * @param state Pointer to 25-element array of 64-bit words representing the Keccak state
 */
void keccak_f1600(uint64_t state[25]);

/**
 * @brief Load a 64-bit little-endian value from unaligned memory
 *
 * Internal utility function for converting bytes to 64-bit values.
 * This is exported for use in other implementations.
 *
 * @param p Pointer to 8-byte memory region
 * @return 64-bit value loaded in little-endian format
 */
uint64_t load64_le(const uint8_t *p);

/**
 * @brief Store a 64-bit value to memory in little-endian format
 *
 * Internal utility function for converting 64-bit values to bytes.
 * This is exported for use in other implementations.
 *
 * @param p Pointer to 8-byte memory region to write to
 * @param v 64-bit value to store in little-endian format
 */
void store64_le(uint8_t *p, uint64_t v);

/** @} */ /* End of KeccakCore group */

#ifdef __cplusplus
}
#endif

#endif /* KECCAK_H */