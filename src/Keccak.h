/**
 * @file Keccak.h
 * @brief Public API for core Keccak hash functions.
 *
 * Defines the public interface for the core Keccak hash functions.
 * FIPS 202 compliant. Public domain.
 */

#ifndef KECCAK_H
#define KECCAK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/**
 * @brief Computes a Keccak hash with SHA3 padding.
 * @param[out] output Buffer to store the hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 * @param[in] output_len Desired length of the output hash in bytes
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len);

/**
 * @brief Performs the Keccak-f[1600] permutation (24 rounds).
 * @param[in,out] state The 1600-bit state to permute (25 uint64_t elements)
 */
void keccak_f1600(uint64_t state[25]);

/**
 * @brief Loads an 8-byte value in little-endian format.
 * @param[in] p Pointer to the 8-byte buffer to read from
 * @return The loaded 64-bit value
 */
uint64_t load64_le(const uint8_t *p);

/**
 * @brief Stores a 64-bit value in little-endian format.
 * @param[out] p Pointer to the buffer to store to
 * @param[in] v The 64-bit value to store
 */
void store64_le(uint8_t *p, uint64_t v);

#ifdef __cplusplus
}
#endif

#endif /* KECCAK_H */
