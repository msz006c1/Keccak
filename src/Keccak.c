/**
 * @file Keccak.c
 * @brief Implementation of the core Keccak hash function (SHA3 foundation)
 *
 * This file implements the core Keccak hash function family, providing
 * the Keccak-f[1600] permutation and generic sponge construction.
 * This serves as the foundation for SHA3 and other derived hash functions.
 *
 * ## Implementation Details
 *
 * - **Standard**: FIPS 202 SHA-3 Standard foundation
 * - **Algorithm**: Keccak-f[1600] permutation with sponge construction
 * - **Padding**: SHA3 padding (0x06 followed by 0x80)
 * - **Rounds**: 24 permutation rounds
 * - **State Size**: 1600 bits (25 x 64-bit lanes)
 *
 * ## Algorithm Overview
 *
 * The Keccak sponge construction operates in two phases:
 *
 * 1. **Absorption Phase**: Input is XORed into the state in blocks of size
 *    (1600 - 2*output_length) bits, followed by permutation.
 *
 * 2. **Squeezing Phase**: Output blocks are extracted from the state,
 *    applying permutation between blocks as needed.
 *
 * @author Andy Wang
 * @date 2026
 * @version 1.0
 * @license Public Domain
 *
 * @see FIPS 202: SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions
 * @see https://keccak.team/
 */

#include "Keccak.h"
#include <string.h>
#include <stdint.h>

/**
 * @defgroup Implementation Keccak Implementation Details
 * @ingroup KeccakCore
 * @{
 */

/**
 * @name Keccak Constants and Parameters
 * @{
 */

/** @brief Keccak state size in bytes (25 lanes of 64 bits each) */
#define KECCAK_STATE_SIZE 200

/** @brief Number of rounds in Keccak-f[1600] permutation */
#define KECCAK_ROUNDS 24

/** @} */ /* End of Constants */

/**
 * @name Internal Utility Functions
 * @{
 */

/**
 * @brief Rotate a 64-bit value left by n bits
 */
static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

/** @} */ /* End of Internal Utility Functions */

/**
 * @name Keccak-f[1600] Permutation
 * @{
 */

/**
 * @brief Keccak-f[1600] permutation function - FIPS 202 compliant
 */
void keccak_f1600(uint64_t state[25]) {
    int round, x, y, i, j, t;

    for (round = 0; round < KECCAK_ROUNDS; round++) {
        uint64_t C[5], D[5];
        uint64_t RC = 0;  /* Round constant */

        /* Theta */
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

        /* Rho and Pi combined - following official XKCP implementation */
        /* This is a state machine that performs rotations while permuting */
        uint64_t B[25];
        memcpy(B, state, sizeof(B));

        x = 1;
        y = 0;
        uint64_t current = B[x + 5 * y];

        for (t = 0; t < 24; t++) {
            int new_x = y;
            int new_y = (2 * x + 3 * y) % 5;
            int rho_offset = ((t + 1) * (t + 2)) / 2;

            uint64_t temp = B[new_x + 5 * new_y];
            B[new_x + 5 * new_y] = rotl64(current, rho_offset % 64);
            current = temp;

            x = new_x;
            y = new_y;
        }

        memcpy(state, B, sizeof(B));

        /* Chi */
        uint64_t temp_state[25];
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                i = x + 5 * y;
                temp_state[i] = state[i] ^ ((~state[(x + 1) % 5 + 5 * y]) & state[(x + 2) % 5 + 5 * y]);
            }
        }
        memcpy(state, temp_state, sizeof(temp_state));

        /* Iota - generate round constant via LFSR */
        /* Initialize R for this round */
        static uint8_t R_state = 0;
        if (round == 0) {
            R_state = 1;
        }

        for (j = 0; j < 7; j++) {
            R_state = ((R_state << 1) ^ ((R_state >> 7) * 0x71)) % 256;
            if (R_state & 2) {
                RC ^= (1ULL << ((1 << j) - 1));
            }
        }

        state[0] ^= RC;
    }
}

/** @} */ /* End of Keccak-f Permutation */

/**
 * @brief Load a 64-bit little-endian value from unaligned memory
 */
uint64_t load64_le(const uint8_t *p) {
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
void store64_le(uint8_t *p, uint64_t v) {
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
    p[2] = (v >> 16) & 0xFF;
    p[3] = (v >> 24) & 0xFF;
    p[4] = (v >> 32) & 0xFF;
    p[5] = (v >> 40) & 0xFF;
    p[6] = (v >> 48) & 0xFF;
    p[7] = (v >> 56) & 0xFF;
}

/** @} */ /* End of Implementation group */

/**
 * @defgroup Public Public API Functions
 * @{
 */

/**
 * @brief Generic Keccak hash function
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len) {
    uint64_t state[25] = {0};
    uint8_t state_bytes[KECCAK_STATE_SIZE];
    size_t rate = KECCAK_STATE_SIZE - 2 * output_len;  // Rate in bytes
    size_t i, j;

    /* Input validation */
    if (output == NULL || (input == NULL && input_len > 0) || output_len == 0) {
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

/** @} */ /* End of Public API Functions */

/** @} */ /* End of Keccak module */