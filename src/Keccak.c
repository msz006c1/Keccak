/**
 * @file Keccak.c
 * @brief Core Keccak-f[1600] permutation and sponge construction.
 *
 * Implements the core Keccak-f[1600] permutation and the sponge construction
 * for the Keccak hash function. Complies with FIPS 202 standard.
 * Public domain implementation.
 */

#include "Keccak.h"
#include <string.h>
#include <stdint.h>

#define KECCAK_STATE_SIZE 200 /**< Size of the Keccak state in bytes */
#define KECCAK_ROUNDS 24      /**< Number of rounds in the Keccak-f[1600] permutation */

/**
 * @brief Rotates a 64-bit value left by n bits.
 * @param[in] x The value to rotate
 * @param[in] n The number of bits to rotate
 * @return The rotated value
 */
static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

/**
 * @brief Performs the Keccak-f[1600] permutation (24 rounds).
 * @param[in,out] state The 1600-bit state to permute (25 uint64_t elements)
 */
void keccak_f1600(uint64_t state[25]) {
    int round, x, y, i, j, t;

    for (round = 0; round < KECCAK_ROUNDS; round++) {
        uint64_t C[5], D[5];
        uint64_t RC = 0;

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

        /* Rho and Pi */
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

        /* Iota */
        uint8_t R_state = 1;
        for (int r = 0; r < round; r++) {
            for (j = 0; j < 7; j++) {
                R_state = ((R_state << 1) ^ ((R_state >> 7) * 0x71)) % 256;
            }
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

/**
 * @brief Loads an 8-byte value in little-endian format.
 * @param[in] p Pointer to the 8-byte buffer to read from
 * @return The loaded 64-bit value
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
 * @brief Stores a 64-bit value in little-endian format.
 * @param[out] p Pointer to the buffer to store to
 * @param[in] v The 64-bit value to store
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

/**
 * @brief Computes a Keccak hash with SHA3 padding.
 * @param[out] output Buffer to store the hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 * @param[in] output_len Desired length of the output hash in bytes
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len) {
    uint64_t state[25] = {0};
    uint8_t state_bytes[KECCAK_STATE_SIZE];
    size_t rate = KECCAK_STATE_SIZE - 2 * output_len;
    size_t i, j;

    if (output == NULL || (input == NULL && input_len > 0) || output_len == 0) {
        return;
    }

    /* Absorb */
    for (i = 0; i < input_len; ) {
        size_t remaining = input_len - i;
        size_t block_size = (remaining < rate) ? remaining : rate;

        for (j = 0; j < 25; j++)
            store64_le(&state_bytes[j * 8], state[j]);

        for (j = 0; j < block_size; j++)
            state_bytes[j] ^= input[i + j];

        for (j = 0; j < 25; j++)
            state[j] = load64_le(&state_bytes[j * 8]);

        i += block_size;

        if (block_size == rate)
            keccak_f1600(state);
    }

    /* Pad (SHA3: 0x06 ... 0x80) */
    for (j = 0; j < 25; j++)
        store64_le(&state_bytes[j * 8], state[j]);

    size_t pos = input_len % rate;
    state_bytes[pos] ^= 0x06;
    state_bytes[rate - 1] ^= 0x80;

    for (j = 0; j < 25; j++)
        state[j] = load64_le(&state_bytes[j * 8]);
    keccak_f1600(state);

    /* Squeeze */
    for (i = 0; i < output_len; ) {
        for (j = 0; j < 25; j++)
            store64_le(&state_bytes[j * 8], state[j]);

        size_t remaining = output_len - i;
        size_t copy_size = (remaining < rate) ? remaining : rate;

        for (j = 0; j < copy_size; j++)
            output[i + j] = state_bytes[j];

        i += copy_size;

        if (i < output_len)
            keccak_f1600(state);
    }
}
