/**
 * @file keccak_nif.c
 * @brief Erlang NIF interface for Keccak/SHA3 hash functions.
 *
 * This file implements an Erlang NIF (Native Implemented Function) interface
 * for the Keccak/SHA3 hash functions. It embeds the Keccak implementation
 * to avoid linking issues and provides both single-call and streaming APIs.
 */

#include "erl_nif.h"
#include <string.h>

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef size_t size_t;

#define KECCAK_STATE_SIZE 200 /**< Size of the Keccak state in bytes */
#define KECCAK_ROUNDS 24      /**< Number of rounds in the Keccak-f[1600] permutation */

/**
 * @brief Computes a Keccak hash with SHA3 padding.
 * @param[out] output Buffer to store the hash result
 * @param[in] input Input data to hash
 * @param[in] input_len Length of the input data in bytes
 * @param[in] output_len Desired length of the output hash in bytes
 */
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len);

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
 * @brief Performs the Keccak-f[1600] permutation (24 rounds).
 * @param[in,out] state The 1600-bit state to permute (25 uint64_t elements)
 */
void keccak_f1600(uint64_t state[25]) {
    int round, x, y, i, t;

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
        for(i = 0; i < 25; i++) {
            B[i] = state[i];
        }

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

        for(i = 0; i < 25; i++) {
            state[i] = B[i];
        }

        /* Chi */
        uint64_t temp_state[25];
        for (y = 0; y < 5; y++) {
            for (x = 0; x < 5; x++) {
                i = x + 5 * y;
                temp_state[i] = state[i] ^ ((~state[(x + 1) % 5 + 5 * y]) & state[(x + 2) % 5 + 5 * y]);
            }
        }
        for(i = 0; i < 25; i++) {
            state[i] = temp_state[i];
        }

        /* Iota */
        uint8_t R_state = 1;
        int j;
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

/* Erlang NIF helpers */

/**
 * @brief Converts C binary data to an Erlang binary term.
 * @param[in] env The Erlang environment
 * @param[in] data Pointer to the binary data
 * @param[in] len Length of the binary data
 * @return An Erlang binary term
 */
static ERL_NIF_TERM c_to_binary(ErlNifEnv* env, unsigned char* data, size_t len) {
    ErlNifBinary bin;
    if (!enif_alloc_binary(len, &bin)) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }
    memcpy(bin.data, data, len);
    return enif_make_binary(env, &bin);
}

/* NIF functions */

/**
 * @brief NIF wrapper for SHA3-256 hash function.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The hash result as an Erlang binary
 */
static ERL_NIF_TERM nif_sha3_256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);

    unsigned char output[32];
    sha3_256(output, input.data, input.size);
    return c_to_binary(env, output, 32);
}

/**
 * @brief NIF wrapper for SHA3-512 hash function.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The hash result as an Erlang binary
 */
static ERL_NIF_TERM nif_sha3_512(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);

    unsigned char output[64];
    sha3_512(output, input.data, input.size);
    return c_to_binary(env, output, 64);
}

/**
 * @brief NIF wrapper for SHA3-1024 hash function.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The hash result as an Erlang binary
 */
static ERL_NIF_TERM nif_sha3_1024(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);

    unsigned char output[128];
    sha3_1024(output, input.data, input.size);
    return c_to_binary(env, output, 128);
}

/**
 * @brief NIF wrapper for generic Keccak hash function.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The hash result as an Erlang binary
 */
static ERL_NIF_TERM nif_keccak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ErlNifBinary input;
    unsigned output_len;

    if (!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &output_len) || output_len == 0) return enif_make_badarg(env);
    if (output_len > 1024) return enif_make_badarg(env);

    unsigned char* output = (unsigned char*)enif_alloc(output_len);
    if (output == NULL) return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));

    keccak_hash(output, input.data, input.size, output_len);

    ERL_NIF_TERM result = c_to_binary(env, output, output_len);
    enif_free(output);
    return result;
}

/* Streaming NIF resources */

/**
 * @struct keccak_stream_resource
 * @brief Resource structure for streaming hash operations in Erlang.
 */
typedef struct {
    sha3_ctx ctx;              /**< The underlying SHA3 context */
    int finalized;             /**< Flag indicating if the resource has been finalized */
} keccak_stream_resource;

/**
 * @brief Destructor for the streaming context resource.
 * @param[in] env The Erlang environment
 * @param[in] obj Pointer to the resource object
 */
static void stream_context_dtor(ErlNifEnv* env, void* obj) {
    (void)env; (void)obj;
}

static ErlNifResourceType* STREAM_RESOURCE_TYPE = NULL;

/**
 * @brief Load function for the NIF module.
 * @param[in] env The Erlang environment
 * @param[in] priv_data Private data pointer
 * @param[in] load_info Load information term
 * @return 0 on success, -1 on failure
 */
static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    (void)priv_data; (void)load_info;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "keccak_stream",
                                                      stream_context_dtor, ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL) return -1;
    STREAM_RESOURCE_TYPE = rt;
    return 0;
}

/**
 * @brief NIF wrapper to initialize a streaming hash context.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return A reference to the initialized context
 */
static ERL_NIF_TERM nif_init_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    unsigned output_len;
    if (!enif_get_uint(env, argv[0], &output_len) || output_len == 0) return enif_make_badarg(env);

    keccak_stream_resource* res = (keccak_stream_resource*)enif_alloc_resource(
        STREAM_RESOURCE_TYPE, sizeof(keccak_stream_resource));
    if (res == NULL) return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));

    sha3_init(&(res->ctx), output_len);
    res->finalized = 0;

    ERL_NIF_TERM result = enif_make_resource(env, res);
    enif_release_resource(res);
    return result;
}

/**
 * @brief NIF wrapper to update a streaming hash context.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The updated context reference
 */
static ERL_NIF_TERM nif_update_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    keccak_stream_resource* res;
    ErlNifBinary input;

    if (!enif_get_resource(env, argv[0], STREAM_RESOURCE_TYPE, (void**)&res)) return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &input)) return enif_make_badarg(env);
    if (res->finalized) return enif_make_atom(env, "finalized");

    sha3_update(&(res->ctx), input.data, input.size);
    return argv[0];
}

/**
 * @brief NIF wrapper to finalize a streaming hash context.
 * @param[in] env The Erlang environment
 * @param[in] argc Number of arguments
 * @param[in] argv Array of argument terms
 * @return The hash result as an Erlang binary
 */
static ERL_NIF_TERM nif_final_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    keccak_stream_resource* res;
    if (!enif_get_resource(env, argv[0], STREAM_RESOURCE_TYPE, (void**)&res)) return enif_make_badarg(env);
    if (res->finalized) return enif_make_atom(env, "already_finalized");

    size_t output_len = res->ctx.output_len;
    unsigned char* output = (unsigned char*)enif_alloc(output_len);
    if (output == NULL) return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));

    sha3_final(&(res->ctx), output, output_len);
    res->finalized = 1;

    ERL_NIF_TERM result = c_to_binary(env, output, output_len);
    enif_free(output);
    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"sha3_256_nif", 1, nif_sha3_256},
    {"sha3_512_nif", 1, nif_sha3_512},
    {"sha3_1024_nif", 1, nif_sha3_1024},
    {"keccak_nif", 2, nif_keccak},
    {"init_stream_nif", 1, nif_init_stream},
    {"update_stream_nif", 2, nif_update_stream},
    {"final_stream_nif", 1, nif_final_stream}
};

ERL_NIF_INIT(keccak, nif_funcs, nif_load, NULL, NULL, NULL)
