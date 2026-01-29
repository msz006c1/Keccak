#include "erl_nif.h"
#include <string.h>

// Include the Keccak implementation directly to avoid linking issues
// This is a simplified version focusing on the core functions we need

// Basic types and constants
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef size_t size_t;

#define KECCAK_STATE_SIZE 200
#define KECCAK_ROUNDS 24

// Forward declarations of the functions we'll implement below
void keccak_hash(uint8_t *output, const uint8_t *input, size_t input_len, size_t output_len);
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len);
void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len);
void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len);
void keccak_f1600(uint64_t state[25]);
uint64_t load64_le(const uint8_t *p);
void store64_le(uint8_t *p, uint64_t v);

// Implementation of utility functions
static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

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

// Keccak-f[1600] permutation function
void keccak_f1600(uint64_t state[25]) {
    int round, x, y, i, t;

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

        /* Rho and Pi combined */
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

        /* Iota - generate round constant */
        static uint8_t R_state = 0;
        if (round == 0) {
            R_state = 1;
        }

        int j;
        for (j = 0; j < 7; j++) {
            R_state = ((R_state << 1) ^ ((R_state >> 7) * 0x71)) % 256;
            if (R_state & 2) {
                RC ^= (1ULL << ((1 << j) - 1));
            }
        }

        state[0] ^= RC;
    }
}

// Generic Keccak hash function
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

// SHA3 functions
void sha3_256(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 32);
}

void sha3_512(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 64);
}

void sha3_1024(uint8_t *output, const uint8_t *input, size_t input_len) {
    keccak_hash(output, input, input_len, 128);
}

// Define sha3_ctx structure similar to the one in sha3.h
typedef struct {
    uint64_t state[25];              /**< Keccak state (25 x 64-bit words) */
    uint8_t state_bytes[200];        /**< State as bytes for I/O operations */
    size_t rate;                     /**< Absorption rate in bytes */
    size_t block_pos;                /**< Current position in absorption block */
    size_t output_len;               /**< Desired output hash length */
    int finalized;                   /**< Flag: hash finalized? (1=yes, 0=no) */
} sha3_ctx;

// Helper function to initialize SHA3 context
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

// Helper function to update SHA3 context
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

// Helper function to finalize SHA3 context
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

// Helper function to convert Erlang binary to C binary
static ERL_NIF_TERM binary_to_c(ErlNifEnv* env, ERL_NIF_TERM bin_term, unsigned char** out_data, size_t* out_len) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, bin_term, &bin)) {
        return enif_make_badarg(env);
    }

    *out_data = (unsigned char*)enif_alloc(bin.size);
    if (*out_data == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }

    memcpy(*out_data, bin.data, bin.size);
    *out_len = bin.size;

    return 0; // Success indicator
}

// Helper function to convert C binary to Erlang binary
static ERL_NIF_TERM c_to_binary(ErlNifEnv* env, unsigned char* data, size_t len) {
    ErlNifBinary bin;
    if (!enif_alloc_binary(len, &bin)) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }
    
    memcpy(bin.data, data, len);
    return enif_make_binary(env, &bin);
}

// NIF function for SHA3-256
static ERL_NIF_TERM nif_sha3_256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
    
    unsigned char output[32];
    sha3_256(output, input.data, input.size);
    
    return c_to_binary(env, output, 32);
}

// NIF function for SHA3-512
static ERL_NIF_TERM nif_sha3_512(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
    
    unsigned char output[64];
    sha3_512(output, input.data, input.size);
    
    return c_to_binary(env, output, 64);
}

// NIF function for SHA3-1024
static ERL_NIF_TERM nif_sha3_1024(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
    
    unsigned char output[128];
    sha3_1024(output, input.data, input.size);
    
    return c_to_binary(env, output, 128);
}

// NIF function for generic Keccak
static ERL_NIF_TERM nif_keccak(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary input;
    unsigned output_len;
    
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
    
    if (!enif_get_uint(env, argv[1], &output_len) || output_len == 0) {
        return enif_make_badarg(env);
    }
    
    if (output_len > 1024) { // Reasonable upper limit
        return enif_make_badarg(env);
    }
    
    unsigned char* output = (unsigned char*)enif_alloc(output_len);
    if (output == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }
    
    keccak_hash(output, input.data, input.size, output_len);
    
    ERL_NIF_TERM result = c_to_binary(env, output, output_len);
    enif_free(output);
    
    return result;
}

// Resource type for streaming context
typedef struct {
    sha3_ctx ctx;
    int finalized;
} keccak_stream_resource;

// Destructor for streaming context
static void stream_context_dtor(ErlNifEnv* env, void* obj) {
    // Nothing to free since sha3_ctx doesn't allocate dynamic memory
}

static ErlNifResourceType* STREAM_RESOURCE_TYPE = NULL;

// NIF initialization - create resource type
static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    ErlNifResourceType* rt;

    rt = enif_open_resource_type(env, NULL, "keccak_stream", stream_context_dtor,
                                 ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL) {
        return -1;
    }
    STREAM_RESOURCE_TYPE = rt;

    return 0;
}

// NIF function to initialize streaming context
static ERL_NIF_TERM nif_init_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    unsigned output_len;
    if (!enif_get_uint(env, argv[0], &output_len) || output_len == 0) {
        return enif_make_badarg(env);
    }

    keccak_stream_resource* res = (keccak_stream_resource*)enif_alloc_resource(STREAM_RESOURCE_TYPE, sizeof(keccak_stream_resource));
    if (res == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }

    sha3_init(&(res->ctx), output_len);
    res->finalized = 0;

    ERL_NIF_TERM result = enif_make_resource(env, res);
    enif_release_resource(res);
    return result;
}

// NIF function to update streaming context
static ERL_NIF_TERM nif_update_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    keccak_stream_resource* res;
    ErlNifBinary input;

    if (!enif_get_resource(env, argv[0], STREAM_RESOURCE_TYPE, (void**)&res)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }

    if (res->finalized) {
        return enif_make_atom(env, "finalized");
    }

    sha3_update(&(res->ctx), input.data, input.size);

    return argv[0]; // Return the same context
}

// NIF function to finalize streaming context
static ERL_NIF_TERM nif_final_stream(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    keccak_stream_resource* res;
    if (!enif_get_resource(env, argv[0], STREAM_RESOURCE_TYPE, (void**)&res)) {
        return enif_make_badarg(env);
    }

    if (res->finalized) {
        return enif_make_atom(env, "already_finalized");
    }

    // Determine output length from the context
    size_t output_len = res->ctx.output_len;
    unsigned char* output = (unsigned char*)enif_alloc(output_len);
    if (output == NULL) {
        return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
    }

    sha3_final(&(res->ctx), output, output_len);
    res->finalized = 1;

    ERL_NIF_TERM result = c_to_binary(env, output, output_len);
    enif_free(output);

    return result;
}

// NIF initialization
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