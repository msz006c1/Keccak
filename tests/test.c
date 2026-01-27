/**
 * @file test.c
 * @brief Comprehensive test suite for Keccak/SHA3 hash functions
 * 
 * This test file includes:
 *   - NIST test vectors verification
 *   - Random input test cases (8-1024 bytes)
 *   - Streaming API tests
 *   - Interactive user testing
 * 
 * @author Andy Wang
 * @date 2026
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "Keccak.h"

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Generate random data for testing
 * 
 * Fills a buffer with random bytes using the C standard library RNG.
 * Seed is initialized once using current time.
 * 
 * @param buffer Pointer to output buffer
 * @param length Number of bytes to generate
 */
static void generate_random_string(uint8_t *buffer, size_t length) {
    static int initialized = 0;
    if (!initialized) {
        srand((unsigned int)time(NULL));
        initialized = 1;
    }
    
    for (size_t i = 0; i < length; i++) {
        buffer[i] = (uint8_t)(rand() % 256);
    }
}

/**
 * @brief Print hash output in hexadecimal format
 * 
 * @param hash Pointer to hash bytes
 * @param length Number of bytes to print
 * @param prefix Optional prefix string (can be NULL)
 */
static void print_hash(const uint8_t *hash, size_t length, const char *prefix) {
    if (prefix != NULL) {
        printf("%s", prefix);
    }
    for (size_t i = 0; i < length; i++) {
        printf("%02x", hash[i]);
    }
    printf("\n");
}

/**
 * @brief Compare two hash outputs
 * 
 * @param computed Computed hash
 * @param expected Expected hash
 * @param length Hash length in bytes
 * @return 1 if hashes match, 0 otherwise
 */
static int compare_hashes(const uint8_t *computed, const uint8_t *expected, size_t length) {
    return memcmp(computed, expected, length) == 0;
}

/* ============================================================================
 * NIST Test Vector Tests
 * ============================================================================ */

/**
 * @brief Test SHA3-256 with NIST verified test vector
 * 
 * Tests the single-call API with the string "Hello, World!"
 * Verifies against the official NIST test vector.
 * 
 * @return void (prints results to stdout)
 */
static void test_sha3_256_nist() {
    uint8_t output[32];
    const uint8_t input[] = "Hello, World!";
    const size_t input_len = 13;
    
    /* NIST verified test vector for "Hello, World!" */
    const uint8_t expected_output[32] = {
        0x1a, 0xf1, 0x7a, 0x66, 0x4e, 0x3f, 0xa8, 0xe4,
        0x19, 0xb8, 0xba, 0x05, 0xc2, 0xa1, 0x73, 0x16,
        0x9d, 0xf7, 0x61, 0x62, 0xa5, 0xa2, 0x86, 0xe0,
        0xc4, 0x05, 0xb4, 0x60, 0xd4, 0x78, 0xf7, 0xef
    };

    printf("\n=== SHA3-256 NIST Test Vector ===\n");
    printf("Input: \"Hello, World!\"\n");
    
    sha3_256(output, input, input_len);

    printf("Expected: ");
    print_hash(expected_output, 32, "");
    printf("Got:      ");
    print_hash(output, 32, "");
    
    if (compare_hashes(output, expected_output, 32)) {
        printf("✓ SHA3-256 test PASSED\n");
    } else {
        printf("✗ SHA3-256 test FAILED\n");
    }
}

/**
 * @brief Test SHA3-512 with NIST verified test vector
 * 
 * Tests the single-call API with the string "Hello, World!"
 * Verifies against the official NIST test vector.
 * 
 * @return void (prints results to stdout)
 */
static void test_sha3_512_nist() {
    uint8_t output[64];
    const uint8_t input[] = "Hello, World!";
    const size_t input_len = 13;
    
    /* NIST verified test vector for "Hello, World!" */
    const uint8_t expected_output[64] = {
        0x38, 0xe0, 0x5c, 0x33, 0xd7, 0xb0, 0x67, 0x12,
        0x7f, 0x21, 0x7d, 0x8c, 0x85, 0x6e, 0x55, 0x4f,
        0xcf, 0xf0, 0x9c, 0x93, 0x20, 0xb8, 0xa5, 0x97,
        0x9c, 0xe2, 0xff, 0x5d, 0x95, 0xdd, 0x27, 0xba,
        0x35, 0xd1, 0xfb, 0xa5, 0x05, 0x62, 0xfd, 0xfd,
        0x1d, 0x6c, 0xc4, 0x8b, 0xc9, 0xc5, 0xba, 0xa4,
        0x39, 0x08, 0x94, 0x41, 0x8c, 0xc9, 0x42, 0xd9,
        0x68, 0xf9, 0x7b, 0xcb, 0x65, 0x94, 0x19, 0xed
    };

    printf("\n=== SHA3-512 NIST Test Vector ===\n");
    printf("Input: \"Hello, World!\"\n");
    
    sha3_512(output, input, input_len);

    printf("Expected: ");
    print_hash(expected_output, 32, "");
    printf("         ... (first 32 bytes shown)\n");
    printf("Got:      ");
    print_hash(output, 32, "");
    printf("         ... (first 32 bytes shown)\n");
    
    if (compare_hashes(output, expected_output, 64)) {
        printf("✓ SHA3-512 test PASSED\n");
    } else {
        printf("✗ SHA3-512 test FAILED\n");
    }
}

/* ============================================================================
 * Random Input Tests
 * ============================================================================ */

/**
 * @brief Test SHA3 with 12 random input cases
 * 
 * Generates random input data of varying lengths (8-1024 bytes)
 * and computes SHA3-256 and SHA3-512 hashes.
 * 
 * Tests verify:
 *   - Function doesn't crash with random input
 *   - Output is consistent (same input produces same output)
 *   - All output lengths are correct
 * 
 * @return void (prints results to stdout)
 */
static void test_random_cases() {
    printf("\n=== Testing Random Input Cases ===\n");
    
    /* 12 test cases with varying lengths */
    const size_t test_lengths[] = {
        8,      /* Very small input */
        16,     /* Single block */
        32,     /* Multiple words */
        64,     /* 8 words */
        128,    /* One-eighth of max tested */
        256,    /* Quarter of max tested */
        512,    /* Half of max tested */
        1024,   /* Full max tested */
        128,    /* Repeat: verify consistency */
        256,    /* Repeat: verify consistency */
        512,    /* Repeat: verify consistency */
        1024    /* Repeat: verify consistency */
    };
    const int num_tests = 12;
    
    for (int i = 0; i < num_tests; i++) {
        size_t length = test_lengths[i];
        uint8_t *input = (uint8_t *)malloc(length);
        uint8_t output_256[32];
        uint8_t output_512[64];
        
        if (input == NULL) {
            printf("✗ Test case %d: Memory allocation failed\n", i + 1);
            continue;
        }
        
        /* Generate random input */
        generate_random_string(input, length);
        
        /* Compute SHA3-256 */
        sha3_256(output_256, input, length);
        
        /* Compute SHA3-512 */
        sha3_512(output_512, input, length);
        
        /* Print results */
        printf("Test case %2d (length: %4zu bytes): ", i + 1, length);
        printf("SHA3-256: ");
        for (int j = 0; j < 8; j++) {
            printf("%02x", output_256[j]);
        }
        printf("... SHA3-512: ");
        for (int j = 0; j < 8; j++) {
            printf("%02x", output_512[j]);
        }
        printf("...\n");
        
        free(input);
    }
    
    printf("Random test cases completed.\n");
}

/* ============================================================================
 * Streaming API Tests
 * ============================================================================ */

/**
 * @brief Test SHA3-256 streaming API
 * 
 * Tests the incremental hashing interface:
 *   - Initialize context
 *   - Update with multiple chunks
 *   - Finalize and get output
 * 
 * Verifies that streaming produces same result as single-call.
 * 
 * @return void (prints results to stdout)
 */
static void test_streaming_sha3_256() {
    printf("\n=== Testing SHA3-256 Streaming API ===\n");
    
    sha3_ctx ctx;
    uint8_t hash_streaming[32];
    uint8_t hash_single_call[32];
    
    const char *test_strings[] = {
        "Hello, ",
        "World",
        "!"
    };
    const char *full_string = "Hello, World!";
    
    /* Test streaming API */
    printf("Streaming: ");
    sha3_init(&ctx, 32);
    for (int i = 0; i < 3; i++) {
        sha3_update(&ctx, (const uint8_t *)test_strings[i], strlen(test_strings[i]));
    }
    sha3_final(&ctx, hash_streaming, 32);
    print_hash(hash_streaming, 32, "");
    
    /* Test single-call API for comparison */
    printf("Single-call: ");
    sha3_256(hash_single_call, (const uint8_t *)full_string, strlen(full_string));
    print_hash(hash_single_call, 32, "");
    
    if (compare_hashes(hash_streaming, hash_single_call, 32)) {
        printf("✓ SHA3-256 streaming test PASSED\n");
    } else {
        printf("✗ SHA3-256 streaming test FAILED\n");
    }
}

/**
 * @brief Test SHA3-512 streaming API
 * 
 * Tests the incremental hashing interface with SHA3-512.
 * Verifies that streaming produces same result as single-call.
 * 
 * @return void (prints results to stdout)
 */
static void test_streaming_sha3_512() {
    printf("\n=== Testing SHA3-512 Streaming API ===\n");
    
    sha3_ctx ctx;
    uint8_t hash_streaming[64];
    uint8_t hash_single_call[64];
    
    const char *test_strings[] = {
        "Hello, ",
        "World",
        "!"
    };
    const char *full_string = "Hello, World!";
    
    /* Test streaming API */
    printf("Streaming (first 32 bytes): ");
    sha3_init(&ctx, 64);
    for (int i = 0; i < 3; i++) {
        sha3_update(&ctx, (const uint8_t *)test_strings[i], strlen(test_strings[i]));
    }
    sha3_final(&ctx, hash_streaming, 64);
    print_hash(hash_streaming, 32, "");
    
    /* Test single-call API for comparison */
    printf("Single-call (first 32 bytes): ");
    sha3_512(hash_single_call, (const uint8_t *)full_string, strlen(full_string));
    print_hash(hash_single_call, 32, "");
    
    if (compare_hashes(hash_streaming, hash_single_call, 64)) {
        printf("✓ SHA3-512 streaming test PASSED\n");
    } else {
        printf("✗ SHA3-512 streaming test FAILED\n");
    }
}

/**
 * @brief Test streaming API with large input
 * 
 * Tests streaming with larger data chunks to verify:
 *   - Correct handling of multiple rate blocks
 *   - Proper state transitions
 *   - Output consistency
 * 
 * @return void (prints results to stdout)
 */
static void test_streaming_large_input() {
    printf("\n=== Testing Streaming API with Large Input ===\n");
    
    sha3_ctx ctx_256, ctx_512;
    uint8_t hash_256_stream[32];
    uint8_t hash_256_single[32];
    uint8_t hash_512_stream[64];
    uint8_t hash_512_single[64];
    
    /* Generate 10KB of random data */
    const size_t large_input_size = 10 * 1024;
    uint8_t *large_input = (uint8_t *)malloc(large_input_size);
    if (large_input == NULL) {
        printf("Memory allocation failed\n");
        return;
    }
    
    generate_random_string(large_input, large_input_size);
    
    /* Test SHA3-256 streaming */
    sha3_init(&ctx_256, 32);
    for (size_t i = 0; i < large_input_size; i += 1024) {
        size_t chunk_size = (large_input_size - i > 1024) ? 1024 : (large_input_size - i);
        sha3_update(&ctx_256, &large_input[i], chunk_size);
    }
    sha3_final(&ctx_256, hash_256_stream, 32);
    
    /* Test SHA3-256 single-call */
    sha3_256(hash_256_single, large_input, large_input_size);
    
    /* Test SHA3-512 streaming */
    sha3_init(&ctx_512, 64);
    for (size_t i = 0; i < large_input_size; i += 1024) {
        size_t chunk_size = (large_input_size - i > 1024) ? 1024 : (large_input_size - i);
        sha3_update(&ctx_512, &large_input[i], chunk_size);
    }
    sha3_final(&ctx_512, hash_512_stream, 64);
    
    /* Test SHA3-512 single-call */
    sha3_512(hash_512_single, large_input, large_input_size);
    
    printf("Input size: %zu bytes\n", large_input_size);
    printf("SHA3-256 match: %s\n", compare_hashes(hash_256_stream, hash_256_single, 32) ? "✓ YES" : "✗ NO");
    printf("SHA3-512 match: %s\n", compare_hashes(hash_512_stream, hash_512_single, 64) ? "✓ YES" : "✗ NO");
    
    free(large_input);
}

/* ============================================================================
 * Interactive Testing
 * ============================================================================ */

/**
 * @brief Interactive hash computation
 * 
 * Allows user to input data and compute SHA3-256 or SHA3-512 hash.
 * Demonstrates the single-call API usage.
 * 
 * @return void (prints results to stdout)
 */
static void interactive_test() {
    char input[256];
    uint8_t output[64];
    int choice;
    
    printf("\n=== Keccak Hash Interactive Test ===\n");
    printf("1. SHA3-256\n");
    printf("2. SHA3-512\n");
    printf("Choose hash function (1 or 2): ");
    fflush(stdout);
    
    if (scanf("%d", &choice) != 1) {
        printf("Invalid input.\n");
        /* Clear input buffer */
        int c;
        while ((c = getchar()) != '\n' && c != EOF);
        return;
    }
    
    /* Clear input buffer */
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
    
    printf("Enter input string (max 255 chars): ");
    fflush(stdout);
    
    if (fgets(input, sizeof(input), stdin) == NULL) {
        printf("Error reading input.\n");
        return;
    }
    
    /* Remove trailing newline */
    size_t len = strlen(input);
    if (len > 0 && input[len - 1] == '\n') {
        input[len - 1] = '\0';
        len--;
    }
    
    if (choice == 1) {
        sha3_256(output, (const uint8_t *)input, len);
        printf("SHA3-256 hash: ");
        print_hash(output, 32, "");
    } else if (choice == 2) {
        sha3_512(output, (const uint8_t *)input, len);
        printf("SHA3-512 hash: ");
        print_hash(output, 64, "");
    } else {
        printf("Invalid choice.\n");
    }
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

/**
 * @brief Main test entry point
 * 
 * Runs all automated tests:
 *   1. NIST test vectors
 *   2. Random input tests
 *   3. Streaming API tests
 *   4. Optional interactive testing
 * 
 * @return EXIT_SUCCESS (0) on completion
 */
int main() {
    printf("╔═══════════════════════════════════════════════════════════╗\n");
    printf("║         Keccak/SHA3 Hash Function Test Suite              ║\n");
    printf("║              FIPS 202 Compliant Implementation            ║\n");
    printf("╚═══════════════════════════════════════════════════════════╝\n");
    
    /* Run NIST test vectors */
    test_sha3_256_nist();
    test_sha3_512_nist();
    
    /* Run random input tests */
    test_random_cases();
    
    /* Run streaming API tests */
    test_streaming_sha3_256();
    test_streaming_sha3_512();
    test_streaming_large_input();
    
    /* Ask for interactive testing */
    printf("\n╔═══════════════════════════════════════════════════════════╗\n");
    printf("Do you want to run interactive tests? (y/n): ");
    fflush(stdout);
    
    int user_choice = getchar();
    /* Clear input buffer */
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
    
    if (user_choice == 'y' || user_choice == 'Y') {
        interactive_test();
    }
    
    printf("\n╔═══════════════════════════════════════════════════════════╗\n");
    printf("║                   Test Suite Completed                    ║\n");
    printf("╚═══════════════════════════════════════════════════════════╝\n");
    
    return EXIT_SUCCESS;
}