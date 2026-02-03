#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "sha3.h"

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <algorithm> <input>\n", argv[0]);
        fprintf(stderr, "Algorithms: sha3-256, sha3-512, sha3-1024\n");
        return 1;
    }

    const char *algo = argv[1];
    const char *input = argv[2];
    size_t input_len = strlen(input);

    uint8_t output[128];
    size_t output_len;

    if (strcmp(algo, "sha3-256") == 0) {
        output_len = 32;
        sha3_256(output, (const uint8_t *)input, input_len);
    } else if (strcmp(algo, "sha3-512") == 0) {
        output_len = 64;
        sha3_512(output, (const uint8_t *)input, input_len);
    } else if (strcmp(algo, "sha3-1024") == 0) {
        output_len = 128;
        sha3_1024(output, (const uint8_t *)input, input_len);
    } else {
        fprintf(stderr, "Unknown algorithm: %s\n", algo);
        return 2;
    }

    for (size_t i = 0; i < output_len; i++) {
        printf("%02x", output[i]);
    }
    printf("\n");

    return 0;
}
