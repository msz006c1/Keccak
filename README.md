# Keccak Hash Algorithm

## Overview
Keccak is a cryptographic hash function that is the basis for the SHA-3 standard. It is designed to be secure, efficient, and flexible, making it suitable for a wide range of applications.

## Features
- Implements the Keccak hash algorithm.
- Supports SHA3-256 and SHA3-512 hash functions.
- Provides a simple interface for hashing data.

## Files
- `src/Keccak.c`: Contains the core implementation of the Keccak hash algorithm, including the permutation function and hash computation.
- `src/Keccak.h`: Header file that declares function prototypes and constants for use in other source files.
- `tests/test.c`: Unit tests for the Keccak algorithm, verifying the correctness of the implementation.
- `Makefile`: Build script that defines compilation rules and targets for the project.
- `LICENSE`: Contains the license information for the project.

## Usage
To use the Keccak hash functions, include the `Keccak.h` header in your source files and link against the compiled `Keccak.c` implementation.

### Example
```c
#include "Keccak.h"

int main() {
    uint8_t output[32]; // For SHA3-256
    const uint8_t input[] = "Hello, World!";
    sha3_256(output, input, sizeof(input) - 1);
    // Output the hash...
    return 0;
}
```

## Building the Project
To build the project, run the following command in the project directory:
```
make
```

## License
This project is licensed under the MIT License. See the LICENSE file for more details.