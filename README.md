# Keccak/SHA3 Implementation

## Overview
A FIPS 202 compliant implementation of the Keccak hash function family in C, C++, and Erlang.
Supports SHA3-256, SHA3-512, and SHA3-1024 (extended Keccak variant).

## Files
- `src/Keccak.c` / `src/Keccak.h` — Core Keccak-f[1600] permutation and sponge construction
- `src/sha3.c` / `src/sha3.h` — SHA3 wrapper functions (single-call and streaming API)
- `src/keccak.hpp` — C++ wrapper with RAII classes
- `src/keccak.erl` — Erlang NIF interface
- `c_src/keccak_nif.c` — Erlang NIF C implementation
- `Keccak_erl/keccak_real_fixed.erl` — Pure Erlang implementation
- `tests/test.c` — Unit tests verifying NIST test vectors
- `Makefile` — Build script

## Building

### C library and tests
```bash
make
```

### Erlang NIF
```bash
bash build_nif.sh
```

### Pure Erlang
```bash
cd Keccak_erl && mkdir -p ebin && erlc -o ebin *.erl
```

## Documentation

Comprehensive documentation for this library is available. See [DOCS.md](DOCS.md) for instructions on generating and viewing the documentation.

To generate documentation locally:

```bash
./generate_docs.sh
```

The documentation includes detailed API references for C, C++, and Erlang interfaces.

## Example
```c
#include "sha3.h"

int main() {
    uint8_t hash[32];
    sha3_256(hash, (const uint8_t*)"Hello, World!", 13);
    return 0;
}
```

## License
MIT License. See the LICENSE file for details.
