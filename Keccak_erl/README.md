# Keccak Native Erlang Implementation

This directory contains a native Erlang implementation of the Keccak hash function (SHA3 foundation).

## Files

- `keccak_real_fixed.erl`: The main implementation of the Keccak hash functions with proper Keccak-f[1600] permutation and sponge construction
- `keccak_real_test.erl`: Test module for the Keccak implementation
- `keccak_verification.erl`: Verification module that tests correctness against known vectors
- `stream_test.erl`: Test module for streaming functionality
- `Makefile`: Build and test automation
- `README.md`: This file

## Features

- Pure Erlang implementation (no NIFs required)
- Full implementation of the Keccak-f[1600] permutation
- Proper sponge construction with SHA3 padding
- Support for SHA3-256, SHA3-512, SHA3-1024, and custom Keccak
- Streaming API support
- FIPS 202 compliant

## API

The module provides the following functions:

- `sha3_256/1` - Compute SHA3-256 hash of binary data
- `sha3_512/1` - Compute SHA3-512 hash of binary data
- `sha3_1024/1` - Compute SHA3-1024 hash of binary data
- `keccak/2` - Generic Keccak hash function with custom output length
- `init/1` - Initialize streaming hash context
- `update/2` - Update streaming hash with new data
- `final/1` - Finalize streaming hash and return result

## Building and Testing

To compile and run tests:

```bash
make test
```

To compile only:

```bash
make
```

To clean compiled files:

```bash
make clean
```

## Note

This is a complete implementation of the Keccak algorithm with the proper state permutation and sponge construction, compliant with FIPS 202 SHA3 standard.