# Keccak/SHA3 Erlang Implementation

This directory contains an Erlang implementation of the Keccak/SHA3 hash functions, using NIFs (Native Implemented Functions) to interface with the optimized C implementation.

## Features

- **SHA3-256, SHA3-512, SHA3-1024** functions
- **Generic Keccak** hash function
- **Streaming API** for incremental hashing
- **FIPS 202 compliant**
- **High performance** through NIF integration

## Building

To build the Erlang NIF:

```bash
make -f Makefile.erlang
```

This will:
1. Compile the C NIF library to `priv/keccak.so`
2. Compile the Erlang module to `ebin/keccak.beam`

## Usage

### Single-call API

```erlang
% SHA3-256 hash
Hash = keccak:sha3_256(<<"Hello, World!">>).

% SHA3-512 hash
Hash = keccak:sha3_512(<<"Hello, World!">>).

% SHA3-1024 hash
Hash = keccak:sha3_1024(<<"Hello, World!">>).

% Generic Keccak hash (32-byte output)
Hash = keccak:keccak(<<"Hello, World!">>, 32).
```

### Streaming API

```erlang
% Initialize context for SHA3-256
Ctx = keccak:init(sha3_256),

% Update with data chunks
Ctx1 = keccak:update(Ctx, <<"Hello, ">>),
Ctx2 = keccak:update(Ctx1, <<"World!">>),

% Finalize to get hash
Hash = keccak:final(Ctx2).
```

## Testing

Run the test suite:

```bash
make -f Makefile.erlang test
```

Or manually:

```bash
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:./priv erl -pa ebin -eval 'test_keccak:run_tests(), halt().'
```

## Architecture

- `src/keccak.erl`: Erlang module with NIF interface
- `c_src/keccak_nif.c`: C NIF implementation with embedded Keccak functions
- `priv/keccak.so`: Compiled NIF library
- `test_keccak.erl`: Test suite

The NIF embeds the Keccak implementation directly to avoid linking issues, providing optimal performance while maintaining compatibility with the original C implementation.