# Keccak/SHA3 Library Documentation

This document explains how to generate and use the documentation for the Keccak/SHA3 library.

## Generating Documentation

The library uses Doxygen to generate documentation from source code comments. To generate the documentation:

### Prerequisites

Make sure you have Doxygen installed on your system:

```bash
# On Ubuntu/Debian
sudo apt-get install doxygen

# On macOS with Homebrew
brew install doxygen

# On Windows with Chocolatey
choco install doxygen
```

### Generate Documentation

Run the documentation generation script:

```bash
./generate_docs.sh
```

Alternatively, you can run Doxygen directly:

```bash
doxygen Doxyfile
```

The documentation will be generated in the `docs/html/` directory. Open `docs/html/index.html` in your browser to view it.

## Source Code Documentation

The library includes comprehensive documentation in the form of:

- **C/C++ files**: Documented using Doxygen comments (`/** ... */`)
- **Erlang files**: Documented using Edoc comments (`%% @doc ... %% @end`)

### C/C++ Documentation Standards

C/C++ source files use Doxygen-style comments:

```c
/**
 * @brief Brief description of the function
 * @param[in] param1 Description of input parameter
 * @param[out] param2 Description of output parameter
 * @return Description of return value
 */
int example_function(int param1, int* param2);
```

### Erlang Documentation Standards

Erlang source files use Edoc-style comments:

```erlang
%% @doc Brief description of the function
%% @param Param1 Description of input parameter
%% @return Description of return value
%% @end
-spec example_function(integer()) -> integer().
example_function(Param1) -> Param1.
```

## Documentation Structure

The library consists of several components:

- **Core C Implementation**: Located in `src/` directory
  - `Keccak.c/h`: Core Keccak-f[1600] permutation and sponge construction
  - `sha3.c/h`: SHA3-256/512/1024 and streaming API
- **Erlang NIF Interface**: Located in `c_src/keccak_nif.c`
- **C++ Wrapper**: Located in `src/keccak.hpp`

Each component is thoroughly documented with function descriptions, parameter details, and usage examples where appropriate.

## Building Documentation

The Doxyfile is configured to:

- Extract documentation from C, C++, and Erlang source files
- Generate HTML documentation with cross-references
- Include source code with syntax highlighting
- Create diagrams for class hierarchies where applicable

For more advanced documentation generation options, refer to the Doxygen manual or modify the Doxyfile as needed.