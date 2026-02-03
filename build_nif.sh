#!/bin/bash
# Script to build the Erlang NIF for Keccak

# Check if Erlang is installed
if ! command -v erl &> /dev/null; then
    echo "Erlang is not installed or not in PATH"
    exit 1
fi

# Check if gcc is installed
if ! command -v gcc &> /dev/null; then
    echo "GCC is not installed or not in PATH"
    exit 1
fi

# Create priv directory if it doesn't exist
mkdir -p priv

# Get the current directory
CURRENT_DIR=$(pwd)

# Get Erlang include directory
ERLANG_INCLUDE_DIR=$(erl -eval "io:format(\"~s\", [code:root_dir()])" -s init stop -noshell)/usr/include

# Check if Erlang include directory exists
if [ ! -d "$ERLANG_INCLUDE_DIR" ]; then
    # Alternative location for some systems
    ERLANG_INCLUDE_DIR=$(erl -eval "io:format(\"~s/erts-~s/include\", [code:root_dir(), erlang:system_info(version)])" -s init stop -noshell)
fi

if [ ! -d "$ERLANG_INCLUDE_DIR" ]; then
    echo "Erlang include directory not found. Please check your Erlang installation."
    exit 1
fi

echo "Building Erlang NIF for Keccak..."
echo "Erlang include directory: $ERLANG_INCLUDE_DIR"

# Compile the NIF
gcc -O2 -fPIC -shared -I"$ERLANG_INCLUDE_DIR" -Isrc c_src/keccak_nif.c -o priv/keccak.so

if [ $? -eq 0 ]; then
    echo "Erlang NIF built successfully!"
    echo "Output: priv/keccak.so"
else
    echo "Failed to build Erlang NIF"
    exit 1
fi