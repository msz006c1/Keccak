#!/bin/bash

# Script to generate documentation for the Keccak/SHA3 library
# This script creates documentation using Doxygen

echo "Generating documentation for Keccak/SHA3 library..."

# Check if Doxygen is installed
if ! command -v doxygen &> /dev/null; then
    echo "Error: Doxygen is not installed. Please install Doxygen to generate documentation."
    exit 1
fi

# Generate documentation
echo "Running Doxygen..."
doxygen Doxyfile

# Check if documentation was generated successfully
if [ -d "docs/html" ]; then
    echo "Documentation generated successfully!"
    echo "You can view it by opening docs/html/index.html in your browser."
else
    echo "Error: Documentation generation failed."
    exit 1
fi

echo "Documentation generation complete."