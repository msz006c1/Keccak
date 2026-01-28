#!/bin/bash
# Cross-platform build script for Keccak/SHA3 Library
# Supports cross-compilation for multiple platforms

set -e

echo ""
echo "╔═══════════════════════════════════════════════════════════╗"
echo "║       Keccak/SHA3 Library Build Script                    ║"
echo "║    Cross-Compilation Support Enabled                      ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# Detect platform
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    HOST_PLATFORM="Linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    HOST_PLATFORM="macOS"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
    HOST_PLATFORM="Windows"
else
    HOST_PLATFORM="Unknown"
fi

echo "Host platform detected: $HOST_PLATFORM"
echo ""

# Parse command-line arguments
BUILD_TYPE="Release"
TEST_BUILD=false
DOCS_BUILD=false
TARGET_PLATFORM=""
CHECK_TOOLCHAINS=false
PARALLEL_JOBS=${PARALLEL_JOBS:-4}

show_help() {
    cat << EOF
Usage: ./build.sh [options]

Build options:
  --debug            Build with debug symbols
  --test             Build and run tests
  --docs             Generate documentation
  --jobs N           Number of parallel build jobs (default: 4)

Cross-compilation targets:
  --target <target>  Specify target platform:
    windows-native     Windows x86_64 (default)
    windows-32         Windows x86
    linux-x86_64       Linux x86_64
    linux-arm64        Linux ARM64
    linux-arm          Linux ARMv7
    macos-x86_64       macOS Intel
    macos-arm64        macOS Apple Silicon
    ios-arm64          iOS ARM64
    android-arm64      Android ARM64
    android-arm        Android ARMv7

Platform packages:
  --windows-all      Build all Windows targets
  --linux-all        Build all Linux targets
  --macos-all        Build all macOS targets
  --ios              Build iOS
  --android-all      Build all Android targets
  --all-platforms    Build for all supported platforms

Utilities:
  --check-toolchains Check available toolchains
  --help             Show this help message

Examples:
  ./build.sh                              (default native build)
  ./build.sh --test                       (build and run tests)
  ./build.sh --target linux-arm64         (cross-compile for Linux ARM64)
  ./build.sh --all-platforms              (build all platforms)
  ./build.sh --check-toolchains           (verify toolchain installation)

EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        --test)
            TEST_BUILD=true
            shift
            ;;
        --docs)
            DOCS_BUILD=true
            shift
            ;;
        --target)
            TARGET_PLATFORM="$2"
            shift 2
            ;;
        --windows-all)
            TARGET_PLATFORM="windows-all"
            shift
            ;;
        --linux-all)
            TARGET_PLATFORM="linux-all"
            shift
            ;;
        --macos-all)
            TARGET_PLATFORM="macos-all"
            shift
            ;;
        --ios)
            TARGET_PLATFORM="ios"
            shift
            ;;
        --android-all)
            TARGET_PLATFORM="android-all"
            shift
            ;;
        --all-platforms)
            TARGET_PLATFORM="all-platforms"
            shift
            ;;
        --check-toolchains)
            CHECK_TOOLCHAINS=true
            shift
            ;;
        --jobs)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        --help)
            show_help
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            ;;
    esac
done

# Check toolchains if requested
if [[ "$CHECK_TOOLCHAINS" == true ]]; then
    echo ""
    echo "Checking for available cross-compilation toolchains..."
    echo ""
    
    toolchains=(
        "gcc:GCC (native)"
        "x86_64-linux-gnu-gcc:Linux x86_64"
        "aarch64-linux-gnu-gcc:Linux ARM64"
        "arm-linux-gnueabihf-gcc:Linux ARMv7"
        "x86_64-apple-darwin-clang:macOS Intel"
        "aarch64-apple-darwin-clang:macOS ARM64"
        "arm64-apple-ios-clang:iOS ARM64"
        "aarch64-linux-android-clang:Android ARM64"
        "armv7a-linux-android-clang:Android ARMv7"
    )
    
    found_any=0
    for tool_info in "${toolchains[@]}"; do
        IFS=':' read -r tool_cmd tool_name <<< "$tool_info"
        if command -v "$tool_cmd" &> /dev/null; then
            found_any=1
            echo "[✓] $tool_name: $(command -v $tool_cmd)"
        else
            echo "[✗] $tool_name: NOT FOUND"
        fi
    done
    
    echo ""
    if [[ $found_any -eq 1 ]]; then
        echo "Some toolchains found. Installation instructions:"
        echo "  macOS: brew install mingw-w64"
        echo "  Linux: sudo apt install gcc-arm-linux-gnueabihf gcc-aarch64-linux-gnu ..."
        echo "  WSL2:  Same as Linux"
    else
        echo "No toolchains found. Installation required."
    fi
    echo ""
    exit 0
fi

# Show build configuration
echo "Build Configuration:"
echo "  Type:       $BUILD_TYPE"
echo "  Target:     ${TARGET_PLATFORM:-native}"
echo "  Test:       $TEST_BUILD"
echo "  Docs:       $DOCS_BUILD"
echo "  Jobs:       $PARALLEL_JOBS"
echo ""

# Determine make command and target
if [[ -z "$TARGET_PLATFORM" ]]; then
    MAKE_TARGET="lib"
    MAKE_ARGS=""
else
    MAKE_TARGET="$TARGET_PLATFORM"
    MAKE_ARGS="TARGET=$TARGET_PLATFORM"
fi

# Build
echo "Building library ($BUILD_TYPE)..."
if [[ "$BUILD_TYPE" == "Debug" ]]; then
    make -j"$PARALLEL_JOBS" DEBUG=1 $MAKE_ARGS "$MAKE_TARGET"
else
    make -j"$PARALLEL_JOBS" $MAKE_ARGS "$MAKE_TARGET"
fi

echo "Build complete!"
echo ""

# Run tests if requested and native target
if [[ "$TEST_BUILD" == true ]]; then
    if [[ -z "$TARGET_PLATFORM" ]] || [[ "$TARGET_PLATFORM" == "native" ]]; then
        echo "Building and running tests..."
        make test
        echo ""
    else
        echo "Note: Tests can only run with native target"
        echo ""
    fi
fi

# Generate documentation if requested
if [[ "$DOCS_BUILD" == true ]]; then
    echo "Generating documentation..."
    if command -v doxygen &> /dev/null; then
        make docs
        echo "Documentation generated in docs/html/"
    else
        echo "Doxygen not installed. Skipping documentation generation."
    fi
    echo ""
fi

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║                    Build Complete!                        ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# List output files
if [[ -d "build" ]]; then
    echo "Output files in: build/"
    ls -lh build/ | grep -E '\.(a|lib|exe)' || true
    echo ""
fi
