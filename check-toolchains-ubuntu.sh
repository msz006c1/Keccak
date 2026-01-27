#!/bin/bash
###############################################################################
# Ubuntu Toolchain Detection Script
# 
# Purpose: Detect and verify cross-compilation toolchains available in Ubuntu
# Usage: ./check-toolchains-ubuntu.sh
# 
# Description:
#   This script checks for availability of various cross-compilation toolchains
#   commonly used for building Keccak/SHA3 library for multiple target platforms.
#   It displays detailed information about each toolchain and provides installation
#   instructions for missing tools using Ubuntu's apt package manager.
#
###############################################################################

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Counters
found_count=0
total_checks=0

# Detect if running with sudo/root
SUDO_PREFIX=""
if [[ $EUID -ne 0 ]]; then
   SUDO_PREFIX="sudo "
fi

# Function to check if a command exists
check_command() {
    if command -v "$1" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

# Function to get version of a command
get_version() {
    local cmd="$1"
    local flag="$2"
    
    if [ -z "$flag" ]; then
        flag="--version"
    fi
    
    if command -v "$cmd" &> /dev/null; then
        $cmd $flag 2>&1 | head -1
    fi
}

# Function to display toolchain check result
check_toolchain() {
    local toolchain_name="$1"
    local binary="$2"
    local description="$3"
    local package_name="$4"
    
    ((total_checks++))
    
    echo ""
    echo -e "${BLUE}[$total_checks]${NC} Checking for ${BLUE}${toolchain_name}${NC} - ${description}"
    
    if check_command "$binary"; then
        ((found_count++))
        echo -e "    ${GREEN}[✓]${NC} Found: ${binary}"
        local version=$(get_version "$binary")
        echo -e "    Version: ${GREEN}${version}${NC}"
        return 0
    else
        echo -e "    ${RED}[✗]${NC} Not found: ${binary}"
        if [ -n "$package_name" ]; then
            echo -e "    Install: ${YELLOW}apt install ${package_name}${NC}"
        fi
        return 1
    fi
}

###############################################################################
# Main Script
###############################################################################

clear

# Print header
echo ""
echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║${NC}     Ubuntu Cross-Compilation Toolchain Checker        ${BLUE}║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Print system information
echo -e "${YELLOW}System Information:${NC}"
echo "  OS: $(lsb_release -ds 2>/dev/null || echo 'Unknown Linux')"
echo "  Kernel: $(uname -r)"
echo "  Architecture: $(uname -m)"

if command -v lsb_release &> /dev/null; then
    echo "  Release: $(lsb_release -cs)"
fi
echo ""

# ============================================================================
# Check for Build Tools
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}1. Essential Build Tools${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

check_toolchain "GCC" "gcc" "GNU C Compiler" "build-essential"

check_toolchain "GCC++" "g++" "GNU C++ Compiler" "build-essential"

check_toolchain "GNU Make" "make" "Build automation tool" "build-essential"

check_toolchain "GNU Binutils" "ar" "Binary utilities for managing archives" "binutils"

check_toolchain "GDB" "gdb" "GNU Debugger" "gdb"

check_toolchain "Pkg-config" "pkg-config" "Package configuration utility" "pkg-config"

# ============================================================================
# Check for Cross-Compilation Toolchains - Linux
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}2. Linux Cross-Compilation Toolchains${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

echo ""
echo -e "${CYAN}ARM Architectures:${NC}"
check_toolchain "ARM (32-bit)" "arm-linux-gnueabihf-gcc" \
    "ARMv7 (32-bit hard float)" "gcc-arm-linux-gnueabihf"

check_toolchain "ARM64 (aarch64)" "aarch64-linux-gnu-gcc" \
    "ARM64 (aarch64)" "gcc-aarch64-linux-gnu"

echo ""
echo -e "${CYAN}x86 Architectures:${NC}"
check_toolchain "x86_64 (native)" "gcc" \
    "x86_64 native compiler" "build-essential"

check_toolchain "i686 (32-bit x86)" "gcc-i686-linux-gnu" \
    "i686 (32-bit x86)" "gcc-i686-linux-gnu"

echo ""
echo -e "${CYAN}PowerPC Architectures:${NC}"
check_toolchain "PowerPC 64 LE" "powerpc64le-linux-gnu-gcc" \
    "PowerPC 64-bit Little Endian" "gcc-powerpc64le-linux-gnu"

check_toolchain "PowerPC 32-bit" "powerpc-linux-gnu-gcc" \
    "PowerPC 32-bit" "gcc-powerpc-linux-gnu"

echo ""
echo -e "${CYAN}RISC-V Architectures:${NC}"
check_toolchain "RISC-V 64-bit" "riscv64-linux-gnu-gcc" \
    "RISC-V 64-bit" "gcc-riscv64-linux-gnu"

echo ""
echo -e "${CYAN}MIPS Architectures:${NC}"
check_toolchain "MIPS 32-bit" "mips-linux-gnu-gcc" \
    "MIPS 32-bit" "gcc-mips-linux-gnu"

check_toolchain "MIPS 64-bit LE" "mips64el-linux-gnu-gcc" \
    "MIPS 64-bit Little Endian" "gcc-mips64el-linux-gnu"

# ============================================================================
# Check for Cross-Compilation Toolchains - macOS / iOS
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}3. macOS / iOS Cross-Compilation Toolchains${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

echo -e "${YELLOW}(Note: Full iOS/macOS toolchains typically require Xcode on macOS)${NC}"
echo ""

check_toolchain "macOS Intel" "x86_64-apple-darwin-clang" \
    "macOS x86_64 (Intel)" "osxcross (compile from source)"

check_toolchain "macOS ARM64" "aarch64-apple-darwin-clang" \
    "macOS ARM64 (M1/M2)" "osxcross (compile from source)"

check_toolchain "iOS ARM64" "arm64-apple-ios-clang" \
    "iOS ARM64" "osxcross (compile from source)"

# ============================================================================
# Check for Windows Cross-Compilation Toolchains
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}4. Windows Cross-Compilation Toolchains${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

check_toolchain "MinGW-w64 (i686)" "i686-w64-mingw32-gcc" \
    "Windows x86 (32-bit)" "mingw-w64"

check_toolchain "MinGW-w64 (x86_64)" "x86_64-w64-mingw32-gcc" \
    "Windows x86_64 (64-bit)" "mingw-w64"

# ============================================================================
# Check for Android NDK
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}5. Android Cross-Compilation${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

if [ -n "$ANDROID_NDK_HOME" ] || [ -n "$ANDROID_SDK_ROOT" ]; then
    echo -e "${GREEN}[✓] Android environment detected${NC}"
    if [ -n "$ANDROID_NDK_HOME" ]; then
        echo "    NDK Home: $ANDROID_NDK_HOME"
    fi
    if [ -n "$ANDROID_SDK_ROOT" ]; then
        echo "    SDK Root: $ANDROID_SDK_ROOT"
    fi
    ((found_count++))
else
    echo -e "${RED}[✗] Android environment not configured${NC}"
    echo "    Set ANDROID_NDK_HOME or ANDROID_SDK_ROOT environment variables"
    echo "    Download from: https://developer.android.com/ndk"
fi

((total_checks++))

check_toolchain "Android NDK (aarch64)" "aarch64-linux-android-gcc" \
    "Android ARM64" "android-ndk (manual installation)"

check_toolchain "Android NDK (armv7a)" "armv7a-linux-androideabi-gcc" \
    "Android ARMv7" "android-ndk (manual installation)"

# ============================================================================
# Check for Additional Development Tools
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}6. Additional Development Tools${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

check_toolchain "Git" "git" "Version control system" "git"

check_toolchain "CMake" "cmake" "Build system generator" "cmake"

check_toolchain "Doxygen" "doxygen" "API documentation generator" "doxygen"

check_toolchain "Valgrind" "valgrind" "Memory debugging tool" "valgrind"

check_toolchain "gcov" "gcov" "Code coverage analysis" "gcc"

check_toolchain "clang" "clang" "LLVM C/C++ compiler" "clang"

check_toolchain "lldb" "lldb" "LLVM debugger" "lldb"

# ============================================================================
# Summary Report
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Summary${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

percentage=$((found_count * 100 / total_checks))

echo "Total Tools Checked: $total_checks"
echo -e "Tools Found: ${GREEN}$found_count${NC}"
echo -e "Tools Missing: ${RED}$((total_checks - found_count))${NC}"
echo "Coverage: ${percentage}%"
echo ""

if [ $found_count -eq $total_checks ]; then
    echo -e "${GREEN}✓ All toolchains are available!${NC}"
    echo "  Ready for comprehensive cross-compilation."
elif [ $found_count -ge 8 ]; then
    echo -e "${YELLOW}◐ Essential build tools found. Some cross-compilation toolchains missing.${NC}"
    echo "  You can build for the native platform and some targets."
else
    echo -e "${RED}✗ Some essential build tools are missing.${NC}"
    echo "  Please install required tools before attempting to build."
fi

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# ============================================================================
# Installation Instructions
# ============================================================================
echo -e "${YELLOW}Installation Instructions:${NC}"
echo ""

echo "1. ${CYAN}Quick Install (Essential Build Tools):${NC}"
echo "   ${YELLOW}${SUDO_PREFIX}apt update${NC}"
echo "   ${YELLOW}${SUDO_PREFIX}apt install build-essential gdb pkg-config${NC}"
echo ""

echo "2. ${CYAN}Install All Common Cross-Compilers:${NC}"
echo "   ${YELLOW}${SUDO_PREFIX}apt install build-essential gdb pkg-config \\${NC}"
echo "      gcc-arm-linux-gnueabihf gcc-aarch64-linux-gnu \\${NC}"
echo "      gcc-i686-linux-gnu gcc-powerpc64le-linux-gnu \\${NC}"
echo "      gcc-riscv64-linux-gnu mingw-w64 gcc-mips-linux-gnu${NC}"
echo ""

echo "3. ${CYAN}Install Specific Cross-Compiler:${NC}"
echo "   - ARM (32-bit): ${YELLOW}${SUDO_PREFIX}apt install gcc-arm-linux-gnueabihf${NC}"
echo "   - ARM64: ${YELLOW}${SUDO_PREFIX}apt install gcc-aarch64-linux-gnu${NC}"
echo "   - Windows MinGW: ${YELLOW}${SUDO_PREFIX}apt install mingw-w64${NC}"
echo "   - PowerPC 64LE: ${YELLOW}${SUDO_PREFIX}apt install gcc-powerpc64le-linux-gnu${NC}"
echo "   - RISC-V: ${YELLOW}${SUDO_PREFIX}apt install gcc-riscv64-linux-gnu${NC}"
echo ""

echo "4. ${CYAN}Install Development Tools:${NC}"
echo "   - CMake: ${YELLOW}${SUDO_PREFIX}apt install cmake${NC}"
echo "   - Doxygen: ${YELLOW}${SUDO_PREFIX}apt install doxygen graphviz${NC}"
echo "   - LLVM/Clang: ${YELLOW}${SUDO_PREFIX}apt install clang lldb lld${NC}"
echo "   - Code Analysis: ${YELLOW}${SUDO_PREFIX}apt install cppcheck clang-tools valgrind${NC}"
echo ""

echo "5. ${CYAN}Android NDK Setup:${NC}"
echo "   - Download from: https://developer.android.com/ndk/downloads"
echo "   - Extract to a directory, e.g.: ~/android-ndk"
echo "   - Add to .bashrc or .profile:"
echo "     ${YELLOW}export ANDROID_NDK_HOME=~/android-ndk/android-ndk-r25c${NC}"
echo "     ${YELLOW}export PATH=\$ANDROID_NDK_HOME/toolchains/llvm/prebuilt/linux-x86_64/bin:\$PATH${NC}"
echo ""

echo "6. ${CYAN}macOS/iOS Cross-Compilation:${NC}"
echo "   - osxcross requires: ${YELLOW}${SUDO_PREFIX}apt install clang llvm-dev libssl-dev lzma-dev uuid-dev${NC}"
echo "   - Build osxcross from source: https://github.com/tpoechtrager/osxcross"
echo ""

echo "7. ${CYAN}Update Package List (if needed):${NC}"
echo "   ${YELLOW}${SUDO_PREFIX}apt update${NC}"
echo "   ${YELLOW}${SUDO_PREFIX}apt upgrade${NC}"
echo ""

# ============================================================================
# Recommended Build Commands
# ============================================================================
echo -e "${YELLOW}Recommended Build Commands:${NC}"
echo ""

echo "1. ${CYAN}Build for native platform:${NC}"
echo "   ${YELLOW}make clean && make lib${NC}"
echo ""

echo "2. ${CYAN}Build and run tests:${NC}"
echo "   ${YELLOW}make test${NC}"
echo ""

echo "3. ${CYAN}Cross-compile for ARM:${NC}"
echo "   ${YELLOW}make TARGET=linux-arm clean lib${NC}"
echo ""

echo "4. ${CYAN}Cross-compile for ARM64:${NC}"
echo "   ${YELLOW}make TARGET=linux-arm64 clean lib${NC}"
echo ""

echo "5. ${CYAN}Cross-compile for Windows (MinGW):${NC}"
echo "   ${YELLOW}make TARGET=windows-32 clean lib${NC}"
echo "   ${YELLOW}make TARGET=windows-native clean lib${NC}"
echo ""

echo "6. ${CYAN}Generate documentation:${NC}"
echo "   ${YELLOW}make docs${NC}"
echo ""

echo "7. ${CYAN}Check build configuration:${NC}"
echo "   ${YELLOW}make info${NC}"
echo ""

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}For more information, see: README.md, BUILD_GUIDE.md${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

exit 0
