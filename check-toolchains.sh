#!/bin/bash
###############################################################################
# Cygwin Toolchain Detection Script
# 
# Purpose: Detect and verify cross-compilation toolchains available in Cygwin
# Usage: ./check-toolchains.sh
# 
# Description:
#   This script checks for availability of various cross-compilation toolchains
#   commonly used for building Keccak/SHA3 library for multiple target platforms.
#   It displays detailed information about each toolchain and provides installation
#   instructions for missing tools.
#
###############################################################################

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
found_count=0
total_checks=0

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
    local install_hint="$4"
    
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
        if [ -n "$install_hint" ]; then
            echo -e "    Install: ${YELLOW}${install_hint}${NC}"
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
echo -e "${BLUE}║${NC}   Cygwin Cross-Compilation Toolchain Checker          ${BLUE}║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Print system information
echo -e "${YELLOW}System Information:${NC}"
echo "  Platform: $(uname -s)"
echo "  Architecture: $(uname -m)"
echo "  Cygwin Version: $(uname -r)"
echo ""

# ============================================================================
# Check for Build Tools
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}1. Essential Build Tools${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

check_toolchain "GCC" "gcc" "GNU C Compiler (native)" \
    "cygwin-setup.exe or apt-cyg (if using apt-cyg)"

check_toolchain "GCC++" "g++" "GNU C++ Compiler (native)" \
    "cygwin-setup.exe"

check_toolchain "GNU Make" "make" "Build automation tool" \
    "cygwin-setup.exe"

check_toolchain "GNU Binutils" "ar" "Binary utilities for managing archives" \
    "cygwin-setup.exe"

check_toolchain "GDB" "gdb" "GNU Debugger" \
    "cygwin-setup.exe"

# ============================================================================
# Check for Cross-Compilation Toolchains
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}2. Cross-Compilation Toolchains${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

echo ""
echo -e "${YELLOW}Linux Targets:${NC}"
check_toolchain "Linux x86_64" "x86_64-linux-gnu-gcc" \
    "Linux x86_64 (64-bit)" \
    "apt-cyg install cross-gcc-linux-x86_64"

check_toolchain "Linux ARM64" "aarch64-linux-gnu-gcc" \
    "Linux ARM64 (aarch64)" \
    "apt-cyg install cross-gcc-linux-aarch64"

check_toolchain "Linux ARMv7" "arm-linux-gnueabihf-gcc" \
    "Linux ARMv7 (32-bit ARM)" \
    "apt-cyg install cross-gcc-linux-armv7l"

echo ""
echo -e "${YELLOW}Windows Targets:${NC}"
check_toolchain "Windows i686" "i686-w64-mingw32-gcc" \
    "Windows x86 (32-bit)" \
    "apt-cyg install cross-gcc-windows-i686"

check_toolchain "Windows x86_64" "x86_64-w64-mingw32-gcc" \
    "Windows x86_64 (64-bit)" \
    "apt-cyg install cross-gcc-windows-x86_64"

echo ""
echo -e "${YELLOW}macOS Targets:${NC}"
check_toolchain "macOS Intel" "x86_64-apple-darwin-clang" \
    "macOS x86_64 (Intel)" \
    "apt-cyg install cross-clang-darwin-x86_64"

check_toolchain "macOS Apple Silicon" "aarch64-apple-darwin-clang" \
    "macOS ARM64 (M1/M2)" \
    "apt-cyg install cross-clang-darwin-aarch64"

echo ""
echo -e "${YELLOW}iOS Targets:${NC}"
check_toolchain "iOS ARM64" "arm64-apple-ios-clang" \
    "iOS ARM64" \
    "apt-cyg install cross-clang-ios-arm64"

echo ""
echo -e "${YELLOW}Android Targets:${NC}"
check_toolchain "Android NDK (aarch64)" "aarch64-linux-android-clang" \
    "Android ARM64 (Clang)" \
    "Download and install Android NDK (r21+)"

check_toolchain "Android NDK (armv7a)" "armv7a-linux-android-clang" \
    "Android ARMv7 (Clang)" \
    "Download and install Android NDK (r21+)"

# ============================================================================
# Check for Additional Tools
# ============================================================================
echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}3. Additional Development Tools${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"

check_toolchain "Git" "git" "Version control system" \
    "apt-cyg install git"

check_toolchain "Doxygen" "doxygen" "API documentation generator" \
    "apt-cyg install doxygen"

check_toolchain "pkg-config" "pkg-config" "Package configuration utility" \
    "apt-cyg install pkg-config"

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
    echo "  Ready for full cross-compilation."
elif [ $found_count -ge 4 ]; then
    echo -e "${YELLOW}◐ Basic build tools found. Some cross-compilation toolchains missing.${NC}"
    echo "  You can build for the native platform, but cross-compilation may be limited."
else
    echo -e "${RED}✗ Essential build tools missing.${NC}"
    echo "  Please install Cygwin development tools before attempting to build."
fi

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# ============================================================================
# Installation Instructions
# ============================================================================
echo -e "${YELLOW}Installation Instructions:${NC}"
echo ""

echo "1. ${BLUE}Using Cygwin Setup:${NC}"
echo "   - Run: cygwin-setup.exe (or download from https://cygwin.com/)"
echo "   - Select packages in 'Devel' category"
echo "   - Search for: gcc-core, gcc-g++, make, binutils, gdb"
echo ""

echo "2. ${BLUE}Using apt-cyg (if installed):${NC}"
echo "   - Install single package: apt-cyg install <package>"
echo "   - Update: apt-cyg update"
echo "   - Install cross-compiler: apt-cyg install cross-gcc-<target>"
echo ""

echo "3. ${BLUE}Cygwin Package Search:${NC}"
echo "   - Official packages: https://cygwin.com/packages/"
echo "   - Search for: cross-gcc, cross-binutils, cross-gdb"
echo ""

echo "4. ${BLUE}Building Without Cross-Compilers:${NC}"
echo "   - You can still build for native Cygwin: make TARGET=native"
echo "   - Or: make clean && make"
echo ""

# ============================================================================
# Recommended Next Steps
# ============================================================================
echo -e "${YELLOW}Next Steps:${NC}"
echo ""
echo "1. Navigate to project directory:"
echo "   cd ~/keccak  (or appropriate path)"
echo ""
echo "2. Verify build configuration:"
echo "   make info"
echo ""
echo "3. Build the library:"
echo "   make clean && make lib"
echo ""
echo "4. Run tests:"
echo "   make test"
echo ""
echo "5. Cross-compile for a specific target:"
echo "   make TARGET=linux-x86_64 clean lib"
echo "   make TARGET=linux-arm clean lib"
echo ""

echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
echo ""

exit 0
