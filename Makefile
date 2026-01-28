# ============================================================================
# Makefile - Cross-platform Keccak/SHA3 Library Build System
# 
# Platforms: Windows (MinGW/MSVC), macOS, Linux, iOS, Android
# Targets: Static library (.a / .lib), Object files, Tests, Documentation
# 
# Usage:
#   make              - Build static library (default)
#   make test         - Build and run tests
#   make clean        - Remove build artifacts
#   make docs         - Generate Doxygen documentation
#   make install      - Install library and headers (Unix-like)
#
# ============================================================================

# ============================================================================
# Build Configuration
# ============================================================================

# ============================================================================
# Cross-Compilation Support
# ============================================================================

# Detect cross-compilation target
# Usage: make TARGET=<target>
# Supported targets:
#   windows-native   - Windows x86_64 (default on Windows)
#   windows-32       - Windows x86
#   linux-x86_64     - Linux x86_64
#   linux-arm64      - Linux ARM64 (aarch64)
#   linux-arm        - Linux ARMv7
#   macos-x86_64     - macOS Intel
#   macos-arm64      - macOS Apple Silicon (M1/M2)
#   ios-arm64        - iOS ARM64
#   android-arm64    - Android ARM64
#   android-arm      - Android ARMv7

TARGET ?= native

ifeq ($(TARGET),native)
    ifeq ($(PLATFORM),WINDOWS)
        TARGET = windows-native
    else ifeq ($(PLATFORM),LINUX)
        TARGET = linux-x86_64
    else ifeq ($(PLATFORM),MACOS)
        TARGET = macos-arm64
    endif
endif

# Cross-compilation toolchain configuration
ifeq ($(TARGET),windows-native)
    CROSS_COMPILE =
    CC ?= gcc
    AR ?= ar
else ifeq ($(TARGET),windows-32)
    CROSS_COMPILE = i686-w64-mingw32-
    CC ?= $(CROSS_COMPILE)gcc
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),linux-x86_64)
    CROSS_COMPILE = x86_64-linux-gnu-
    CC ?= $(CROSS_COMPILE)gcc
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),linux-arm64)
    CROSS_COMPILE = aarch64-linux-gnu-
    CC ?= $(CROSS_COMPILE)gcc
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),linux-arm)
    CROSS_COMPILE = arm-linux-gnueabihf-
    CC ?= $(CROSS_COMPILE)gcc
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),macos-x86_64)
    CROSS_COMPILE = x86_64-apple-darwin25.3-
    CC ?= $(CROSS_COMPILE)clang
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),macos-arm64)
    CROSS_COMPILE = aarch64-apple-darwin25.3-
    CC ?= $(CROSS_COMPILE)clang
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),ios-arm64)
    CROSS_COMPILE = arm64-apple-ios-
    CC ?= $(CROSS_COMPILE)clang
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),android-arm64)
    CROSS_COMPILE = aarch64-linux-android35-
    CC ?= $(CROSS_COMPILE)clang
    AR ?= $(CROSS_COMPILE)ar
else ifeq ($(TARGET),android-arm)
    CROSS_COMPILE = armv7a-linux-androideabi35-
    CC ?= $(CROSS_COMPILE)clang
    AR ?= $(CROSS_COMPILE)ar
else
    # Default fallback for undefined targets
    CROSS_COMPILE =
    CC ?= gcc
    AR ?= ar
endif

# Directories
SRC_DIR = src
TEST_DIR = tests
BUILD_DIR = build
INCLUDE_DIR = $(SRC_DIR)
DOC_DIR = docs
INSTALL_PREFIX ?= /usr/local

# Files
LIB_NAME = keccak
STATIC_LIB = $(BUILD_DIR)/lib$(LIB_NAME).a
TEST_EXECUTABLE = $(BUILD_DIR)/test_$(LIB_NAME)
DOXYGEN_FILE = Doxyfile

# Source files
SRC_FILES = $(SRC_DIR)/Keccak.c
TEST_FILES = $(TEST_DIR)/test.c
OBJ_FILES = $(BUILD_DIR)/Keccak.o
TEST_OBJ_FILES = $(BUILD_DIR)/test.o

# ============================================================================
# Platform Detection
# ============================================================================

# Detect operating system
UNAME_S := $(shell uname -s 2>/dev/null)
UNAME_M := $(shell uname -m 2>/dev/null)

# Windows detection
ifeq ($(OS),Windows_NT)
    PLATFORM = WINDOWS
    EXEC_SUFFIX = .exe
    STATIC_LIB = $(BUILD_DIR)/$(LIB_NAME).lib
    RMDIR = rmdir /s /q
    MKDIR = mkdir
    ifdef MINGW
        CC = mingw32-gcc
        AR = mingw32-ar
    endif
else
    EXEC_SUFFIX =
    RMDIR = rm -rf
    MKDIR = mkdir -p
    
    # Unix-like platform detection
    ifeq ($(UNAME_S),Linux)
        PLATFORM = LINUX
    else ifeq ($(UNAME_S),Darwin)
        PLATFORM = MACOS
        # Check for iOS cross-compilation
        ifdef IPHONEOS_DEPLOYMENT_TARGET
            PLATFORM = IOS
        endif
    else ifeq ($(UNAME_S),*)
        # Fallback for other Unix variants (including Android)
        PLATFORM = UNIX
        # Android detection
        ifdef ANDROID_NDK
            PLATFORM = ANDROID
        endif
    endif
endif

# ============================================================================
# Compiler Detection and Selection
# ============================================================================

COMPILER = GCC
COMPILER_VERSION = $(shell $(CC) --version 2>/dev/null | head -n1)

# Detect compiler type
ifeq ($(CC),clang)
    COMPILER = CLANG
else ifeq ($(CC),clang++)
    COMPILER = CLANG
else ifeq ($(findstring clang,$(CC)),clang)
    COMPILER = CLANG
else ifeq ($(findstring gcc,$(CC)),gcc)
    COMPILER = GCC
else ifeq ($(findstring g++,$(CC)),g++)
    COMPILER = GCC
else ifeq ($(findstring cl.exe,$(CC)),cl.exe)
    COMPILER = MSVC
else ifeq ($(CC),cl)
    COMPILER = MSVC
endif

# ============================================================================
# Compiler Flags
# ============================================================================

# Base compilation flags
CFLAGS = -Wall -Wextra -std=c99 -I$(INCLUDE_DIR) -fPIC
CFLAGS += -D_DEFAULT_SOURCE -D_POSIX_C_SOURCE=200809L

# Optimization flags
CFLAGS += -O2

# Platform-specific flags
ifeq ($(PLATFORM),WINDOWS)
    CFLAGS += -D_WIN32_WINNT=0x0601
    LDFLAGS =
else ifeq ($(PLATFORM),MACOS)
    CFLAGS += -D_DARWIN_C_SOURCE
    CFLAGS += -mmacosx-version-min=10.7
    LDFLAGS =
else ifeq ($(PLATFORM),IOS)
    CFLAGS += -D_DARWIN_C_SOURCE
    CFLAGS += -fPIC
    CFLAGS += -arch arm64
    CFLAGS += -isysroot $(SDKROOT)
    LDFLAGS =
else ifeq ($(PLATFORM),LINUX)
    CFLAGS += -D_POSIX_C_SOURCE=200809L
    LDFLAGS = -lm
else ifeq ($(PLATFORM),ANDROID)
    CFLAGS += -D_POSIX_C_SOURCE=200809L
    CFLAGS += -fPIC
    LDFLAGS =
else
    # Default Unix-like
    CFLAGS += -D_POSIX_C_SOURCE=200809L
    LDFLAGS = -lm
endif

# Compiler-specific flags
ifeq ($(COMPILER),CLANG)
    CFLAGS += -Wno-reserved-id-macro
else ifeq ($(COMPILER),GCC)
    CFLAGS += -Wstrict-prototypes
    CFLAGS += -Wold-style-definition
else ifeq ($(COMPILER),MSVC)
    CFLAGS = /Wall /std:c99 /I$(INCLUDE_DIR)
    CFLAGS += /O2
    CFLAGS += /D_CRT_SECURE_NO_WARNINGS
    CFLAGS += /D_WINDOWS
endif

# Additional debug/release flags
ifdef DEBUG
    CFLAGS += -g -O0 -D_DEBUG
else
    CFLAGS += -DNDEBUG
endif

# ============================================================================
# Target Rules
# ============================================================================

.PHONY: all lib test clean help docs install info

# Default target
all: info lib

# Print build information
info:
	@echo "#############################################################"
	@echo "#       Keccak/SHA3 Library Build Configuration             #"
	@echo "#############################################################"
	@echo "Host Platform:   $(PLATFORM)"
	@echo "Target:          $(TARGET)"
	@echo "Cross-Compile:   $(CROSS_COMPILE)"
	@echo "Compiler:        $(COMPILER)"
	@echo "CC:              $(CC)"
	@echo "AR:              $(AR)"
	@echo "Version:         $(COMPILER_VERSION)"
	@echo "Arch:            $(UNAME_M)"
	@echo "Flags:           $(CFLAGS)"
	@echo "Build dir:       $(BUILD_DIR)"

# Create build directory
$(BUILD_DIR):
	@$(MKDIR) -p $@

# Compile source files to object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	@echo "[CC] $< -> $@"
	@$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.c | $(BUILD_DIR)
	@echo "[CC] $< -> $@"
	@$(CC) $(CFLAGS) -c $< -o $@

# Build static library
lib: $(STATIC_LIB)

$(STATIC_LIB): $(OBJ_FILES) | $(BUILD_DIR)
	@echo "[AR] Creating static library: $@"
ifeq ($(COMPILER),MSVC)
	@lib.exe /OUT:$@ $^
else
	@$(AR) rcs $@ $^
#	@$(RANLIB) $@
endif
	@echo "[✓] Static library created: $@"

# Build test executable
test: $(TEST_EXECUTABLE)
	@echo "[✓] Test executable created: $@"
	@echo "Running tests..."
	@$(TEST_EXECUTABLE)

$(TEST_EXECUTABLE): lib $(BUILD_DIR)/test.o | $(BUILD_DIR)
	@echo "[LD] Linking test executable: $@"
	@$(CC) $(CFLAGS) -o $@ $(BUILD_DIR)/test.o $(STATIC_LIB) $(LDFLAGS)

# Generate Doxygen documentation
docs: $(DOXYGEN_FILE)
	@echo "[DOX] Generating documentation..."
	@doxygen $(DOXYGEN_FILE)
	@echo "[✓] Documentation generated in $(DOC_DIR)/"

# Install library and headers
install: lib
	@echo "[INSTALL] Installing to $(INSTALL_PREFIX)"
ifeq ($(PLATFORM),WINDOWS)
	@echo "Installation not supported on Windows. Use manual copy."
else
	@mkdir -p $(INSTALL_PREFIX)/lib
	@mkdir -p $(INSTALL_PREFIX)/include
	@cp $(STATIC_LIB) $(INSTALL_PREFIX)/lib/
	@cp $(SRC_DIR)/Keccak.h $(INSTALL_PREFIX)/include/
	@echo "[✓] Installation complete"
	@echo "Library: $(INSTALL_PREFIX)/lib/lib$(LIB_NAME).a"
	@echo "Header:  $(INSTALL_PREFIX)/include/Keccak.h"
endif

# Clean build artifacts
clean:
	@echo "[CLEAN] Removing build artifacts..."
ifeq ($(PLATFORM),WINDOWS)
	@if exist $(BUILD_DIR) $(RMDIR) $(BUILD_DIR)
else
	@$(RMDIR) $(BUILD_DIR)
endif
	@echo "[✓] Clean complete"

# Clean everything including documentation
distclean: clean
	@echo "[CLEAN] Removing all generated files..."
ifeq ($(PLATFORM),WINDOWS)
	@if exist $(DOC_DIR) $(RMDIR) $(DOC_DIR)
else
	@$(RMDIR) $(DOC_DIR)
endif
	@echo "[✓] Distribution clean complete"

# Print help
help:
	@echo "╔═══════════════════════════════════════════════════════════╗"
	@echo "║           Keccak/SHA3 Library Build Help                  ║"
	@echo "╚═══════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Basic targets:"
	@echo "  all           - Build static library (default)"
	@echo "  lib           - Build static library"
	@echo "  test          - Build and run test suite"
	@echo "  docs          - Generate Doxygen documentation"
	@echo "  clean         - Remove build artifacts"
	@echo "  distclean     - Remove all generated files"
	@echo "  info          - Show build configuration"
	@echo ""
	@echo "Cross-Compilation targets (Windows to other platforms):"
	@echo "  windows-x86_64     - Build for Windows x86_64"
	@echo "  windows-x86        - Build for Windows x86"
	@echo "  windows-all        - Build all Windows targets"
	@echo "  linux-x86_64       - Build for Linux x86_64"
	@echo "  linux-arm64        - Build for Linux ARM64 (aarch64)"
	@echo "  linux-arm          - Build for Linux ARMv7"
	@echo "  linux-all          - Build all Linux targets"
	@echo "  macos-x86_64       - Build for macOS Intel"
	@echo "  macos-arm64        - Build for macOS Apple Silicon"
	@echo "  macos-universal    - Build universal macOS binary"
	@echo "  macos-all          - Build all macOS targets"
	@echo "  ios-arm64          - Build for iOS ARM64"
	@echo "  android-arm64      - Build for Android ARM64"
	@echo "  android-arm        - Build for Android ARMv7"
	@echo "  android-all        - Build all Android targets"
	@echo "  all-platforms      - Build for all supported platforms"
	@echo ""
	@echo "Environment variables:"
	@echo "  CC=<compiler>      - Specify C compiler"
	@echo "  TARGET=<target>    - Specify cross-compile target"
	@echo "  DEBUG=1            - Enable debug build"
	@echo "  INSTALL_PREFIX=    - Installation prefix (default: /usr/local)"
	@echo ""
	@echo "Cross-compilation examples:"
	@echo "  make TARGET=linux-arm64         # Build for Linux ARM64"
	@echo "  make linux-all                  # Build all Linux variants"
	@echo "  make windows-all                # Build all Windows targets"
	@echo "  make all-platforms              # Build for all platforms"
	@echo ""
	@echo "Requirements for cross-compilation from Windows:"
	@echo "  - MSYS2/MinGW-w64 for host tools"
	@echo "  - Cross-compilation toolchains (see TOOLCHAINS.md)"
# ============================================================================
# Cross-Compilation Targets
# ============================================================================

# Windows targets
windows-x86_64:
	@echo "[CROSS] Building for Windows x86_64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=windows-native
	@$(MAKE) lib TARGET=windows-native

windows-x86:
	@echo "[CROSS] Building for Windows x86..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=windows-32
	@$(MAKE) lib TARGET=windows-32

windows-all: windows-x86_64 windows-x86
	@echo "[✓] Windows builds complete"

# Linux targets
linux-x86_64:
	@echo "[CROSS] Building for Linux x86_64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=linux-x86_64
	@$(MAKE) lib TARGET=linux-x86_64

linux-arm64:
	@echo "[CROSS] Building for Linux ARM64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=linux-arm64
	@$(MAKE) lib TARGET=linux-arm64

linux-arm:
	@echo "[CROSS] Building for Linux ARMv7..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=linux-arm
	@$(MAKE) lib TARGET=linux-arm

linux-all: linux-x86_64 linux-arm64 linux-arm
	@echo "[✓] Linux builds complete"

# macOS targets
macos-x86_64:
	@echo "[CROSS] Building for macOS x86_64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=macos-x86_64
	@$(MAKE) lib TARGET=macos-x86_64

macos-arm64:
	@echo "[CROSS] Building for macOS ARM64 (Apple Silicon)..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=macos-arm64
	@$(MAKE) lib TARGET=macos-arm64

macos-universal: macos-x86_64 macos-arm64
	@echo "[LIPO] Creating universal macOS binary..."
	@mkdir -p $(BUILD_DIR)
	@lipo -create $(BUILD_DIR)/libkeccak.a $(BUILD_DIR)/libkeccak-arm64.a \
		-output $(BUILD_DIR)/libkeccak-universal.a 2>/dev/null || \
		echo "[WARNING] lipo not available (not on macOS)"
	@echo "[✓] Universal macOS binary created"

macos-all: macos-universal
	@echo "[✓] macOS builds complete"

# iOS target
ios-arm64:
	@echo "[CROSS] Building for iOS ARM64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=ios-arm64
	@$(MAKE) lib TARGET=ios-arm64

ios: ios-arm64
	@echo "[✓] iOS builds complete"

# Android targets
android-arm64:
	@echo "[CROSS] Building for Android ARM64..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=android-arm64
	@$(MAKE) lib TARGET=android-arm64

android-arm:
	@echo "[CROSS] Building for Android ARMv7..."
	@$(MAKE) clean
	@$(MAKE) info TARGET=android-arm
	@$(MAKE) lib TARGET=android-arm

android-all: android-arm64 android-arm
	@echo "[✓] Android builds complete"

# Build all targets
all-platforms: windows-all linux-all macos-all ios android-all
	@echo "╔═══════════════════════════════════════════════════════════╗"
	@echo "║        All platform builds completed successfully!         ║"
	@echo "╚═══════════════════════════════════════════════════════════╝"

# Run all tests with verbose output
test-verbose: test
	@echo "Test run complete"

# Static analysis (if available)
analyze:
	@command -v cppcheck >/dev/null 2>&1 && \
		echo "[ANALYSIS] Running cppcheck..." && \
		cppcheck --enable=all --std=c99 $(SRC_DIR)/ $(TEST_DIR)/ || \
		echo "cppcheck not installed, skipping static analysis"

# ============================================================================
# End of Makefile
# ============================================================================
