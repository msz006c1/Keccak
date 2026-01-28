@echo off
REM Toolchain Detection Script for Windows
REM Detects cross-compilation toolchains available on the system

echo.
echo ╔═══════════════════════════════════════════════════════════╗
echo ║     Keccak Cross-Compilation Toolchain Checker            ║
echo ╚═══════════════════════════════════════════════════════════╝
echo.

setlocal enabledelayedexpansion

set "found_any=0"

REM Check for MinGW/GCC (Windows native)
echo [1] Checking for GCC (Windows native)...
where gcc >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] gcc found:
    gcc --version | findstr /R ".*"
    echo.
) else (
    echo   [✗] gcc not found
    echo.
)

REM Check for MinGW i686 toolchain (Windows 32-bit)
echo [2] Checking for i686-w64-mingw32 toolchain (Windows 32-bit)...
where i686-w64-mingw32-gcc >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] i686-w64-mingw32-gcc found
) else (
    echo   [✗] i686-w64-mingw32-gcc not found
    echo      Install: pacman -S mingw-w64-i686-toolchain ^(MSYS2^)
)
echo.

REM Check for Linux x86_64 toolchain
echo [3] Checking for x86_64-linux-gnu toolchain (Linux x86_64)...
where x86_64-linux-gnu-gcc >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] x86_64-linux-gnu-gcc found
) else (
    echo   [✗] x86_64-linux-gnu-gcc not found
    echo      Install: pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-binutils
)
echo.

REM Check for Linux ARM64 toolchain
echo [4] Checking for aarch64-linux-gnu toolchain (Linux ARM64)...
where aarch64-linux-gnu-gcc >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] aarch64-linux-gnu-gcc found
) else (
    echo   [✗] aarch64-linux-gnu-gcc not found
    echo      Install: pacman -S mingw-w64-cross-gcc-aarch64-linux-gnu
)
echo.

REM Check for Linux ARM toolchain
echo [5] Checking for arm-linux-gnueabihf toolchain (Linux ARMv7)...
where arm-linux-gnueabihf-gcc >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] arm-linux-gnueabihf-gcc found
) else (
    echo   [✗] arm-linux-gnueabihf-gcc not found
    echo      Install: pacman -S mingw-w64-arm-linux-gnueabihf-toolchain
)
echo.

REM Check for macOS x86_64 toolchain
echo [6] Checking for x86_64-apple-darwin toolchain (macOS Intel)...
where x86_64-apple-darwin-clang >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] x86_64-apple-darwin-clang found
) else (
    echo   [✗] x86_64-apple-darwin-clang not found
    echo      Install: pacman -S mingw-w64-x86_64-apple-darwin-toolchain
)
echo.

REM Check for macOS ARM64 toolchain
echo [7] Checking for aarch64-apple-darwin toolchain (macOS ARM64)...
where aarch64-apple-darwin-clang >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] aarch64-apple-darwin-clang found
) else (
    echo   [✗] aarch64-apple-darwin-clang not found
    echo      Install: pacman -S mingw-w64-aarch64-apple-darwin-toolchain
)
echo.

REM Check for iOS ARM64 toolchain
echo [8] Checking for arm64-apple-ios toolchain (iOS ARM64)...
where arm64-apple-ios-clang >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] arm64-apple-ios-clang found
) else (
    echo   [✗] arm64-apple-ios-clang not found
    echo      Install: pacman -S mingw-w64-arm64-apple-ios-toolchain
)
echo.

REM Check for Android toolchains
echo [9] Checking for Android ARM64 toolchain...
where aarch64-linux-android-clang >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] aarch64-linux-android-clang found
) else (
    echo   [✗] aarch64-linux-android-clang not found
    echo      Install: Android NDK (r21+)
)
echo.

echo [10] Checking for Android ARMv7 toolchain...
where armv7a-linux-android-clang >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "found_any=1"
    echo   [✓] armv7a-linux-android-clang found
) else (
    echo   [✗] armv7a-linux-android-clang not found
    echo      Install: Android NDK (r21+)
)
echo.

REM Summary
echo ╔═══════════════════════════════════════════════════════════╗
if !found_any! equ 1 (
    echo ║     Some toolchains found. Ready for cross-compilation.   ║
) else (
    echo ║     No toolchains found. Installation required.           ║
)
echo ╚═══════════════════════════════════════════════════════════╝
echo.

echo Installation Instructions:
echo.
echo On Windows, use MSYS2 with MinGW-w64:
echo   1. Download and install MSYS2 from https://www.msys2.org/
echo   2. Run MSYS2 terminal and execute:
echo      pacman -Syuu
echo      pacman -S base-devel mingw-w64-toolchain
echo      pacman -S mingw-w64-cross-toolchains
echo.
echo Or use WSL2 (Windows Subsystem for Linux):
echo   1. Install WSL2: wsl --install
echo   2. Update packages: sudo apt update ^&^& sudo apt upgrade
echo   3. Install cross-toolchains: sudo apt install gcc-arm-linux-gnueabihf ...
echo.

pause
