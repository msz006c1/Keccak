@echo off
REM Windows Build Script for Keccak/SHA3 Library with Cross-Compilation Support

setlocal enabledelayedexpansion

echo.
echo ╔═══════════════════════════════════════════════════════════╗
echo ║   Keccak/SHA3 Library Build Script ^(Windows^)               ║
echo ║   Cross-Compilation Support Enabled                       ║
echo ╚═══════════════════════════════════════════════════════════╝
echo.

REM Detect MSVC or MinGW
if defined VCINSTALLDIR (
    echo Compiler: Microsoft Visual C++ detected
    set COMPILER=MSVC
) else (
    echo Compiler: GCC/MinGW detected
    set COMPILER=GCC
)
echo.

REM Initialize variables
set BUILD_TYPE=Release
set TEST_BUILD=0
set DOCS_BUILD=0
set TARGET=native
set CHECK_TOOLCHAINS=0

:parse_args
if "%1"=="" goto start_build
if "%1"=="--debug" (
    set BUILD_TYPE=Debug
    shift
    goto parse_args
)
if "%1"=="--test" (
    set TEST_BUILD=1
    shift
    goto parse_args
)
if "%1"=="--docs" (
    set DOCS_BUILD=1
    shift
    goto parse_args
)
if "%1"=="--target" (
    set TARGET=%2
    shift
    shift
    goto parse_args
)
if "%1"=="--all-platforms" (
    set TARGET=all-platforms
    shift
    goto parse_args
)
if "%1"=="--check-toolchains" (
    set CHECK_TOOLCHAINS=1
    shift
    goto parse_args
)
if "%1"=="--help" (
    call :show_help
    exit /b 0
)
if "%1"=="--windows-all" (
    set TARGET=windows-all
    shift
    goto parse_args
)
if "%1"=="--linux-all" (
    set TARGET=linux-all
    shift
    goto parse_args
)
if "%1"=="--macos-all" (
    set TARGET=macos-all
    shift
    goto parse_args
)
if "%1"=="--ios" (
    set TARGET=ios
    shift
    goto parse_args
)
if "%1"=="--android-all" (
    set TARGET=android-all
    shift
    goto parse_args
)

:start_build

REM Check toolchains if requested
if %CHECK_TOOLCHAINS% equ 1 (
    echo.
    call check-toolchains.bat
    exit /b 0
)

REM Show configuration
echo Build Configuration:
echo   Type:   %BUILD_TYPE%
echo   Target: %TARGET%
echo   Test:   %TEST_BUILD%
echo   Docs:   %DOCS_BUILD%
echo.

REM Build
echo Building library ^(%BUILD_TYPE%^)...
if "%BUILD_TYPE%"=="Debug" (
    mingw32-make DEBUG=1 TARGET=%TARGET% lib
) else (
    mingw32-make TARGET=%TARGET% lib
)

if %ERRORLEVEL% neq 0 (
    echo [ERROR] Build failed!
    exit /b 1
)

echo Build complete!
echo.

REM Run tests if requested
if %TEST_BUILD% equ 1 (
    if "%TARGET%"=="native" (
        echo Building and running tests...
        mingw32-make test
        echo.
    ) else (
        echo Note: Tests can only run with TARGET=native
        echo.
    )
)

REM Generate documentation if requested
if %DOCS_BUILD% equ 1 (
    echo Generating documentation...
    where doxygen >nul 2>nul
    if %ERRORLEVEL% equ 0 (
        mingw32-make docs
        echo Documentation generated in docs\html\
    ) else (
        echo Doxygen not found. Skipping documentation generation.
    )
    echo.
)

REM Show results
echo ╔═══════════════════════════════════════════════════════════╗
echo ║                    Build Complete!                        ║
echo ╚═══════════════════════════════════════════════════════════╝
echo.
echo Output files in: build/
dir /b build\
echo.

goto end

:show_help
echo Usage: build.bat [options]
echo.
echo Build options:
echo   --debug              Build with debug symbols
echo   --test               Build and run tests
echo   --docs               Generate documentation
echo.
echo Cross-compilation targets:
echo   --target windows-native    Windows x86_64 ^(default^)
echo   --target windows-32        Windows x86
echo   --target linux-x86_64      Linux x86_64
echo   --target linux-arm64       Linux ARM64
echo   --target linux-arm         Linux ARMv7
echo   --target macos-x86_64      macOS Intel
echo   --target macos-arm64       macOS Apple Silicon
echo   --target ios-arm64         iOS ARM64
echo   --target android-arm64     Android ARM64
echo   --target android-arm       Android ARMv7
echo.
echo Platform packages:
echo   --windows-all              Build all Windows targets
echo   --linux-all                Build all Linux targets
echo   --macos-all                Build all macOS targets
echo   --ios                      Build iOS
echo   --android-all              Build all Android targets
echo   --all-platforms            Build for all platforms
echo.
echo Utilities:
echo   --check-toolchains         Check available toolchains
echo   --help                     Show this help message
echo.
echo Examples:
echo   build.bat                              ^(default Windows build^)
echo   build.bat --test                       ^(build and run tests^)
echo   build.bat --target linux-arm64         ^(cross-compile for Linux ARM64^)
echo   build.bat --all-platforms              ^(build all platforms^)
echo   build.bat --linux-all --test           ^(build all Linux targets^)
echo   build.bat --check-toolchains           ^(verify toolchain installation^)
echo.
exit /b 0

:end
pause
