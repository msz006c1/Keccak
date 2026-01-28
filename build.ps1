# Keccak/SHA3 Library Build Script (PowerShell)
# Cross-Compilation Support for Windows, Linux, macOS, iOS, Android

param(
    [ValidateSet("Release", "Debug")]
    [string]$BuildType = "Release",
    
    [ValidateSet(
        "windows-native", "windows-32",
        "linux-x86_64", "linux-arm64", "linux-arm",
        "macos-x86_64", "macos-arm64", "ios-arm64",
        "android-arm64", "android-arm",
        "windows-all", "linux-all", "macos-all", "android-all",
        "all-platforms"
    )]
    [string]$Target = "windows-native",
    
    [switch]$Test,
    [switch]$Docs,
    [switch]$CheckToolchains,
    [switch]$Help,
    [int]$Jobs = 4
)

# Display help
function Show-Help {
    Write-Host @"
╔═══════════════════════════════════════════════════════════╗
║   Keccak/SHA3 Library Build Script (PowerShell)           ║
║   Cross-Compilation Support Enabled                       ║
╚═══════════════════════════════════════════════════════════╝

Usage: .\build.ps1 [Options]

Options:
  -BuildType <Type>      Build type: Release (default) or Debug
  -Target <Target>       Target platform to build
  -Test                  Build and run tests
  -Docs                  Generate Doxygen documentation
  -CheckToolchains       Check available cross-compilation toolchains
  -Jobs <Number>         Number of parallel build jobs (default: 4)
  -Help                  Show this help message

Build Targets:
  Windows:
    windows-native       Windows x86_64 (default)
    windows-32           Windows x86
    windows-all          All Windows targets

  Linux:
    linux-x86_64         Linux x86_64
    linux-arm64          Linux ARM64
    linux-arm            Linux ARMv7
    linux-all            All Linux targets

  macOS:
    macos-x86_64         macOS Intel
    macos-arm64          macOS Apple Silicon
    macos-all            All macOS targets

  Mobile:
    ios-arm64            iOS ARM64
    android-arm64        Android ARM64
    android-arm          Android ARMv7
    android-all          All Android targets

  All Platforms:
    all-platforms        Build for all supported platforms

Examples:
  .\build.ps1                                    # Default Windows build
  .\build.ps1 -Target linux-arm64                # Cross-compile for Linux ARM64
  .\build.ps1 -Target linux-all                  # Build all Linux targets
  .\build.ps1 -Target all-platforms              # Build all platforms
  .\build.ps1 -Target windows-native -Test       # Build and run tests
  .\build.ps1 -BuildType Debug                   # Debug build
  .\build.ps1 -CheckToolchains                   # Check toolchains
  .\build.ps1 -Help                              # Show this help

"@
    exit 0
}

# Show help if requested
if ($Help) {
    Show-Help
}

# Detect platform
$OSInfo = [System.Environment]::OSVersion
if ($OSInfo.Platform -eq "Win32NT") {
    $HostPlatform = "Windows"
} elseif ($PSVersionTable.OS -like "*Linux*") {
    $HostPlatform = "Linux"
} elseif ($PSVersionTable.OS -like "*Darwin*" -or $IsMacOS) {
    $HostPlatform = "macOS"
} else {
    $HostPlatform = "Unknown"
}

# Display header
Write-Host ""
Write-Host "╔═══════════════════════════════════════════════════════════╗"
Write-Host "║   Keccak/SHA3 Library Build Script (PowerShell)           ║"
Write-Host "║   Cross-Compilation Support Enabled                       ║"
Write-Host "╚═══════════════════════════════════════════════════════════╝"
Write-Host ""
Write-Host "Host Platform: $HostPlatform"
Write-Host ""

# Check toolchains if requested
if ($CheckToolchains) {
    Write-Host "Checking for available cross-compilation toolchains..."
    Write-Host ""
    
    $toolchains = @{
        "gcc" = "GCC (native)"
        "x86_64-linux-gnu-gcc" = "Linux x86_64"
        "aarch64-linux-gnu-gcc" = "Linux ARM64"
        "arm-linux-gnueabihf-gcc" = "Linux ARMv7"
        "x86_64-apple-darwin-clang" = "macOS Intel"
        "aarch64-apple-darwin-clang" = "macOS ARM64"
        "arm64-apple-ios-clang" = "iOS ARM64"
        "aarch64-linux-android-clang" = "Android ARM64"
        "armv7a-linux-android-clang" = "Android ARMv7"
    }
    
    $foundAny = $false
    foreach ($tool_cmd in $toolchains.Keys) {
        $tool_name = $toolchains[$tool_cmd]
        $toolExists = $null -ne (Get-Command $tool_cmd -ErrorAction SilentlyContinue)
        if ($toolExists) {
            $foundAny = $true
            $toolPath = (Get-Command $tool_cmd -ErrorAction SilentlyContinue).Source
            Write-Host "[✓] $tool_name`: $toolPath"
        } else {
            Write-Host "[✗] $tool_name`: NOT FOUND"
        }
    }
    
    Write-Host ""
    if ($foundAny) {
        Write-Host "Some toolchains found. Installation instructions:"
        Write-Host "  Windows (MSYS2): pacman -S mingw-w64-toolchain"
        Write-Host "  macOS:           brew install mingw-w64"
        Write-Host "  Linux:           sudo apt install gcc-arm-linux-gnueabihf ..."
    } else {
        Write-Host "No toolchains found. Installation required."
    }
    Write-Host ""
    exit 0
}

# Show build configuration
Write-Host "Build Configuration:"
Write-Host "  Type:       $BuildType"
Write-Host "  Target:     $Target"
Write-Host "  Test:       $Test"
Write-Host "  Docs:       $Docs"
Write-Host "  Jobs:       $Jobs"
Write-Host ""

# Check if make is available
$makeExists = $null -ne (Get-Command mingw32-make -ErrorAction SilentlyContinue)
if (-not $makeExists) {
    $makeExists = $null -ne (Get-Command make -ErrorAction SilentlyContinue)
}

if (-not $makeExists) {
    Write-Host "[ERROR] make/mingw32-make not found in PATH"
    Write-Host "Install MSYS2 with MinGW-w64 toolchain"
    exit 1
}

# Determine make command
if ($null -ne (Get-Command mingw32-make -ErrorAction SilentlyContinue)) {
    $makeCmd = "mingw32-make"
} else {
    $makeCmd = "make"
}

# Build
Write-Host "Building library ($BuildType)..."
Write-Host ""

$makeArgs = @("-j$Jobs")
if ($BuildType -eq "Debug") {
    $makeArgs += "DEBUG=1"
}

if ($Target -ne "windows-native") {
    $makeArgs += "TARGET=$Target"
}

$makeArgs += $Target

$output = & $makeCmd $makeArgs 2>&1
Write-Host $output

if ($LASTEXITCODE -ne 0) {
    Write-Host "[ERROR] Build failed!"
    exit 1
}

Write-Host "Build complete!"
Write-Host ""

# Run tests if requested
if ($Test) {
    if ($Target -eq "windows-native" -or $Target -eq "native") {
        Write-Host "Building and running tests..."
        & $makeCmd test
        Write-Host ""
    } else {
        Write-Host "Note: Tests can only run with native target"
        Write-Host ""
    }
}

# Generate documentation if requested
if ($Docs) {
    Write-Host "Generating documentation..."
    $doxygenExists = $null -ne (Get-Command doxygen -ErrorAction SilentlyContinue)
    if ($doxygenExists) {
        & $makeCmd docs
        Write-Host "Documentation generated in docs\html\"
    } else {
        Write-Host "Doxygen not found. Skipping documentation generation."
    }
    Write-Host ""
}

# Show results
Write-Host "╔═══════════════════════════════════════════════════════════╗"
Write-Host "║                    Build Complete!                        ║"
Write-Host "╚═══════════════════════════════════════════════════════════╝"
Write-Host ""

# List output files
if (Test-Path "build") {
    Write-Host "Output files in: build/"
    Get-ChildItem "build" -File | Where-Object { $_.Extension -match '\.(a|lib|exe)$' } | Format-Table Name, Length
    Write-Host ""
}
