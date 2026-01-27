# Keccak 库交叉编译指南

## 概述

本文档说明如何在 Windows 环境下为多个平台进行交叉编译。

## 支持的平台

### 主机平台（Host）
- Windows 10/11 with MinGW-w64 或 MSYS2

### 目标平台（Target）
- Windows (x86, x86_64)
- Linux (x86_64, ARM64, ARMv7)
- macOS (Intel, Apple Silicon)
- iOS (ARM64)
- Android (ARM64, ARMv7)

## 环境设置

### 方案 1：MSYS2 + MinGW-w64（推荐）

#### 1.1 安装 MSYS2

1. 从 https://www.msys2.org/ 下载 MSYS2 安装程序
2. 运行安装程序，选择安装路径（如 `C:\msys64`）
3. 完成安装

#### 1.2 更新 MSYS2

打开 MSYS2 终端（使用 `MSYS2 MinGW 64-bit` 快捷方式）：

```bash
pacman -Syuu
pacman -Su
```

按需求重启 MSYS2。

#### 1.3 安装交叉编译工具链

```bash
# 基本工具
pacman -S base-devel git

# 原生 Windows MinGW 工具链（x86_64）
pacman -S mingw-w64-x86_64-toolchain

# Windows i686 工具链（32-bit）
pacman -S mingw-w64-i686-toolchain

# Linux x86_64 交叉编译工具链
pacman -S mingw-w64-x86_64-gcc-linux-gnu

# Linux ARM64 交叉编译工具链
pacman -S mingw-w64-aarch64-linux-gnu

# Linux ARM 交叉编译工具链
pacman -S mingw-w64-arm-linux-gnueabihf

# macOS 交叉编译工具链（可选，更复杂）
# pacman -S mingw-w64-x86_64-apple-darwin
# pacman -S mingw-w64-aarch64-apple-darwin

# Android NDK（可选）
# 从官方网站下载 Android NDK
```

### 方案 2：WSL2（Windows Subsystem for Linux）

#### 2.1 安装 WSL2

```powershell
# 在 PowerShell (管理员) 中执行
wsl --install
wsl --install -d Ubuntu
```

#### 2.2 在 WSL2 中安装工具链

```bash
# 在 WSL2 Ubuntu 终端中执行
sudo apt update
sudo apt upgrade -y

# 原生工具
sudo apt install -y build-essential git

# Linux ARM64
sudo apt install -y gcc-aarch64-linux-gnu g++-aarch64-linux-gnu

# Linux ARM
sudo apt install -y gcc-arm-linux-gnueabihf g++-arm-linux-gnueabihf

# macOS（通过 osxcross）
# 更复杂的设置，参考 osxcross 文档

# Android NDK
sudo apt install -y android-ndk
```

## 构建说明

### 原生编译（Windows）

```bash
# 默认编译（Windows x86_64）
make

# 显式指定目标
make TARGET=windows-native

# 编译测试
make test

# 编译所有 Windows 变体
make windows-all
```

### Linux 交叉编译

```bash
# Linux x86_64
make TARGET=linux-x86_64

# Linux ARM64
make TARGET=linux-arm64

# Linux ARMv7
make TARGET=linux-arm

# 编译所有 Linux 变体
make linux-all
```

### macOS 交叉编译

```bash
# macOS x86_64（Intel）
make TARGET=macos-x86_64

# macOS ARM64（Apple Silicon）
make TARGET=macos-arm64

# 创建通用二进制（需要在 macOS 上）
make macos-universal

# 编译所有 macOS 变体
make macos-all
```

### iOS 交叉编译

```bash
# iOS ARM64
make TARGET=ios-arm64
```

### Android 交叉编译

```bash
# Android ARM64
make TARGET=android-arm64

# Android ARMv7
make TARGET=android-arm

# 编译所有 Android 变体
make android-all
```

### 编译所有平台

```bash
# 编译所有支持的平台
make all-platforms
```

## 构建输出

编译后的静态库将位于 `build/` 目录：

```
build/
├── libkeccak.a          # 原生编译结果
├── test_keccak          # 测试可执行文件
└── (其他目标文件)
```

## 调试选项

### 启用调试符号

```bash
make DEBUG=1 TARGET=linux-arm64
```

### 显示编译信息

```bash
make info TARGET=linux-x86_64
```

## 验证工具链

运行提供的工具链检查脚本：

```bash
./check-toolchains.bat
```

此脚本将显示系统中可用的所有交叉编译工具链。

## 常见问题

### Q1: "Command not found: aarch64-linux-gnu-gcc"

**原因**：交叉编译工具链未安装

**解决**：
- MSYS2：`pacman -S mingw-w64-aarch64-linux-gnu`
- WSL2：`sudo apt install gcc-aarch64-linux-gnu`

### Q2: "conflicting architectures: i386 and x86_64"

**原因**：尝试在 32-bit 系统上编译 64-bit 代码

**解决**：使用 `TARGET=windows-32` 或 `TARGET=windows-native`

### Q3: macOS 交叉编译不工作

**原因**：macOS 交叉编译工具链需要特殊配置

**解决**：
- 最简单：在 macOS 上原生编译
- 替代：使用 osxcross 工具链（更复杂）

### Q4: 如何为 Android 编译？

**解决**：
1. 安装 Android NDK
2. 配置 NDK 路径到 PATH
3. 运行 `make android-arm64`

## 性能提示

### 并行编译

```bash
make -j4 all-platforms
```

使用 `-j` 选项并行编译，数字表示并行任务数（推荐为 CPU 核心数）。

### 增量编译

```bash
make TARGET=linux-arm64      # 仅编译此目标
```

不使用 `clean` 时，只编译修改过的文件。

## 许可证

见 [LICENSE](../LICENSE) 文件

## 支持

有问题或需要帮助？提交 issue 或查看 [README.md](../README.md)
