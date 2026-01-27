# Windows 交叉编译配置总结

## ✅ 已完成的配置

### 1. Makefile 增强
- ✅ 添加了目标平台检测和交叉编译工具链配置
- ✅ 支持 10+ 种目标平台
- ✅ 实现了分组构建目标（windows-all, linux-all, macos-all 等）
- ✅ 添加了 all-platforms 全局构建目标
- ✅ 增强的帮助信息和构建配置显示

### 2. 构建脚本
- ✅ **build.bat** - Windows 批处理脚本，支持交叉编译
- ✅ **build.ps1** - PowerShell 脚本，更现代化的界面
- ✅ **build.sh** - Bash 脚本，支持 Linux/macOS/WSL
- ✅ **check-toolchains.bat** - 工具链检查脚本

### 3. 文档
- ✅ **BUILD_GUIDE.md** - 完整构建指南
- ✅ **CROSSCOMPILE.md** - 详细交叉编译说明
- ✅ **QUICKREF.md** - 快速参考卡片

## 🎯 支持的平台和架构

### Windows 主机编译为目标
```
✓ Windows x86_64  (windows-native)
✓ Windows x86     (windows-32)
✓ Linux x86_64    (linux-x86_64)
✓ Linux ARM64     (linux-arm64)
✓ Linux ARMv7     (linux-arm)
✓ macOS x86_64    (macos-x86_64)
✓ macOS ARM64     (macos-arm64)
✓ iOS ARM64       (ios-arm64)
✓ Android ARM64   (android-arm64)
✓ Android ARMv7   (android-arm)
```

## 🚀 快速开始

### 环境准备

1. **安装 MSYS2** (如果尚未安装)
   - 下载: https://www.msys2.org/
   - 运行安装程序

2. **在 MSYS2 中安装工具链**
   ```bash
   # 更新 MSYS2
   pacman -Syuu
   
   # 安装最小化工具链（推荐起点）
   pacman -S mingw-w64-x86_64-toolchain base-devel
   
   # 安装交叉编译工具链（按需）
   pacman -S mingw-w64-aarch64-linux-gnu mingw-w64-arm-linux-gnueabihf
   ```

### 使用构建脚本

#### PowerShell (推荐)
```powershell
# 原生编译
.\build.ps1

# 交叉编译
.\build.ps1 -Target linux-arm64

# 全平台构建
.\build.ps1 -Target all-platforms

# 查看所有选项
.\build.ps1 -Help
```

#### Batch
```batch
# 原生编译
build.bat

# 交叉编译
build.bat --target linux-arm64

# 全平台构建
build.bat --all-platforms

# 查看所有选项
build.bat --help
```

#### Make (MSYS2 / MinGW)
```bash
# 原生编译
make

# 交叉编译
make TARGET=linux-arm64

# 全平台构建
make all-platforms

# 查看帮助
make help
```

## 📋 构建命令速查

### 常用命令
```bash
make lib                          # 构建静态库
make test                         # 构建并运行测试
make clean                        # 清除构建产物
make info                         # 显示配置信息
make help                         # 显示帮助

# 交叉编译
make TARGET=linux-arm64 lib       # 为 Linux ARM64 编译
make TARGET=android-all lib       # 为所有 Android 目标编译
make all-platforms                # 为所有平台编译
```

### 高级用法
```bash
# 调试构建
make DEBUG=1 TARGET=linux-arm64

# 并行编译
make -j8 all-platforms

# 增量编译
make TARGET=linux-arm64           # 只编译修改部分

# 完整清理和重建
make clean
make all-platforms
```

## 📁 文件结构说明

```
Keccak/
├── Makefile                   # 主构建配置（已增强）
├── build.bat                  # Windows 批处理脚本（已增强）
├── build.ps1                  # PowerShell 脚本（新增）
├── build.sh                   # Bash 脚本（已增强）
├── check-toolchains.bat       # 工具链检查脚本（新增）
├── BUILD_GUIDE.md             # 构建指南（新增）
├── CROSSCOMPILE.md            # 交叉编译详细说明（新增）
├── QUICKREF.md                # 快速参考（新增）
├── src/
│   ├── Keccak.c               # Keccak 实现（已优化）
│   └── Keccak.h               # Keccak 头文件（已优化）
├── tests/
│   └── test.c                 # 测试文件（已增强）
└── build/                      # 构建输出目录（自动创建）
    ├── libkeccak.a            # 静态库
    ├── Keccak.o               # 对象文件
    ├── test_keccak            # 测试可执行文件
    └── ...
```

## 🔧 工具链配置详情

### 交叉编译工具链映射

| 目标 | 工具链前缀 | 编译器 | 说明 |
|------|-----------|--------|------|
| windows-native | (无) | gcc | Windows 本地编译 |
| windows-32 | i686-w64-mingw32- | gcc | Windows 32-bit |
| linux-x86_64 | x86_64-linux-gnu- | gcc | Linux 64-bit |
| linux-arm64 | aarch64-linux-gnu- | gcc | Linux ARM64 |
| linux-arm | arm-linux-gnueabihf- | gcc | Linux ARMv7 |
| macos-x86_64 | x86_64-apple-darwin- | clang | macOS Intel |
| macos-arm64 | aarch64-apple-darwin- | clang | macOS Apple Silicon |
| ios-arm64 | arm64-apple-ios- | clang | iOS |
| android-arm64 | aarch64-linux-android- | gcc | Android 64-bit |
| android-arm | armv7a-linux-androideabi- | gcc | Android 32-bit |

### 所需工具链包

#### MSYS2 MinGW-w64 安装命令
```bash
# 最小化（仅 Windows）
pacman -S mingw-w64-x86_64-toolchain

# 标准（Windows + Linux）
pacman -S mingw-w64-x86_64-toolchain \
          mingw-w64-aarch64-linux-gnu \
          mingw-w64-arm-linux-gnueabihf \
          mingw-w64-x86_64-gcc-linux-gnu

# 完整（所有平台）
# 见 CROSSCOMPILE.md
```

## 📊 构建示例

### 示例 1: Windows 开发者快速构建
```powershell
.\build.ps1 -Test
# 输出: 原生 Windows 库 + 运行测试
```

### 示例 2: 全平台发布
```bash
make all-platforms
# 输出: 所有平台的静态库
```

### 示例 3: Linux ARM64 交叉编译
```bash
make TARGET=linux-arm64 lib
# 输出: Linux ARM64 静态库
```

### 示例 4: 调试 Android
```bash
make DEBUG=1 TARGET=android-arm64 lib
# 输出: 带调试符号的 Android ARM64 库
```

### 示例 5: 多线程全平台构建
```bash
make -j8 all-platforms
# 使用 8 个并行任务编译所有平台
```

## 🔍 诊断和验证

### 检查工具链安装

```powershell
# PowerShell
.\build.ps1 -CheckToolchains

# Batch
check-toolchains.bat

# Bash
./build.sh --check-toolchains
```

### 显示构建配置

```bash
make info                        # 默认配置
make info TARGET=linux-arm64     # Linux ARM64 配置
```

## 🛠️ 故障排除

### 常见问题

| 问题 | 解决方案 |
|------|--------|
| `gcc not found` | 安装 `mingw-w64-x86_64-toolchain` |
| `aarch64-linux-gnu-gcc not found` | 安装 `mingw-w64-aarch64-linux-gnu` |
| `make: command not found` | 安装 `mingw-w64-x86_64-make` |
| `Permission denied` on .sh | 运行 `chmod +x build.sh` |
| 编译失败：undefined reference | 检查链接标志，运行 `make clean` 后重试 |

### 获取帮助

```bash
# Makefile 帮助
make help

# 脚本帮助
./build.ps1 -Help      # PowerShell
build.bat --help       # Batch
./build.sh --help      # Bash
```

## 📚 相关文档

1. **BUILD_GUIDE.md** - 完整的构建和使用指南
2. **CROSSCOMPILE.md** - 详细的交叉编译配置说明
3. **QUICKREF.md** - 命令速查表
4. **Makefile** - 构建系统源代码

## ✨ 功能亮点

- ✅ **自动平台检测** - 自动识别主机平台
- ✅ **完整的工具链配置** - 支持 10+ 目标架构
- ✅ **多种构建接口** - Make、Batch、PowerShell、Bash
- ✅ **智能错误处理** - 友好的错误提示
- ✅ **并行编译支持** - 加速多平台构建
- ✅ **完整文档** - 快速参考和详细指南
- ✅ **工具链验证** - 自动检查依赖项

## 🎯 下一步

1. **验证环境**
   ```bash
   .\build.ps1 -CheckToolchains
   ```

2. **构建测试**
   ```bash
   .\build.ps1 -Test
   ```

3. **交叉编译**
   ```bash
   .\build.ps1 -Target linux-arm64
   ```

4. **全平台构建**
   ```bash
   make all-platforms
   ```

---

**版本**: 1.0  
**更新**: 2026-01-26  
**维护者**: Keccak Team  
**平台支持**: Windows 10/11 (MSYS2), Linux, macOS, WSL2
