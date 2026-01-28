# Keccak/SHA3 库交叉编译配置

## 🎯 概述

本项目支持在 Windows 环境下为多个平台进行交叉编译，包括：

- **Windows**: x86, x86_64
- **Linux**: x86_64, ARM64, ARMv7
- **macOS**: Intel, Apple Silicon (M1/M2)
- **iOS**: ARM64
- **Android**: ARM64, ARMv7

## 🚀 快速开始

### Windows 用户

#### 1. 安装 MSYS2（推荐）

```powershell
# 从 https://www.msys2.org/ 下载安装程序
# 运行安装程序（安装到 C:\msys64）
```

#### 2. 在 MSYS2 中安装工具链

```bash
# 打开 MSYS2 MinGW 64-bit 终端

# 更新包管理器
pacman -Syuu
pacman -Su

# 安装基本工具
pacman -S base-devel git

# 安装 Windows 原生工具链
pacman -S mingw-w64-x86_64-toolchain

# 安装交叉编译工具链（按需）
pacman -S mingw-w64-aarch64-linux-gnu      # Linux ARM64
pacman -S mingw-w64-arm-linux-gnueabihf    # Linux ARM
pacman -S mingw-w64-x86_64-gcc-linux-gnu   # Linux x86_64
```

#### 3. 构建

```bash
cd y:\Project\Keccak

# 构建原生库
mingw32-make

# 构建测试
mingw32-make test

# 交叉编译
mingw32-make TARGET=linux-arm64

# 构建所有平台
mingw32-make all-platforms
```

### 使用 PowerShell 脚本（更便捷）

```powershell
# 查看帮助
.\try_build.ps1 -Help

# 构建
.\try_build.ps1

# 交叉编译
.\try_build.ps1 -Target linux-arm64

# 构建并运行测试
.\try_build.ps1 -BuildType Release -Test

# 构建所有平台
.\try_build.ps1 -Target all-platforms

# 检查工具链
.\try_build.ps1 -CheckToolchains
```

### 使用批处理脚本

```cmd
# 查看帮助
try_build.bat --help

# 构建
try_build.bat

# 交叉编译
try_build.bat --target linux-arm64

# 构建并运行测试
try_build.bat --test

# 构建所有平台
try_build.bat --all-platforms

# 检查工具链
try_build.bat --check-toolchains
```

## 📋 支持的构建目标

### 单个目标

| 目标 | 说明 |
|------|------|
| `windows-native` | Windows x86_64（默认） |
| `windows-32` | Windows x86 |
| `linux-x86_64` | Linux x86_64 |
| `linux-arm64` | Linux ARM64（aarch64） |
| `linux-arm` | Linux ARMv7 |
| `macos-x86_64` | macOS Intel |
| `macos-arm64` | macOS Apple Silicon |
| `ios-arm64` | iOS ARM64 |
| `android-arm64` | Android ARM64 |
| `android-arm` | Android ARMv7 |

### 平台组合

```makefile
# 构建所有变体
make windows-all      # 所有 Windows 目标
make linux-all        # 所有 Linux 目标
make macos-all        # 所有 macOS 目标
make ios              # iOS
make android-all      # 所有 Android 目标
make all-platforms    # 所有平台
```

## 🔧 构建命令

### 基本命令

```bash
# 显示配置信息
make info

# 构建静态库
make lib

# 运行测试
make test

# 生成文档
make docs

# 清除构建产物
make clean

# 显示帮助
make help
```

### 交叉编译

```bash
# 指定目标平台
make TARGET=linux-arm64

# 结合其他选项
make DEBUG=1 TARGET=linux-arm64
make -j4 TARGET=all-platforms   # 并行构建
```

## 📦 输出文件

编译后的文件位于 `build/` 目录：

```
build/
├── libkeccak.a              # 静态库
├── Keccak.o                 # 目标文件
├── test_keccak              # 测试可执行文件（仅原生）
└── [其他对象文件]
```

## 🔍 验证工具链

检查系统中可用的交叉编译工具链：

```bash
# Windows 批处理
check-toolchains.bat

# PowerShell
.\try_build.ps1 -CheckToolchains

# Makefile
make info
```

## 📚 详细文档

- [CROSSCOMPILE.md](./CROSSCOMPILE.md) - 详细的交叉编译指南
- [Makefile](./Makefile) - 构建配置详解

## 🐛 故障排除

### 工具链找不到

**问题**：`aarch64-linux-gnu-gcc: command not found`

**解决**：
```bash
# MSYS2
pacman -S mingw-w64-aarch64-linux-gnu

# WSL2 Ubuntu
sudo apt install gcc-aarch64-linux-gnu
```

### make 命令找不到

**问题**：`mingw32-make: command not found`

**解决**：
```bash
# 在 MSYS2 中安装
pacman -S mingw-w64-x86_64-make
```

### 权限不足

**问题**：`Permission denied`

**解决**：
```bash
# 使 shell 脚本可执行
chmod +x build.sh
chmod +x check-toolchains.sh
```

## 💡 最佳实践

### 1. 并行构建

```bash
# 使用多个并行任务加速编译
make -j8 all-platforms
```

### 2. 增量编译

```bash
# 只编译修改过的部分
make TARGET=linux-arm64
# 下次修改后再次运行，只重编修改的文件
```

### 3. 调试构建

```bash
# 保留调试符号
make DEBUG=1 TARGET=linux-arm64
```

### 4. 清晰的输出

```bash
# 显示构建配置
make info TARGET=linux-arm64
```

## 🎯 常见工作流

### 完整多平台发布

```bash
# 清除旧构建
make clean

# 为所有平台构建
make all-platforms

# 结果在 build/ 目录中
ls -la build/
```

### 开发调试（Linux ARM64）

```bash
# 调试构建
make DEBUG=1 TARGET=linux-arm64

# 查看调试信息
make info TARGET=linux-arm64
```

### Windows 原生开发

```bash
# 快速构建和测试
make lib test

# 或使用批处理脚本
build.bat --test
```

## 📖 参考资源

- [FIPS 202 标准](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf) - Keccak 标准
- [MSYS2 官网](https://www.msys2.org/) - MinGW-w64 工具链
- [GNU 交叉编译工具链](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain) - ARM 工具链
- [Android NDK](https://developer.android.com/ndk) - Android 开发工具

## 📄 许可证

见 [LICENSE](./LICENSE) 文件

## 🤝 贡献

欢迎提出问题和建议！

## 📞 支持

有问题？
- 查看 [CROSSCOMPILE.md](./CROSSCOMPILE.md)
- 运行 `make help`
- 检查 [BUG_REPORT.md](./BUG_REPORT.md)
