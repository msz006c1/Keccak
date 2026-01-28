# Keccak 库构建 - 快速参考

## 🪟 Windows (MSYS2 MinGW 环境)

```bash
# 基础命令
make                              # 构建原生库
make test                         # 构建并测试
make all-platforms                # 构建所有平台

# 单个目标
make TARGET=linux-arm64           # Linux ARM64
make TARGET=macos-arm64           # macOS Apple Silicon
make TARGET=ios-arm64             # iOS

# 平台组合
make linux-all                    # 所有 Linux 变体
make macos-all                    # 所有 macOS 变体
make android-all                  # 所有 Android 变体
make windows-all                  # 所有 Windows 变体

# 调试和优化
make DEBUG=1                      # 启用调试符号
make -j8                          # 8 线程并行编译
make clean                        # 清除构建产物
make distclean                    # 清除所有产物（包括文档）

# 信息和帮助
make info TARGET=linux-arm64      # 显示目标配置
make help                         # 显示所有选项
```

## 🐚 PowerShell (Windows)

```powershell
# 基础命令
.\build.ps1                                  # 构建
.\build.ps1 -Test                            # 构建并测试
.\build.ps1 -Target all-platforms            # 全平台构建

# 单个目标
.\build.ps1 -Target linux-arm64              # Linux ARM64
.\build.ps1 -Target ios-arm64                # iOS

# 选项组合
.\build.ps1 -BuildType Debug -Test           # 调试构建并测试
.\build.ps1 -Target all-platforms -Jobs 8    # 多线程全平台构建
.\build.ps1 -Docs                            # 生成文档

# 检查和诊断
.\build.ps1 -CheckToolchains                 # 检查可用工具链
.\build.ps1 -Help                            # 显示完整帮助
```

## 🔨 Batch (Windows cmd)

```cmd
# 基础命令
build.bat                              # 构建
build.bat --test                       # 构建并测试
build.bat --all-platforms              # 全平台构建

# 单个目标
build.bat --target linux-arm64         # Linux ARM64
build.bat --target macos-universal     # macOS 通用二进制

# 选项组合
build.bat --debug --test               # 调试模式并测试
build.bat --all-platforms --docs       # 全平台构建及文档

# 诊断
build.bat --check-toolchains           # 检查工具链
build.bat --help                       # 显示完整帮助
```

## 🐧 Bash (Linux/macOS/WSL)

```bash
# 基础命令
./try_build.sh                             # 构建
./try_build.sh --test                      # 构建并测试
./try_build.sh --all-platforms             # 全平台构建

# 单个目标
./try_build.sh --target linux-arm64        # Linux ARM64
./try_build.sh --target macos-x86_64       # macOS Intel

# 选项组合
./try_build.sh --debug --test              # 调试并测试
./try_build.sh --all-platforms --jobs 8    # 多线程全平台构建
./try_build.sh --docs                      # 生成文档

# 诊断
./try_build.sh --check-toolchains          # 检查工具链
./try_build.sh --help                      # 显示完整帮助
```

## 📦 目标平台代码速查表

| 简名 | 全名 | 用途 |
|------|------|------|
| `windows-native` | Windows x86_64 | 本地 Windows |
| `windows-32` | Windows x86 | 32-bit Windows |
| `linux-x86_64` | Linux x86_64 | 标准 Linux |
| `linux-arm64` | Linux ARM64 | 树莓派 64-bit，服务器 ARM |
| `linux-arm` | Linux ARMv7 | 树莓派 32-bit，嵌入式 Linux |
| `macos-x86_64` | macOS Intel | Intel Mac |
| `macos-arm64` | macOS Apple Silicon | M1/M2 Mac |
| `ios-arm64` | iOS ARM64 | iPhone/iPad |
| `android-arm64` | Android ARM64 | 现代安卓手机 |
| `android-arm` | Android ARMv7 | 旧安卓手机 |

## 🔧 工具链要求

### 最小安装（仅 Windows 编译）
```bash
pacman -S mingw-w64-x86_64-toolchain
```

### 标准安装（Windows + Linux 编译）
```bash
pacman -S mingw-w64-x86_64-toolchain
pacman -S mingw-w64-aarch64-linux-gnu
pacman -S mingw-w64-arm-linux-gnueabihf
pacman -S mingw-w64-x86_64-gcc-linux-gnu
```

### 完整安装（所有平台）
```bash
# 在 MSYS2 中运行
pacman -S mingw-w64-{x86_64,i686}-toolchain
pacman -S mingw-w64-{aarch64,arm}-linux-gnu
pacman -S mingw-w64-x86_64-{apple-darwin,apple-darwin-clang}
pacman -S android-ndk
```

## 💾 输出文件位置

```
build/
├── libkeccak.a           # 主库（所有目标通用名）
├── Keccak.o              # 对象文件
├── test_keccak           # 测试程序（仅原生编译）
├── test.o                # 测试对象文件
└── ...
```

## 🆘 常见问题快速修复

| 问题 | 原因 | 解决方案 |
|------|------|--------|
| `gcc: command not found` | 编译器未安装 | `pacman -S mingw-w64-x86_64-toolchain` |
| `aarch64-linux-gnu-gcc: not found` | 工具链缺失 | `pacman -S mingw-w64-aarch64-linux-gnu` |
| `make: command not found` | make 未安装 | `pacman -S mingw-w64-x86_64-make` |
| `Permission denied` | 脚本无执行权限 | `chmod +x try_build.sh` (Linux/WSL) |
| `cannot find -lm` | 数学库缺失 | 检查 `-lm` 链接标志 |

## 📊 性能优化

```bash
# 快速编译：并行 8 任务
make -j8 TARGET=linux-arm64

# 完整构建：所有平台，8 并行
make -j8 all-platforms

# 增量编译：只编译更改部分
make TARGET=linux-arm64    # 再次运行只编译改动

# 查看编译详情
make info TARGET=linux-arm64
```

## 🔍 诊断命令

```bash
# 检查系统环境
make info

# 检查特定目标
make info TARGET=linux-arm64

# 验证工具链完整性
./check-toolchains.bat        # Windows
./try_build.ps1 -CheckToolchains  # PowerShell
./try_build.sh --check-toolchains # Bash/Linux
```

## 📚 详细文档

- **BUILD_GUIDE.md** - 完整构建指南
- **CROSSCOMPILE.md** - 详细交叉编译说明
- **Makefile** - 构建系统细节
- **README.md** - 项目概述

---

**更新时间**: 2026-01-26  
**支持平台**: Windows, Linux, macOS  
**构建系统**: GNU Make + MinGW-w64
