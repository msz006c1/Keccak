# Cygwin 编译指南 - Keccak/SHA3 库

## 目录
1. [简介](#简介)
2. [Cygwin 安装](#cygwin-安装)
3. [编译环境设置](#编译环境设置)
4. [本地编译](#本地编译)
5. [交叉编译](#交叉编译)
6. [故障排除](#故障排除)
7. [与其他工具链的比较](#与其他工具链的比较)

---

## 简介

本指南详细说明如何在 Windows 系统上使用 Cygwin 编译 Keccak/SHA3 密码库。

### Cygwin 的优势
- **完整的 POSIX 兼容性** - 支持完整的 Unix/Linux 开发环境
- **透明的 Windows 集成** - 文件路径自动转换，Windows 互操作性好
- **丰富的软件包生态** - 可安装数千个 Unix/Linux 工具和库
- **原生 Windows 可执行文件** - 生成的二进制文件是真正的 Windows PE 格式
- **调试支持** - 完整的 GDB 调试器支持

### 与其他工具链的比较
| 特性 | Cygwin | MinGW | MSYS2 |
|------|--------|-------|-------|
| POSIX 兼容性 | 完整 | 部分 | 好 |
| 文件路径处理 | 自动转换 | 手动 | 自动转换 |
| 交叉编译支持 | 好 | 有限 | 优秀 |
| 包管理 | 原生 setup.exe | 无 | pacman |
| 性能 | 中等 | 高 | 高 |
| 学习曲线 | 平缓 | 陡峭 | 中等 |

---

## Cygwin 安装

### 下载 Cygwin

1. **访问官网**
   ```
   https://cygwin.com/
   ```

2. **下载安装程序**
   - 选择 `setup-x86_64.exe`（64 位系统）或 `setup-x86.exe`（32 位系统）
   - 保存到某个目录（如 `C:\cygwin-setup\`）

### 安装步骤

#### 步骤 1：运行安装程序
```batch
setup-x86_64.exe
```

#### 步骤 2：选择安装模式
- 选择 **"Install from Internet"** (推荐)
- 或 **"Install from Local Directory"** (如果已下载)

#### 步骤 3：选择安装路径
- 推荐：`C:\cygwin64\`（避免中文路径和空格）
- **重要**：安装在没有空格的路径上

#### 步骤 4：选择下载镜像
建议选择镜像源：
- `http://mirrors.aliyun.com/cygwin/` （阿里云，中国推荐）
- `https://cygwin.mirror.globo.tech/` （巴西）
- `http://cygwin.mirror.constant.com/` （美国）

#### 步骤 5：选择要安装的软件包

**最小化编译环境**（推荐）：
```
搜索并选择以下包：
- gcc-core (GNU C Compiler)
- gcc-g++ (GNU C++ Compiler)
- make (GNU Make)
- binutils (Binary utilities)
- gdb (GNU Debugger)
```

**完整开发环境**：
```
上述基础包 + :
- git (版本控制)
- wget (下载工具)
- curl (数据传输)
- nano 或 vim (文本编辑器)
- openssh (SSH 支持)
- doxygen (文档生成)
- cppcheck (代码分析)
```

**搜索和选择包的步骤**：
1. 在搜索框输入包名
2. 点击"View"切换到"Category"视图
3. 在"Devel"类别下找到所需工具包
4. 点击包名前的下拉框，选择版本
5. 点击"Next"进行安装

#### 步骤 6：完成安装
- 等待所有包下载和安装完成
- 创建桌面快捷方式（可选）

### 验证安装

打开 Cygwin 终端（双击桌面图标或从开始菜单启动）：

```bash
# 检查 GCC 版本
gcc --version

# 检查 Make 版本
make --version

# 检查 GDB 版本
gdb --version

# 验证路径
which gcc
which make
which ar
```

**预期输出**：
```
gcc (GCC) 11.4.0
...

GNU Make 4.3
...

GDB (Cygwin) 12.1
...

/usr/bin/gcc
/usr/bin/make
/usr/bin/ar
```

---

## 编译环境设置

### 1. 进入项目目录

在 Cygwin 终端中：
```bash
# Cygwin 自动将 Windows 路径映射到 /cygdrive
cd /cygdrive/y/Project/Keccak
```

或者，如果您已在 Cygwin Home 中设置了链接：
```bash
cd ~/keccak  # 如果有链接的话
```

### 2. 验证编译环境

```bash
# 验证 Makefile 可读取
cat Makefile | head -20

# 检查源文件
ls -la src/

# 检查测试文件
ls -la tests/
```

### 3. 查看编译配置

```bash
# 在 Cygwin 上的默认配置
make info
```

**预期输出**：
```
Host Platform:   UNIX (或 LINUX，取决于 uname 识别)
Target:          native
Compiler:        GCC
CC:              gcc
AR:              ar
Flags:           -Wall -Wextra -std=c99 -I./src -fPIC ...
```

---

## 本地编译

### 基础编译步骤

#### 1. 清理之前的构建（如有）
```bash
make clean
```

#### 2. 编译静态库
```bash
make lib
```

**输出示例**：
```
[CC] src/Keccak.c -> build/Keccak.o
[AR] Creating static library: build/libkeccak.a
[✓] Static library created: build/libkeccak.a
```

#### 3. 验证库文件
```bash
ls -lh build/

# 检查库的内容
ar t build/libkeccak.a
```

**预期输出**：
```
-rw-r--r--  1 user group  12345 Jan 26 10:30 libkeccak.a
-rw-r--r--  1 user group  45678 Jan 26 10:30 Keccak.o

# ar 输出：
Keccak.o
```

### 编译测试套件

#### 1. 编译并运行测试
```bash
make test
```

**输出示例**：
```
[CC] tests/test.c -> build/test.o
[LD] Linking test executable: build/test_keccak
[✓] Test executable created: build/test_keccak
Running tests...

========== Keccak/SHA3 Test Suite ==========

Test 1: SHA3-256 (NIST Vector)
Input:  "Hello, World!"
Expected: 315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3
Computed: 315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3
✓ PASSED

...
(更多测试输出)

========== Test Summary ==========
Total Tests: 12
Passed: 12
Failed: 0
✓ All tests passed!
```

#### 2. 单独运行测试可执行文件
```bash
./build/test_keccak

# 或在 Windows 中
build/test_keccak.exe
```

### 生成文档

#### 1. 验证 Doxygen 安装
```bash
doxygen --version
```

#### 2. 生成文档
```bash
make docs
```

**输出示例**：
```
[DOX] Generating documentation...
Running Doxygen...
Generating API documentation with Doxygen...
...
[✓] Documentation generated in docs/
```

#### 3. 查看文档
```bash
# 打开生成的 HTML 文档
explorer docs/html/index.html

# 或在浏览器中打开
firefox docs/html/index.html
```

### 完整编译流程

一次性完成所有编译步骤：

```bash
# 方法 1：使用 make all
make clean
make all
make test
make docs

# 方法 2：分步执行（更详细的输出）
make info      # 显示配置信息
make lib       # 编译库
make test      # 编译并运行测试
make docs      # 生成文档
```

---

## 交叉编译

Cygwin 支持交叉编译到其他平台，但需要安装相应的交叉编译工具链。

### 1. 交叉编译到 Linux

#### 安装 Linux GCC 工具链

在 Cygwin 安装程序中搜索并安装：
```
- gcc-core-x86_64-w64-mingw32-native
- binutils-x86_64-w64-mingw32
- mingw64-x86_64-gcc-core
- mingw64-x86_64-binutils
```

**或使用 apt-cyg 包管理器**：
```bash
# 安装 apt-cyg（如果未安装）
lynx -source rawgit.com/transcode-open/apt-cyg/master/apt-cyg > apt-cyg
install apt-cyg /usr/local/bin

# 安装 Linux 交叉编译工具
apt-cyg install gcc-core-x86_64-linux-gnu binutils-x86_64-linux-gnu
```

#### 编译为 Linux 目标

```bash
make clean
make TARGET=linux-x86_64
```

**检查生成的二进制文件类型**：
```bash
file build/libkeccak.a
# 预期输出：ELF 64-bit LSB relocatable, x86-64, version 1 (SYSV), ...
```

### 2. 交叉编译到 ARM

#### 为 Linux ARM64 编译

```bash
# 安装 ARM64 工具链
apt-cyg install gcc-core-aarch64-linux-gnu binutils-aarch64-linux-gnu

# 编译
make clean
make TARGET=linux-arm64
```

#### 为 Linux ARMv7 编译

```bash
# 安装 ARMv7 工具链
apt-cyg install gcc-core-arm-linux-gnueabihf binutils-arm-linux-gnueabihf

# 编译
make clean
make TARGET=linux-arm
```

### 3. 交叉编译到多个目标

```bash
# 编译所有 Linux 变体
make linux-all

# 编译所有支持的平台
make all-platforms
```

---

## 故障排除

### 常见问题

#### 问题 1：`gcc: command not found`

**原因**：GCC 未安装或不在 PATH 中

**解决方案**：
```bash
# 验证 GCC 安装
cygcheck -c gcc-core

# 如果返回"OK"，则已安装
# 检查 PATH
echo $PATH

# 如果 /usr/bin 不在 PATH 中，手动添加
export PATH=/usr/bin:$PATH

# 永久添加到 ~/.bashrc
echo 'export PATH=/usr/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
```

#### 问题 2：`make: command not found`

**原因**：GNU Make 未安装

**解决方案**：
```bash
# 使用 Cygwin 安装程序重新安装 make
# 或使用 apt-cyg
apt-cyg install make

# 验证
make --version
```

#### 问题 3：`Makefile:XXX: *** missing separator. Stop.`

**原因**：Makefile 中的制表符/空格混淆（常见于 Windows 编辑）

**解决方案**：
```bash
# 在 Cygwin 中使用 dos2unix 转换
apt-cyg install dos2unix

# 转换 Makefile
dos2unix Makefile

# 或手动修复：在编辑器中启用"显示空白符"功能
nano Makefile
# 使用 Ctrl+A+H 替换行首的所有空格为制表符
```

#### 问题 4：`libkeccak.a: No such file or directory`

**原因**：编译失败，库文件未生成

**解决方案**：
```bash
# 查看详细编译输出
make clean
make lib V=1  # 详细输出模式

# 检查是否有编译错误
ls -la build/

# 检查 Keccak.o 是否生成
file build/Keccak.o
```

#### 问题 5：`Permission denied` 运行可执行文件

**原因**：Windows 文件权限问题或 Cygwin 权限设置

**解决方案**：
```bash
# 添加执行权限
chmod +x build/test_keccak

# 或在 Cygwin 中设置自动权限
mount -o noacl /cygdrive/c

# 检查当前挂载选项
mount

# 查看文件权限
ls -la build/test_keccak
```

#### 问题 6：编译过慢

**原因**：Windows 病毒扫描程序干扰，或磁盘性能不足

**解决方案**：
```bash
# 方法 1：使用并行编译
make -j4 lib  # 使用 4 个并行任务

# 方法 2：将项目移到 SSD
# 避免在网络驱动器或同步文件夹（OneDrive, Dropbox）中编译

# 方法 3：排除 Windows Defender 扫描
# 在 Windows Defender 设置中添加 C:\cygwin64\ 到排除列表
```

### 调试技巧

#### 启用详细输出

```bash
# 显示所有编译命令
make VERBOSE=1

# 或
make V=1
```

#### 生成调试符号

```bash
# 编译时包含调试信息
make DEBUG=1 lib

# 使用 GDB 调试
gdb ./build/test_keccak

# GDB 命令
(gdb) break main
(gdb) run
(gdb) step
(gdb) print variable
(gdb) continue
(gdb) quit
```

#### 检查库的符号

```bash
# 列出库中的符号
nm build/libkeccak.a

# 过滤特定符号
nm build/libkeccak.a | grep sha3

# 预期输出示例：
# 0000000000000000 T sha3_256
# 0000000000000100 T sha3_512
```

---

## 高级主题

### 1. 使用 Cygwin 的 apt-cyg 包管理器

```bash
# 安装 apt-cyg（一次性）
lynx -source rawgit.com/transcode-open/apt-cyg/master/apt-cyg > apt-cyg
install apt-cyg /usr/local/bin

# 搜索包
apt-cyg searchall gcc

# 安装包
apt-cyg install package-name

# 更新所有包
apt-cyg update
apt-cyg upgrade
```

### 2. 配置编辑器

#### 使用 Nano（推荐初学者）
```bash
apt-cyg install nano

nano Makefile
# Ctrl+O：保存，Ctrl+X：退出
```

#### 使用 Vim（高级）
```bash
apt-cyg install vim

vim Makefile
# :wq 保存并退出，:q! 不保存退出
```

#### 使用 VS Code（推荐）
```bash
# 安装 VS Code（从 Windows 中）
# 然后在 Cygwin 中设置集成终端：
# 设置 > Terminal > External:Windows Exec 为 C:\cygwin64\bin\bash.exe
# 设置 > Terminal > External:Windows Args 为 ["-i", "-l"]
```

### 3. Git 集成

```bash
# 安装 Git
apt-cyg install git

# 配置 Git
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# 克隆项目
git clone https://github.com/yourusername/keccak.git
cd keccak

# 编译
make all
```

### 4. 性能优化

#### 编译优化标志

```bash
# 在 Makefile 中修改 CFLAGS
make CFLAGS="-O3 -march=native -mtune=native" lib

# 或编辑 Makefile，修改
CFLAGS = -Wall -Wextra -std=c99 -I$(INCLUDE_DIR) -fPIC -O3 -march=native
```

#### 并行编译

```bash
# 使用所有 CPU 核心
make -j$(nproc) lib

# 或指定核心数
make -j8 lib
```

### 5. 创建 Windows 安装程序

使用 NSIS (Nullsoft Scriptable Install System)：

```bash
# 安装 NSIS
apt-cyg install nsis

# 创建安装脚本 installer.nsi
# 然后编译：
makensis installer.nsi
```

示例 `installer.nsi`：
```nsis
; Keccak Library Installer
!include "MUI2.nsh"

; Basic Settings
Name "Keccak/SHA3 Library"
OutFile "keccak-installer.exe"
InstallDir "$PROGRAMFILES\Keccak"

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

; Installation
Section "Install"
    SetOutPath "$INSTDIR\lib"
    File "build\libkeccak.a"
    
    SetOutPath "$INSTDIR\include"
    File "src\Keccak.h"
    
    SetOutPath "$INSTDIR\docs"
    File /r "docs\html\*"
SectionEnd
```

---

## 与其他工具链的比较详表

### Cygwin vs MinGW vs MSYS2

| 特性 | Cygwin | MinGW-w64 | MSYS2 |
|------|--------|-----------|-------|
| **安装** | setup.exe | 手动/包管理 | pacman |
| **POSIX 支持** | 完整 | 有限 | 好 |
| **包管理** | setup.exe + apt-cyg | 无 | pacman |
| **依赖关系** | cygwin1.dll | msvcrt.dll | msys2-runtime.dll |
| **文件大小** | ~500MB+ | ~200MB | ~150MB |
| **开发环境** | IDE 友好 | IDE 友好 | IDE 友好 |
| **交叉编译** | 优秀 | 有限 | 优秀 |
| **性能** | 中等 | 高 | 高 |
| **许可证** | GPL/LGPL | GPL/LGPL | GPL |
| **学习曲线** | 平缓 | 陡峭 | 中等 |

### 何时选择 Cygwin

✅ **选择 Cygwin 的场景**：
- 需要完整的 POSIX 兼容性
- 移植 Unix/Linux 代码到 Windows
- 需要原生的 bash、grep、sed 等工具
- 需要详细的交叉编译支持
- 喜欢熟悉的 Unix/Linux 开发环境

❌ **不选择 Cygwin 的场景**：
- 需要最高的性能（MinGW-w64 更快）
- 磁盘空间有限（MinGW 更轻量）
- 只需要编译单个项目（MSYS2 更简单）
- 需要最新的工具链版本（MSYS2 更新更快）

---

## 快速参考命令

```bash
# 进入项目目录
cd /cygdrive/y/Project/Keccak

# 显示编译配置
make info

# 编译库
make lib

# 编译并运行测试
make test

# 生成文档
make docs

# 清理构建文件
make clean

# 显示所有可用目标
make help

# 交叉编译到 Linux
make TARGET=linux-x86_64 lib

# 编译所有 Linux 变体
make linux-all

# 编译所有平台
make all-platforms

# 详细输出模式
make V=1 lib

# 并行编译（4 个任务）
make -j4 lib

# 调试编译
make DEBUG=1 lib
```

---

## 资源和链接

- **Cygwin 官网**：https://cygwin.com/
- **Cygwin 软件包列表**：https://cygwin.com/packages/
- **apt-cyg 项目**：https://github.com/transcode-open/apt-cyg
- **GNU Make 手册**：https://www.gnu.org/software/make/manual/
- **GCC 文档**：https://gcc.gnu.org/onlinedocs/
- **Cygwin 用户指南**：https://cygwin.com/cygwin-ug-net.html

---

## 反馈和支持

如在使用 Cygwin 编译本库时遇到问题，请：

1. 检查本指南的"故障排除"部分
2. 参考 Cygwin 官方文档
3. 在项目的 Issues 中报告
4. 在 Cygwin 邮件列表中寻求帮助

---

**最后更新**：2026 年 1 月 26 日
**维护者**：Keccak/SHA3 项目团队
