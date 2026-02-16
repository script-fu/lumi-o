---
title: "构建调试版本"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
本指南介绍了使用 `build/lumi/scripts` 中的脚本进行 Lumi 的**本地调试工作流程**。

该工作流程旨在：

- 使用本地构建工件（无需下载符号），
- 验证调试符号是否确实存在，
- 默认情况下以离线符号模式启动 GDB。

## 先决条件

- 基于 Debian 的 Linux（项目基线：Debian 13）
- Lumi 源树已克隆

## 一次性 GDB 设置（可选但推荐）

安装GDB工具：

```bash
sudo apt update
sudo apt install gdb gdbserver
```

可选的本地日志记录设置：

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注意：Lumi 的本地调试脚本默认禁用`debuginfod`，以保持符号解析本地且可重现。

## 快速入门

从脚本目录：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 调试构建 + 启动（默认）

将其用于正常的调试会话。

```bash
bash lumi-debug-local.sh lumi-dev build
```

这个命令：

1.在调试模式下构建Lumi，
2. 验证调试符号，
3.在GDB下启动Lumi。

### 仅调试构建（用于以后的 TTY/远程会话）

当您想立即构建并稍后启动/调试时，请使用此选项。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## 在 Linux 中使用 TTY

TTY（文本控制台）通常是调试硬冻结的最可靠方法。

- 通过`Ctrl + Alt + F6` 使用`Ctrl + Alt + F1` 切换到TTY
- 从文本提示登录
- 使用`Ctrl + Alt + F7`（或某些系统上的`F2`）返回图形会话

为什么这很重要：如果桌面会话停止，TTY 通常仍会响应，因此您可以附加 GDB、捕获回溯并恢复有用的崩溃数据。

## 可选：远程/TTY 调试

对于硬冻结或显示锁定，请使用 `gdbserver`：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

然后从 TTY（推荐用于冻结场景）或另一个终端：

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

对于本地 GDB 启动（非 TTY 路径）：

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 性能说明

调试构建的设计速度较慢。完成调试后，切换回更快的构建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```