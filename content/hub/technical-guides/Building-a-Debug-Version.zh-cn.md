---
title: "构建调试版本"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

本指南介绍如何使用 `build/lumi/scripts` 中的脚本进行 Lumi 的**本地调试工作流**。

该工作流旨在：

- 使用本地构建产物（无需下载符号）
- 验证调试符号确实存在
- 默认以离线符号模式启动 GDB

## 先决条件

- 基于 Debian 的 Linux（项目基线：Debian 13）
- 已克隆 Lumi 源代码树

## 一次性 GDB 设置（可选但推荐）

安装 GDB 工具：

```bash
sudo apt update
sudo apt install gdb gdbserver
```

可选的本地日志设置：

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注意：Lumi 的本地调试脚本默认禁用 `debuginfod`，以保持符号解析在本地且可重现。

## 快速入门

在脚本目录中：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 调试构建 + 启动（默认）

用于常规调试会话。

```bash
bash lumi-debug-local.sh lumi-dev build
```

此命令会：

1. 以调试模式构建 Lumi
2. 验证调试符号
3. 在 GDB 下启动 Lumi

### 仅调试构建（供后续 TTY/远程会话使用）

适用于现在构建、稍后启动或调试的情况。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## 在 Linux 中使用 TTY

TTY（文本控制台）通常是调试硬冻结最可靠的方法。

- 使用 `Ctrl + Alt + F1` 到 `Ctrl + Alt + F6` 切换到 TTY
- 在文本提示符处登录
- 使用 `Ctrl + Alt + F7`（某些系统为 `F2`）返回图形会话

为什么重要：如果桌面会话已卡住，TTY 通常仍能响应，因此可以附加 GDB、捕获回溯并恢复有用的崩溃数据。

## 可选：远程 / TTY 调试

对于硬冻结或显示锁死，请使用 `gdbserver`：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

然后从 TTY（冻结场景推荐）或另一个终端：

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

本地 GDB 启动（非 TTY 路径）：

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 性能说明

调试构建在设计上较慢。调试完成后，请切换回更快的构建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
