---
title: "建置除錯版本"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

本指南說明如何使用 `build/lumi/scripts` 中的腳本進行 Lumi 的**本機除錯工作流程**。

此工作流程旨在：

- 使用本機建置成品（無需下載符號）
- 驗證除錯符號確實存在
- 預設以離線符號模式啟動 GDB

## 先決條件

- 基於 Debian 的 Linux（專案基線：Debian 13）
- 已複製 Lumi 原始碼樹

## 一次性 GDB 設定（可選但建議）

安裝 GDB 工具：

```bash
sudo apt update
sudo apt install gdb gdbserver
```

可選的本機日誌設定：

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注意：Lumi 的本機除錯腳本預設會停用 `debuginfod`，以保持符號解析在本機且可重現。

## 快速入門

在腳本目錄中：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 除錯建置 + 啟動（預設）

用於一般除錯工作階段。

```bash
bash lumi-debug-local.sh lumi-dev build
```

此命令會：

1. 以除錯模式建置 Lumi
2. 驗證除錯符號
3. 在 GDB 下啟動 Lumi

### 僅除錯建置（供後續 TTY/遠端工作階段使用）

適用於現在建置、稍後再啟動或除錯的情況。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## 在 Linux 中使用 TTY

TTY（文字主控台）通常是除錯硬當機最可靠的方法。

- 使用 `Ctrl + Alt + F1` 到 `Ctrl + Alt + F6` 切換到 TTY
- 在文字提示符處登入
- 使用 `Ctrl + Alt + F7`（某些系統為 `F2`）返回圖形工作階段

為什麼重要：如果桌面工作階段已當機，TTY 通常仍能回應，因此可以附加 GDB、擷取回溯並恢復有用的當機資料。

## 可選：遠端 / TTY 除錯

對於硬當機或顯示鎖死，請使用 `gdbserver`：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

然後從 TTY（凍結情境建議）或另一個終端機：

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

本機 GDB 啟動（非 TTY 路徑）：

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 效能說明

除錯建置在設計上較慢。除錯完成後，請切換回更快的建置：

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
