---
title: "建構調試版本"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
本指南介紹了使用 `build/lumi/scripts` 中的腳本進行 Lumi 的**本地偵錯工作流程**。

此工作流程旨在：

- 使用本地建置工件（無需下載符號），
- 驗證調試符號是否確實存在，
- 預設以離線符號模式啟動 GDB。

## 先決條件

- 基於 Debian 的 Linux（專案基線：Debian 13）
- Lumi 來源樹已克隆

## 一次性 GDB 設定（可選但建議）

安裝GDB工具：

```bash
sudo apt update
sudo apt install gdb gdbserver
```

可選的本機日誌記錄設定：

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注意：Lumi 的本地偵錯腳本預設為禁用`debuginfod`，以保持符號解析本地且可重現。

## 快速入門

從腳本目錄：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### 調試建置 + 啟動（預設）

將其用於正常的調試會話。

```bash
bash lumi-debug-local.sh lumi-dev build
```

這個命令：

1.在調試模式下建構Lumi，
2. 驗證調試符號，
3.在GDB下啟動Lumi。

### 僅調試建置（用於以後的 TTY/遠端會話）

當您想立即建置並稍後啟動/偵錯時，請使用此選項。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## 在 Linux 中使用 TTY

TTY（文字控制台）通常是調試硬凍結的最可靠方法。

- 透過`Ctrl + Alt + F6` 使用`Ctrl + Alt + F1` 切換到TTY
- 從文字提示登入
- 使用`Ctrl + Alt + F7`（或某些系統上的`F2`）返回圖形會話

為什麼這很重要：如果桌面會話停止，TTY 通常仍會回應，因此您可以附加 GDB、捕獲回溯並恢復有用的崩潰資料。

## 可選：遠端/TTY 調試

對於硬凍結或顯示鎖定，請使用 `gdbserver`：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

然後從 TTY（建議用於凍結場景）或另一個終端：

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

對於本機 GDB 啟動（非 TTY 路徑）：

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## 效能說明

調試建置的設計速度較慢。完成調試後，切換回更快的建置：

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```