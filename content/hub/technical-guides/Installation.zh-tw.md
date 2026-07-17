---
title: "安裝"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

下面的初始複製步驟需要 Git。如果尚未安裝 Git，請先安裝（Debian/Ubuntu：`sudo apt install git`），或參閱：[在 Linux 上使用 Git](/hub/technical-guides/Using-Git-on-Linux/)

## 1) 複製 Lumi（首次設定）

建立 Lumi 目錄，並用 Git 複製原始碼。

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 安裝相依項（首次設定）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) 建置 Lumi（首次設定）

首次或重大變更後的完整設定建置：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) 啟動 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 可選：重新建置 / 編譯

程式碼變更後的一般重新建置：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

僅編譯的快速路徑：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

建置單一整合元件（將 `babl` 替換為 `gegl` 或 `gtk3`）：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## 可選：建置類型

需要時使用 `--type`：

- `debug` – 除錯工作流程
- `debugoptimized` – 開發用的平衡預設值
- `release` – 最快的執行速度

範例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
