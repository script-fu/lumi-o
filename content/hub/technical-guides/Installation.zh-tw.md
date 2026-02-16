---
title: "安裝"
type: docs
url: "hub/technical-guides/Installation"
---
本指南使用目前的 Lumi 建置腳本：

`~/code/lumi-dev/build/lumi/scripts`

## 1) 安裝依賴項（首次設定）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) 建構 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) 啟動 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 建置類型

需要時使用`--type`：

- `debug` – 偵錯工作流程
- `debugoptimized` – 開發的平衡預設值
- `release` – 最快的運行時間

範例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```