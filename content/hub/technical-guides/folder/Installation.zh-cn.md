---
title: "安装"
type: docs
url: "hub/technical-guides/folder/Installation"
---
本指南使用当前的 Lumi 构建脚本：

`~/code/lumi-dev/build/lumi/scripts`

## 1) 安装依赖项（首次设置）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) 构建 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) 启动 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 构建类型

需要时使用`--type`：

- `debug` – 调试工作流程
- `debugoptimized` – 开发的平衡默认值
- `release` – 最快的运行时间

示例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```