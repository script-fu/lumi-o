---
title: "インストール"
type: docs
url: "hub/technical-guides/folder/Installation"
---
このガイドでは、次の場所で現在の Lumi ビルド スクリプトを使用します。

`~/code/lumi-dev/build/lumi/scripts`

## 1) 依存関係をインストールする (初回セットアップ)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Lumi を構築する

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Lumi を起動する

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## ビルドタイプ

必要に応じて `--type` を使用します。

- `debug` – ワークフローのデバッグ
- `debugoptimized` – 開発用のバランスのとれたデフォルト
- `release` – 最速の実行時間

例:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```