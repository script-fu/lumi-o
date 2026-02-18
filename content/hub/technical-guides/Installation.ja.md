---
title: "インストール"
type: docs
---
以下の最初のクローン手順には Git が必要です。 Git がまだインストールされていない場合は、最初にインストールするか (Debian/Ubuntu: `sudo apt install git`)、またはフォローしてください: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi のクローン (初回セットアップ)

Lumi 用のディレクトリを作成し、Git を使用してソース コードのクローンを作成します。

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 依存関係のインストール (初回セットアップ)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi をビルドする (初回セットアップ)

最初の完全なセットアップ ビルド (初回または大きな変更後):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lumi を起動する

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## オプション: リビルド/コンパイル

コード変更後の通常のリビルド:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

コンパイル専用のクイック パス:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

単一の統合コンポーネントを構築します (`babl` を `gegl` または `gtk3` に置き換えます)。

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## オプション: ビルド タイプ

必要に応じて `--type` を使用します。

- `debug` – ワークフローのデバッグ
- `debugoptimized` – 開発用のバランスの取れたデフォルト
- `release` – 最速の実行時間

例:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```