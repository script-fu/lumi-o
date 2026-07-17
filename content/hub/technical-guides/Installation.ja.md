---
title: "インストール"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

以下の最初のクローン手順には Git が必要です。Git がまだインストールされていない場合は、先にインストールしてください（Debian/Ubuntu: `sudo apt install git`）。手順はこちら: [Linux で Git を使う](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi のクローン（初回セットアップ）

Lumi 用のディレクトリを作成し、Git でソースコードをクローンします。

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 依存関係のインストール（初回セットアップ）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi のビルド（初回セットアップ）

初回、または大きな変更後の最初のフルセットアップビルド:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lumi の起動

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## オプション: リビルド / コンパイル

コード変更後の通常のリビルド:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

コンパイルのみのクイックパス:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

単一の統合コンポーネントをビルド（`babl` を `gegl` または `gtk3` に置き換え）:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## オプション: ビルドタイプ

必要に応じて `--type` を使用します:

- `debug` – デバッグ向けワークフロー
- `debugoptimized` – 開発向けのバランスの取れたデフォルト
- `release` – 最速の実行時パフォーマンス

例:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
