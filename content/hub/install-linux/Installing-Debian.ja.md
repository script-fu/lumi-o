---
title: "Debian のインストール"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

このドキュメントでは、Lumi-o 開発用オペレーティングシステムとして Debian Stable をインストールする手順を説明します。同様の環境を構築する方にも参考になるでしょう。

Debian Stable を選んだ理由は、Lumi が予測可能な長期プラットフォーム上で確実にビルドできるようにするためです。GIMP の開発は Debian Testing を対象としており、Debian Stable はそれに近いベースシステムになります。

Lumi-o は Debian と Cinnamon（X11）で最も快適に動作し、その環境で開発・テストされています。Cinnamon は Windows に近い使い慣れたデスクトップ操作を提供し、X11 は Lumi 開発で最も安定した環境です。

Windows から移行する場合、大きな違いは、ソフトウェアのインストールと設定の多くがダウンロード型インストーラーではなく、パッケージマネージャーとシンプルなターミナルコマンドで行われる点です。

## このガイドの対象者

このガイドは、Lumi 開発で使っている Debian Stable の構成を記録したものです。一般的な Linux インストール入門ではありません。

特に次の方に役立ちます。

- Windows から移行し、予測可能な Linux 環境を求めるアーティスト
- ソースから Lumi をビルドする開発者
- 独自構成を設計するより、動作が確認済みの環境を再現したいユーザー

ディスクのパーティション分割と、基本的なコマンドライン操作の知識があることを前提としています。

## データのバックアップ

Debian をインストールする前に、ホームディレクトリ全体を外付けドライブにバックアップしてください。残したい追加のデータフォルダーも含めます。

注: Linux では、`~` はホームディレクトリを表します。

Git リポジトリを使っている場合は、重要な変更をリモートにプッシュしておけば、インストール後に簡単に復元できます。この手順は、すでに Git を使っている場合にのみ必要です。

## パーティションの作成

メインドライブに Debian 用の空き領域を確保します。この作業には GParted など多くのガイドやツールがあります。環境に応じて、次のような方法があります。

- デュアルブートのために既存の Windows パーティションを縮小する
- 既存の Linux パーティションを再利用する
- 新しい Linux 用パーティションと swap パーティションを用意する

パーティション作業は機種によって大きく異なるため、不明な場合は変更前に機種向けのガイドを参照してください。

## Debian インストール USB の作成

対象パーティションと swap 領域がすでに用意されている前提です。

1. 公式サイトから Debian ISO をダウンロードします: https://www.debian.org/
2. Windows では BalenaEtcher を使って ISO を USB ドライブに書き込みます。
3. Linux では `dd` などのコマンドラインツールで起動可能 USB を作成します。

## Debian のインストール

1. USB ドライブを挿入します。
2. 再起動し、起動時にブートメニューキー（通常は `F2`、`F12`、`Esc`、`Del` のいずれか）を押します。
3. USB デバイスを選択します。
4. 非グラフィカルインストーラーを選びます。
5. root パスワードは空欄のままにします。ユーザーアカウントに sudo 権限が付与されます。
6. 手動でパーティションを設定します。

   - ファイルシステム: ext4（ジャーナリング）
   - Swap: 既存の swap パーティション
   - マウントポイント: `/`
   - ラベル: `linux`
   - ホスト名: プロンプトで `user@hostname` 形式で表示される名前
   - ユーザーアカウント: 氏名
   - ユーザー名: ターミナルログイン名

7. この段階でデスクトップ環境を選べます。Lumi 推奨構成として **Cinnamon** を選択してください。
8. インストールを完了し、Debian Stable で再起動します。

## システム設定

### 表示スケーリング

Debian Stable では、特に 4K ディスプレイで小数スケーリングの扱いが不安定です。解像度を下げる代わりに、UI 要素を直接調整してください。

推奨設定:

- 小数表示スケーリングは避ける
- メニュー → フォント選択 → フォント設定 → テキスト倍率: `2.5`
- デスクトップフォント: `14`
- パネル → カスタマイズ → パネルの高さ: `60`
- パネルの外観 → 右ゾーンのシンボリックアイコンサイズ: `48px`
- マウスとタッチパッド → ポインタサイズの調整
- デスクトップ（右クリック）→ カスタマイズ → アイコンサイズを大きく

Firefox の調整:

- アドレスバー → `about:config`
- `layout.css.devPixelsPerPx` を `1` に設定

### ターミナル

ターミナルの設定:

1. メニュー → ターミナル → 編集 → 設定
2. テキスト → 初期サイズ: `140 columns`、`40 rows`
3. テキスト → カスタムフォント: `Monospace 10`
4. 色 → 組み込みスキーム → Solarized Dark

## データの復元

必要に応じて、バックアップからホームディレクトリへファイルを戻します。例:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注: `.` で始まるフォルダーは Linux の隠し設定ディレクトリです。

## 任意: Git の設定

Lumi をビルドする、またはリポジトリを復元する場合にのみ必要です。

### Git のインストール

```bash
sudo apt install git
```

ユーザー情報を設定します。

```bash
git config --global --edit
```

#### GitLab へのアクセス

GitLab または GitHub へのリポジトリアクセスを復元します。

1. SSH キーファイルの権限を変更: `chmod 600 ~/.ssh/id_rsa`
2. SSH エージェントにキーを追加: `ssh-add ~/.ssh/id_rsa`
3. 接続を確認: `ssh -T git@ssh.gitlab.gnome.org` または `ssh -T git@github.com`

各リポジトリでリモートを取得し、ローカルブランチを一致させます。

```bash
git reset --hard remote-name/branch-name
git clean -df
```

`git status` でリポジトリがクリーンであることを確認します。

これで新しい OS にデータとリポジトリが復元されました。この構成は Lumi 開発で使っている動作確認済みの環境を反映しており、必要に応じて各自のワークフローに合わせて調整できます。

## OS セットアップ後の Lumi ビルド

Lumi ビルドスクリプトは次の場所にあります。

`~/code/lumi-dev/build/lumi/scripts`。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```
