---
title: "Debian のインストール"
type: docs
url: "hub/install-linux/Installing-Debian"
---
このドキュメントでは、Debian Stable を Lumi·o 開発オペレーティング システムとしてインストールするために使用されるプロセスの概要を説明します。同様の環境を設定している他の人にとっても役立つかもしれません。

Debian Stable が選択されたのは、Lumi が予測可能な長期的なプラットフォーム上に信頼性の高い構築を目指しているためです。 GIMP の開発は Debian テストを対象としており、Debian Stable は緊密に調整された基本システムとなっています。

Windows から使用している場合、主な概念的な変更点は、ほとんどのソフトウェアのインストールと構成が、ダウンロード可能なインストーラーではなく、パッケージ マネージャーと単純なターミナル コマンドを通じて行われることです。

## このガイドの対象者

このガイドでは、Lumi 開発に使用される動作する Debian Stable セットアップについて説明します。これは一般的な Linux インストール チュートリアルではありません。

これは次の場合に最も役立ちます。

- Windows から移行し、予測可能な Linux セットアップを希望するアーティスト
- ソースから Lumi を構築する開発者
- 独自のシステム構成を設計するよりも、既知の作業環境を再現することを好むユーザー

ディスクのパーティショニングと簡単なコマンドラインの使用法に関する基本的な知識があることを前提としています。

## データをバックアップする

Debian をインストールする前に、ホーム ディレクトリの完全なバックアップを外部ドライブに作成します。保存したい追加のデータ フォルダーを含めます。

注: Linux では、`~` はホーム ディレクトリを表します。

Git リポジトリを使用する場合は、重要な変更をそのオリジンにプッシュして、インストール後に簡単に復元できるようにします。この手順は、すでに Git を使用している場合にのみ関係します。

## パーティションを作成する

プライマリ ドライブに Debian 用のスペースを作成します。このステップには、GParted を含む多くのガイドやツールが存在します。設定に応じて、次のことが可能になります。

- デュアルブート用に既存の Windows パーティションを縮小します
- 既存の Linux パーティションを再利用する
- 新しい Linux を準備し、パーティションを交換します

パーティション分割の手順はシステムによって大幅に異なるため、不明な場合は、変更を加える前にハードウェア固有のガイドを参照してください。


## Debian インストール USB を作成する

ターゲット パーティションとスワップ領域がすでに存在すると仮定します。

1. 公式 Web サイトから Debian ISO をダウンロードします: https://www.debian.org/
2. Windows では、BalenaEtcher を使用して ISO を USB ドライブに書き込みます。
3. Linux では、`dd` などのコマンドライン ツールを使用して、ブート可能な USB を作成します。

## Debian をインストールする

1. USB ドライブを挿入します。
2. 再起動し、起動中にブート メニュー キー (通常は `F2`、`F12`、`Esc`、または `Del`) を押します。
3. USB デバイスを選択します。
4. 非グラフィカルインストーラーを選択します。
5. プロンプトが表示されたら root パスワードを空白のままにして、インストーラーがユーザー アカウントに sudo アクセスを許可します。
6. 手動でパーティションを分割します。

   - ファイルシステム: ext4 (ジャーナリング)
   - スワップ: 既存のスワップ パーティション
   - マウントポイント: `/`
   - ラベル: `linux`
   - ホスト名: `user@hostname` として表示されるシステム名
   - ユーザーアカウント: あなたのフルネーム
   - ユーザー名: 端末のログイン名

7. デスクトップ環境として **Cinnamon** を選択します。
8. インストールを完了し、Debian Stable で再起動します。

## システムセットアップ

### 表示スケーリング

現在、Debian Stable は、特に 4K ディスプレイ上で、分数スケーリングの処理に一貫性がありません。ディスプレイの解像度を下げる代わりに、インターフェイス要素を直接調整します。

推奨される調整:- 分数表示のスケーリングを避けてください。
- メニュー → フォント選択 → フォント設定 → テキスト倍率: `2.5`
- デスクトップ フォント: `14`
- パネル → カスタマイズ → パネルの高さ: `60`
- パネルの外観 → 右ゾーンのシンボリック アイコン サイズ: `48px`
- マウスとタッチパッド → ポインタのサイズ調整
- デスクトップ（右クリック）→カスタマイズ→アイコンサイズを大きくする

Firefoxの調整：

- アドレスバー → `about:config`
- `layout.css.devPixelsPerPx` を `1` に設定します

### 端子

端末の設定を構成します。

1. メニュー → ターミナル → 編集 → 設定
2. テキスト→初期サイズ：`140 columns`、`40 rows`
3. テキスト → カスタム フォント: `Monospace 10`
4. 色 → 組み込みスキーム → ソラライズドダーク

## データを復元する

必要に応じて、バックアップ ファイルをホーム ディレクトリに復元します。次に例を示します。

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注: `.` で始まるフォルダーは、Linux の隠し設定ディレクトリです。

## オプション: Git セットアップ

Lumi の構築またはリポジトリの復元を計画している場合にのみ必要です。

### Git をインストールする

```bash
sudo apt install git
```

ID を設定します。

```bash
git config --global --edit
```

#### GitLab へのアクセス

GitLab または GitHub へのリポジトリ アクセスを復元します。

1. SSH キー ファイルの権限を変更します: `chmod 600 ~/.ssh/id_rsa`
2. 新しい Git インストールにユーザーを追加します: `ssh-add ~/.ssh/id_rsa`
3. 接続をテストします: `ssh -T git@ssh.gitlab.gnome.org` または `ssh -T git@github.com`

リポジトリごとに、オリジンをフェッチし、一致するようにローカル ブランチをリセットします。

```bash
git reset --hard remote-name/branch-name
git clean -df
```

`git status` を実行して、リポジトリがクリーンであることを確認します。

新しい OS にデータとリポジトリが復元されました。この設定は、Lumi 開発に使用される既知の作業環境を反映しており、必要に応じて個々のワークフローに適応させることができます。

## OSセットアップ後にLumiをビルドする

Lumi ビルド スクリプトは次の場所にあります。

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