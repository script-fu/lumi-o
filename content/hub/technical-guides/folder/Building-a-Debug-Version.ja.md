---
title: "デバッグバージョンのビルド"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
このガイドでは、`build/lumi/scripts` のスクリプトを使用した Lumi の **ローカル デバッグ ワークフロー**について説明します。

ワークフローは次のように設計されています。

- ローカル ビルド アーティファクトを使用します (シンボルのダウンロードは必要ありません)。
- デバッグ シンボルが実際に存在することを確認します。
- デフォルトでは、オフライン シンボル モードで GDB を起動します。

## 前提条件

- Debian ベースの Linux (プロジェクト ベースライン: Debian 13)
- Lumi ソース ツリーはすでにクローン化されています

## ワンタイム GDB セットアップ (オプションですが推奨)

GDB ツールをインストールします。

```bash
sudo apt update
sudo apt install gdb gdbserver
```

オプションのローカル ロギング設定:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注: Lumi のローカル デバッグ スクリプトは、シンボル解決をローカルかつ再現可能に保つために、デフォルトで `debuginfod` を無効にします。

## クイックスタート

スクリプト ディレクトリから:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### デバッグビルド + 起動 (デフォルト)

これは通常のデバッグ セッションに使用します。

```bash
bash lumi-debug-local.sh lumi-dev build
```

このコマンド:

1. Lumi をデバッグモードでビルドします。
2. デバッグシンボルを検証します。
3. GDB の下で Lumi を起動します。

### デバッグ ビルドのみ (後の TTY/リモート セッション用)

今すぐビルドして後で起動/デバッグしたい場合にこれを使用します。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Linux での TTY の使用

TTY (テキスト コンソール) は、多くの場合、ハード フリーズをデバッグする最も信頼できる方法です。

- `Ctrl + Alt + F1` から `Ctrl + Alt + F6` の TTY に切り替える
- テキストプロンプトからログインします
- `Ctrl + Alt + F7` (一部のシステムでは `F2`) を使用してグラフィカル セッションに戻ります。

これが重要な理由: デスクトップ セッションが停止しても、TTY は多くの場合まだ応答するため、GDB を接続し、バックトレースをキャプチャし、有用なクラッシュ データを回復できます。

## オプション: リモート/TTY デバッグ

ハード フリーズまたはディスプレイのハングアップの場合は、`gdbserver` を使用します。

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

次に、TTY (フリーズ シナリオに推奨) または別の端末から次のようにします。

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

ローカル GDB 起動の場合 (非 TTY パス):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## パフォーマンスに関するメモ

デバッグ ビルドは設計により遅くなります。デバッグが完了したら、より高速なビルドに戻します。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```