---
title: "デバッグ版のビルド"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

このガイドでは、`build/lumi/scripts` 内のスクリプトを使った Lumi の **ローカルデバッグワークフロー** を説明します。

このワークフローは次の目的で設計されています:

- ローカルビルド成果物を使う（シンボルのダウンロードは不要）
- デバッグシンボルが実際に含まれていることを確認する
- デフォルトでオフラインシンボルモードの GDB を起動する

## 前提条件

- Debian ベースの Linux（プロジェクト基準: Debian 13）
- Lumi ソースツリーがすでにクローン済み

## 一度だけ行う GDB セットアップ（任意ですが推奨）

GDB ツールをインストールします:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

任意のローカルログ設定:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

注: Lumi のローカルデバッグスクリプトは、シンボル解決をローカルかつ再現可能に保つため、デフォルトで `debuginfod` を無効にします。

## クイックスタート

スクリプトディレクトリから:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### デバッグビルド + 起動（デフォルト）

通常のデバッグセッション向けです。

```bash
bash lumi-debug-local.sh lumi-dev build
```

このコマンドは次を行います:

1. Lumi をデバッグモードでビルドする
2. デバッグシンボルを検証する
3. GDB 下で Lumi を起動する

### デバッグビルドのみ（後で TTY/リモートセッション用）

今ビルドして、起動やデバッグは後で行う場合に使います。

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Linux で TTY を使う

TTY（テキストコンソール）は、ハードフリーズのデバッグで最も信頼できる方法であることが多いです。

- `Ctrl + Alt + F1` から `Ctrl + Alt + F6` で TTY に切り替える
- テキストプロンプトからログインする
- `Ctrl + Alt + F7`（一部のシステムでは `F2`）でグラフィカルセッションに戻る

なぜ重要か: デスクトップセッションが固まっても TTY は応答することが多く、GDB を接続してバックトレースを取得し、有用なクラッシュデータを回収できます。

## オプション: リモート / TTY デバッグ

ハードフリーズやディスプレイのロックアップには `gdbserver` を使います:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

TTY（フリーズ時は推奨）または別のターミナルから:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

ローカル GDB 起動（非 TTY パス）:

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## パフォーマンスに関する注意

デバッグビルドは意図的に遅くなります。デバッグが終わったら、より高速なビルドに戻してください:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
