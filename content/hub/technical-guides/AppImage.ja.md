---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage は、単一ファイルの Linux アプリケーションパッケージです。1 つのファイルをダウンロードして実行可能に設定すれば、システム全体へのインストールなしで実行できます。

公式 AppImage サイト: https://appimage.org/

AppImage は、インストールやシステム変更なしで動作する Lumi のポータブル版を提供します。依存関係の管理、ソースコードのコンパイル、開発環境の構成をせずに、すぐにソフトウェアを使いたいアーティストに最適です。

自己完結型の実行ファイルとして、AppImage はシステム上の任意の場所に保存できます。新しいリリースのテスト、複数バージョンの保持、マシン間での移動が容易になります。

Lumi の開発プロセスでは、AppImage は継続的インテグレーションの出力と密接に一致するポータブルテストビルドとして機能します。ローカルソースビルドを開発作業に集中させながら、一貫した環境で信頼性の高いテストが可能です。

注: CI は Lumi のリポジトリ内統合依存関係ソース（BABL/GEGL/GTK3）を使って AppImage をビルドするため、依存関係スタックはローカルの `lumi-build-script.sh` ワークフローと一致します。

## リリース版と開発版 AppImage

- **リリース AppImage**: まだ提供されていません（Lumi はまだリリースされていません）。
- **開発 AppImage（CI アーティファクト）**: テスト用に、進行中の開発コミットから自動生成されます。

このガイドでは主に **開発 AppImage** のワークフローを扱います。

現在のアーティファクトページ:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage ダウンロードの基本

CI はアーティファクト zip ファイル（例: `lumi-appimage*.zip`）を生成します。

基本的な手動手順:

1. 最新の CI アーティファクト zip をダウンロードする
2. 展開する
3. 同梱の `Lumi*.AppImage` ファイルを実行する

以下のスクリプトは、これらの手順を自動化する任意のヘルパーです。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 任意のヘルパースクリプト

- `lumi-appimage-unpack-zip.sh`
  - `~/Downloads` 内の最新 `lumi-appimage*.zip` を検索
  - AppImage を `~/AppImage/Lumi/Lumi_CI.AppImage` に配置
  - デスクトップリソースを `~/.local/share/applications/lumi.desktop` に配置

- `lumi-appimage-launch.sh`
  - ターミナルで AppImage を起動
  - ランタイム出力を有効化（`APPIMAGE_DEBUG=1`）

## 一般的な注意事項

- AppImage を手動で（ヘルパースクリプトなしで）実行する場合は、先に実行可能にしてください:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` は実行権限を自動的に付与します。

- Lumi が別のビルドからすでに実行中の場合は、AppImage を起動する前に終了してください。
