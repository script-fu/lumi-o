---
title: "アプリイメージ"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
AppImage は、単一ファイルの Linux アプリケーション パッケージです。 1 つのファイルをダウンロードして実行可能としてマークし、システム全体にソフトウェアをインストールせずに実行します。

公式AppImageサイト：https://appimage.org/

AppImage は、インストールやシステムの変更を行わずに実行できる Lumi のポータブル バージョンを提供します。依存関係の管理、ソース コードのコンパイル、開発環境の構成を行わずに、すぐにソフトウェアを使用したいアーティストに最適です。

AppImage は自己完結型の実行可能ファイルとして、システム上のどこにでも保存できます。これにより、新しいリリースのテスト、複数のバージョンの保持、マシン間でのソフトウェアの移動が簡単になります。

Lumi の開発プロセスでは、AppImage は継続的インテグレーションの出力と厳密に一致するポータブル テスト ビルドとして機能します。これにより、ローカル ソース ビルドを開発作業に集中させながら、一貫した環境で信頼性の高いテストが可能になります。

## リリースと開発 AppImage

- **AppImage のリリース**: まだ利用できません (Lumi はリリースされていません)。
- **開発 AppImage (CI アーティファクト)**: テストのために進行中の開発コミットから自動的に生成されます。

このガイドでは主に **AppImage 開発**のワークフローについて説明します。

現在の成果物ページ:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage ダウンロードの基本

CI はアーティファクト zip ファイル (`lumi-appimage*.zip` など) を生成します。

基本的な手動フロー:

1. 最新の CI アーティファクト zip をダウンロードします。
2. 抽出します。
3. 付属の `Lumi*.AppImage` ファイルを実行します。

以下のスクリプトは、これらの手順を自動化するオプションのヘルパーです。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## オプションのヘルパー スクリプト

- `lumi-appimage-unpack-zip.sh`
  - `~/Downloads` で最新の `lumi-appimage*.zip` を見つけます
  - AppImage を `~/AppImage/Lumi/Lumi_CI.AppImage` にインストールします
  - デスクトップ リソースを `~/.local/share/applications/lumi.desktop` にインストールします

- `lumi-appimage-launch.sh`
  - 端末で AppImage を起動します
  - ランタイム出力を有効にする (`APPIMAGE_DEBUG=1`)

## 共通の注意事項

- AppImage を手動で (ヘルパー スクリプトを使用せずに) 実行する場合は、最初に実行可能にします。

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` はすでに実行可能権限を自動的に適用しています。

- Lumi がすでに別のビルドから実行されている場合は、AppImage を起動する前に Lumi を閉じてください。