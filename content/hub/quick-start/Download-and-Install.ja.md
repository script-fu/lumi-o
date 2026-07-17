---
title: "ダウンロードとインストール"
type: docs
url: "hub/quick-start/Download-and-Install"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5f17d7e9009aeeacf256152bef94386ccc5a8eea87cf0feebef073488fb59283
---
すでに Linux を使用していて、Lumi をすぐに実行したい場合は、GitLab アーティファクトの最新の **開発 AppImage** を使用してください。

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. 最新の開発 AppImage アーティファクト zip をダウンロードします。
2. zip を解凍します。
3. `Lumi*.AppImage` ファイルをダブルクリックして実行します。

AppImage はすでに実行可能になっているはずです。そうでない場合は、ファイルのアクセス許可で **ファイルのプログラムとしての実行を許可** を有効にするか、以下のターミナル メソッドを使用します。

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```