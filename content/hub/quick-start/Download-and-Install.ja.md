---
title: "ダウンロードとインストール"
type: docs
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