---
title: "クイックスタート"
type: docs
---
Lumi はまだリリースされておらず、開発版として利用可能です。

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

## Linux でのワコムのセットアップ

Lumi でのデジタル ペイントの場合、通常はシンプルな **線形圧力設定** が最適です。

- タブレット ドライバーの圧力曲線を直線に保ちます。
- Lumi の圧力/入力曲線を線形に保ちます。
- ブラシのダイナミクスは非線形になる可能性があるため、ブラシ自体で感触を形作ります。

次のコマンドを使用して、Linux ドライバー曲線を確認およびリセットできます。

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

実践的なヒント:

- Lumi は現在、X11 の不具合を回避するために、問題のある Wacom パッド/タッチリング入力をブロックしています。代わりに、タブレット ボタンを **相対** ブラシ サイズの上下にマップします。
- `Alt` によるブラシ サイズのドラッグが機能しない場合は、デスクトップが `Alt` を使用してウィンドウを移動している可能性があります。ウィンドウ マネージャーのショートカットを `Super` に変更するか、無効にします。

ソース コードから作業したい場合は、[Technical Guides](/hub/technical-guides/) および [Installation](/hub/technical-guides/Installation/) にアクセスしてください。