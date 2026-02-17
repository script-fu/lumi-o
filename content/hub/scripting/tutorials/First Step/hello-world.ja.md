---
title: "「こんにちは世界」"
type: docs
weight: 1
---
このチュートリアルでは、Scheme プラグインの最小限の構造について説明します。一部の行は「定型文」です。まだ完全に理解していなくても、Lumi がファイルをロードするために必要です。

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

大まかに言うと、次のことを行います。

1. 関数を定義する
2. プロシージャデータベースに表示されるように登録します。
3. (オプション) メニューエントリを追加します
4. ファイルをプラグインフォルダーにインストールします

### 関数を定義する

_procedure_ とも呼ばれる関数は、名前と目的を持つコードの塊であり、入力を受け取り、出力を生成します。

**入力** > **_関数_** > **出力**

### 関数を登録する

登録とは、Lumi がそれを認識できるように関数名をリストに登録する行為です。

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### メニューへのリンク

これにより、Lumi のメニュー システム内で関数を見つける場所がわかります。

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

これにより、メイン メニュー バーにメニュー「Funky」が表示されます。パスを変更してプラグインを別の場所に置きます。パス `<Image>/Funky` は、プラグインが **画像** メニュー カテゴリの下に表示されることを意味します。プラグインを表示する場所に応じて、`<Image>` を `<Tools>`、`<Filters>` などに変更できます。

### コメント

Scheme の基本言語である Scheme では、コメントは通常、役に立つテキスト行の前に `;;` を付けることによって行われます。コメントの使用は、コーダーとしての流暢さによって決まります。時々コーディングする場合は、より多くのコメントが役に立ちます。常にコーディングを行っている場合、コードはコメントと同じくらい読みやすくなります。また、関数的にプログラミングする場合、コードはスクリプトのように読めるほど説明的なものになる傾向があります。

### 構文

コードでは、行を読みやすくするために、項目を行に配置する方法についてほとんどルールがありません。たとえば、文のコンマやピリオドの後にスペースが含まれる場合があります。読みやすさに役立ちます。

コードは同様の方法で物事を配置することがありますが、最初は奇妙に見えるかもしれません。

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## コード例

これが完全な例です。ほとんどの Lumi プロシージャには、`lumi-` というプレフィックスが付いています。たとえば、`lumi-message` は、構成されたメッセージ ハンドラーに文字列を出力します。

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### プラグインをインストールする

1. **Lumi -> 編集 -> 設定 -> フォルダー -> プラグイン** に移動します。
2. [repo](/hub/scripting/tools/git) プラグイン フォルダーをリストに追加します。
3. プラグイン用のフォルダーを作成し、上記のサンプル コードを `hello-world.scm` として保存します。
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. `hello-world.scm` ファイルを右クリックします。
5. **[プロパティ] -> [権限] -> [ファイルのプログラムとしての実行を許可]** に移動します。
6. Lumiを再起動します。

### プラグインを試してみる

プラグインは、Lumi メインウィンドウの「Funky」メニューの下に表示されるはずです。それをクリックすると、「Hello world!」が表示されるはずです。メッセージ。メッセージ テキストの変更など、コードを変更して、ファイルを保存してみてください。プラグインを再度実行すると、Lumi を再起動しなくても変更が反映されます。

メニュー パスを変更して実験してみてください。たとえば、`"<Image>/File"` はそれを [ファイル] メニュー内に配置し、`"<Image>/File/Funky"` は [ファイル] メニューに新しいセクションを作成します。これは、プラグインが表示される場所をカスタマイズし、ツールを整理するための優れた方法です。