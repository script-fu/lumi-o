---
title: "読み込み中"
type: docs
weight: 3
---
ヘルパー関数が大きくなったらすぐに、それを小さなライブラリ ファイルに移動します。これにより、プラグインに焦点が当てられ、ヘルパーを複数のプラグイン間で再利用できるようになります。

### ライブラリ関数を作成する

send-message 関数を使用して、それをコンテンツとして含む新しいファイルを作成できます。ファイルをプラグイン部分ではなく、リポジトリ フォルダー (おそらく最上位近く) に保存します。

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: これは、Scheme コードを保存するためのメイン ディレクトリです。
  - **ライブラリ/**: `send-message.scm` のような共有関数が存在する場所です。
  - **plug-ins/**: これは、個々のプラグインが保存される場所です。
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

ライブラリ関数send-message.scmの例

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### ライブラリ関数をロードする

Scheme `load` コマンドを使用してそのライブラリ関数をロードできます。

ライブラリファイルのロード:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

おい！コメントなしでそれ自体を説明する、よりシンプルで読みやすいものを用意しました。これがリファクタリングの満足のいく結論です。