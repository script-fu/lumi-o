---
title: "読み込み中"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
ヘルパー関数が大きくなったらすぐに、それを小さなライブラリ ファイルに移動します。これにより、プラグインに焦点が当てられ、ヘルパーを複数のプラグイン間で再利用できるようになります。

### Make a Library Function

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

Example of a library function send-message.scm

```scheme
;; さまざまな出力先へのメッセージ出力を処理する関数
(define (send-message message output)
  (cond
    ;; Message console に送信
    ((eq? output 'error-console)
       ;; ハンドラを Message console に設定する
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; GUI ダイアログボックスに送信
    ((eq? output 'gui)
       ;; ハンドラを GUI ダイアログに設定する
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; ターミナルウィンドウに送信
    ((eq? output 'terminal)
       ;; terminal 出力は display で処理される
       (display message)))

  ;; 既定のメッセージハンドラを Message console に戻す
  (lumi-message-set-handler 2))
```

### Load the Library Function

Scheme `load` コマンドを使用してそのライブラリ関数をロードできます。

ライブラリファイルのロード:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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