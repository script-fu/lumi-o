---
title: "最終的な考え"
type: docs
weight: 10
---
これで、作業プロシージャ プラグインと小さなヘルパー ライブラリが完成しました。このシリーズでは、ほとんどの Lumi スクリプトで使用するコア パターンを紹介しました。

- 機能: プラグインの構成要素。
- リファクタリング: 機能を維持しながらコード構造を改善します。
- コード ライブラリ: コードをクリーンでモジュール化した状態に保つために、再利用可能な関数を一元化します。
- 検証テクニック: コア ロジックを実行する前に入力が有効であることを確認します。

また、Git を使用して変更を追跡し、クリーンなプロジェクト構造を維持するための基本も学びました。このワークフローにより、作業バージョンを失うことなく反復処理が容易になります。

メインのプラグイン コードの最終バージョンは次のとおりです。

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

ライブラリコード:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## 結論

メッセージング ヘルパーを小さなライブラリにリファクタリングすることで、プラグインは意図に重点を置き、ライブラリには実装の詳細が含まれます。検証と一貫したメッセージ ルーティングにより、障害が予測可能になります。

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

次のステップ:

- 再利用可能なヘルパーを専用のライブラリ ファイルに移動します。
- プラグインを小さくし、その機能に応じたプロシージャに名前を付けます。
- 境界 (入力、ファイル パス、メニュー オプション) に検証を追加します。

最終結果をプラグイン リポジトリに 2 つのファイルとして保存します。

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`