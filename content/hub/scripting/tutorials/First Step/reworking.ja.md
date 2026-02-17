---
title: "手直し"
type: docs
weight: 7
---
この手順では、メッセージングの例の微妙な動作を修正します。

文字列「Hello world!\n」をメッセージとして渡していました。 「\n」は特別な種類の文字、「エスケープ」文字です。これは、出力印刷に改行を開始するように指示します。 Scheme では、ステータス バーに送信されたメッセージが GUI ボックスとして強制的にポップアップされます。

ヘルパー `send-to-gui` は、Lumi ダイアログ ボックスにメッセージを送信します。

例が一貫して動作するように、メッセージの内容と宛先を更新します。

エスケープ文字を削除して関数を拡張します。
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

マジックナンバーを Lumi が提供する定数に置き換えます (`MESSAGE-BOX` や `ERROR-CONSOLE` など)。

次に、検証を 2 つの関数に分割して、複数の呼び出しサイトから再利用できるようにします。

- (is-valid-string?) send-to* 関数内で、文字列が空の文字列ではなく文字列であることを確認します。
- (is-valid-output-display?) send-message 関数で、指定された出力先が有効であることを確認します。

ライブラリを再加工します。

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## 結論

メッセージング ライブラリを作り直すことで、より堅牢で信頼性の高いものになりました。改行文字に関する隠れた問題を修正し、わかりやすくするために定数を導入し、ステータス バーとダイアログ ボックスの出力のサポートを追加することで機能を拡張しました。さらに、検証ロジックをより小さな焦点を絞った関数に分割することで、将来のコードの保守と拡張が容易になります。

この再作業は、小さな変更がライブラリの全体的な構造と機能を強化し、プロジェクトの成長に合わせて柔軟性と再利用性を高める道を開く方法を示しています。