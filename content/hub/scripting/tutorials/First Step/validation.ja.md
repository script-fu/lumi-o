---
title: "検証"
type: docs
weight: 4
---
堅牢なプラグインを構築する場合、誤用や予期しない入力があった場合でも、関数がエラーを適切に処理し、期待どおりに動作することを確認することが重要です。検証は、関数の整合性を保護し、クラッシュや意図しない動作を防ぐのに役立ちます。

`send-message` 関数が入力を正しく処理することを確認するための検証チェックを追加して、関数を改善する方法を見てみましょう。

### 入力を検証する

メッセージを送信する前に、`send-message` 関数に渡される `output` 引数が有効であることを確認する必要があります。出力先が期待値 (gui、error-console、またはターミナル) のいずれかであることを確認するチェックを追加できます。

例:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Send to the Error Console
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Send to the GUI dialog box
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Send to the terminal window
      ((eq? output 'terminal)
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

この例では、`member` を使用して、`output` 引数が有効かどうかを確認します。そうでない場合、関数は明確なメッセージとともにエラーを生成し、無効な値による問題の発生を防ぎます。

### 空のメッセージを処理する

`message` 引数が有効であることを確認するのにも役立ちます。たとえば、空の文字列または #f (false) がメッセージとして渡された場合、関数はこれを適切に処理する必要があります。

空のメッセージを処理する例:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

このアプローチにより、関数が常に有効な入力を受信できるようになり、信頼性が向上し、予期しない動作が防止されます。

### 組み合わせた検証の例

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Send to the Error Console
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Send to the GUI dialog box
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Send to the terminal window
        ((eq? output 'terminal)
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

このバージョンでは:
- この関数は、最初に `message` が空か無効かをチェックします。メッセージが有効な場合は、`output` が受け入れられる値 (`gui`、`error-console`、または `terminal`) のいずれかであるかどうかのチェックに進みます。
- 両方のチェックに合格した場合、メッセージは適切な出力に送信されます。それ以外の場合は、明確な説明とともにエラー メッセージが表示されます。
- メッセージも文字列であることを確認するために追加のチェックが行われます。

この結合された検証関数により、コードがクリーンな状態に保たれ、アクションが実行される前に両方の入力が検証されることが保証されるため、関数がより堅牢になります。デバッグ メッセージング システムも構築していることに注意してください。とき
コードが失敗すると、その理由がわかります。自分たちで書いた理由がわかります。

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```