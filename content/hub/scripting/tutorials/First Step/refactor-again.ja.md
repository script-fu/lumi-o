---
title: "再度リファクタリングする"
type: docs
weight: 5
---
ヘルパー ライブラリが大きくなるにつれて、一目で理解するのが難しくなります。各関数を小さく、単一目的に保つために再度リファクタリングします。

### 複雑さを解消する

関数の追跡と保守を容易にするために、関数をより小さな、焦点を絞った関数に分割します。まず、検証をメッセージ ルーティングから分離します。

### 検証関数を作成する

`message` 引数と `output` 引数を検証する関数の部分を取り出して、別の関数に移動できます。こうすることで、コアの `send-message` 関数は検証について心配する必要がなくなり、追跡が容易になります。

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### メッセージ送信を簡素化する

検証が別の関数に移動されたため、`send-message` 関数はメッセージの送信だけに集中できます。メッセージを正しい宛先に送るという特定のタスクのみを処理するため、はるかに単純になります。

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

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
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### さらに細分化: 各出力ハンドラーを分離する

各タイプのメッセージ出力 (GUI、エラー コンソール、ターミナル) は、独自の機能に移動できます。これにより、テスト、変更、将来の拡張が容易になります。

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 各送信関数での検証の再利用

検証はメッセージと出力の両方が正しいことを確認する重要な部分であるため、各 `send-*` 関数が独自の検証を実行することは理にかなっています。これにより、どの出力が呼び出されても、常に最初に入力がチェックされることが保証されます。

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

メッセージ送信関数から検証を削除し、責任を個々の出力関数に移したことを確認してください。この変更により、各宛先 (GUI、エラー コンソール、ターミナル) が独自の検証を処理するようになり、メッセージ送信機能が合理化され、検証ロジックが必要な場所に近づけられるようになります。

このアプローチでは、メッセージ送信関数を単純化して _dispatcher_ にすると同時に、各 send-to-* 関数が処理前にメッセージを正しく検証することを保証できます。

検証を各 send-to-* 関数に移動することで、スタンドアロン関数として再利用できるようにしました。これは、メッセージ送信ディスパッチャー関数に依存せずに、send-to-gui、send-to-error-console、または send-to-terminal 関数のいずれかを直接呼び出すことができることを意味します。これらの各関数は独自のロジックを完全に処理できるようになり、コードの他の部分または他のプラグインで独立して使用できるため、コードがよりモジュール化され、柔軟性が高まります。

## リファクタリングの利点

- **懸念事項の明確な分離**: 各関数は 1 つの責任のみを処理するようになり、コードが理解しやすくなりました。
- **拡張性**: 新しい出力タイプの追加は簡単です。 `send-to-file` や `send-to-logger` のような新しい関数を定義し、`cond` ステートメントにケースを追加するだけです。
- **再利用性**: これらの出力処理関数はそれぞれ、プロジェクト内の他の場所で再利用したり、複数のプラグイン間で共有したりできます。
- **一貫性**: 各 `send-to-*` 関数で検証関数を再利用することで、すべての出力が適切に検証されることを保証し、コードをより堅牢にします。

リファクタリングされたライブラリのバージョン:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

私たちにできることはそれだけでしょうか？いいえ！やるべきことはまだあります。ぜひ読んでください。