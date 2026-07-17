---
title: "戻り値"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 80a8f61c6fc7f6b86167f7489f61558b49f3d1d2b7e1e5236406cbca31ff611e
---
戻り値は、余分な状態を持たずにフローを制御できるため、重要です。 Scheme では、最後に評価された式が戻り値になります。

このページでは、メッセージングの例の検証ヘルパーを使用して、明示的な戻り値によってコードの作成がどのように容易になるかを示します。

### 戻り値とは何ですか?

Scheme では、関数の戻り値は、関数が評価する最後の式によって決まります。これは、関数のコードの最後の行が評価したものが関数の結果として返されることを意味します。値が明示的に返されない場合、関数は `#f` (false) または `undefined` を返します。

検証関数 (is-valid-string?) をもう一度見てみましょう。

```scheme
;; 目的: メッセージが空でない文字列であることを検証する
(define (is-valid-string? message)
  ;; メッセージが空でない文字列かどうかを確認する
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

この関数では、メッセージが無効な場合、エラーがスローされます。ただし、メッセージが有効な場合、明示的な戻り値は与えられず、関数はデフォルトで `#f` を返します。

### 戻り値を明示的にする

戻り値をより明示的にすることで、これを改善できます。たとえば、メッセージが有効な場合は `#t` (true) を返すことができます。

```scheme
;; 目的: メッセージが有効な出力先に送られることを検証する
(define (is-valid-output-display? output)
  ;; 出力が想定される表示先のいずれかかどうかを確認する
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

このバージョンでは、メッセージが有効な場合、関数は `#t` を返し、明確な結果を提供します。これにより、ブール値の結果が必要な他のコンテキストでこの関数をより柔軟に使用できるようになります。

### 戻り値を効果的に使用する

関数が何を返すかを決定することで、関数をより予測可能で便利なものにすることができます。 `#t`、`#f` などの値、または特定の結果を返すと、関数がコードの残りの部分とどのように対話するかをより詳細に制御できるようになります。たとえば、戻り値を使用して呼び出し側関数でさらに決定を行ったり、戻り値を引数として別の関数に渡すことができます。

戻り値を使用してロジック フローを制御する簡単な例を次に示します。

```scheme
;; 目的: メッセージを適切な出力先に送る
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

この場合、(send-message) は (is-valid-output-display?) の戻り値に基づいて続行するかどうかを決定します。
最初のテストが失敗した場合、条件ステートメント `cond` はスキップされます。また、有効な出力表示である場合、非常に自然な方法で読み取れることにも注目してください。

## Scheme 内の If ステートメントのロジック

リファクタリングされたライブラリの例の前に、条件付きロジックを簡単に確認します。スキームは `if` を使用して 2 つのパスから選択します。

`if` ステートメントの単純な形式を次に示します。

```scheme
(if (conditional test)
  do if true
  do if false)
```

この構造は条件をチェックし、条件が true の場合は最初のアクションを実行します。条件が false の場合、2 番目のアクションが実行されます。

条件が true または false のときに複数のアクションを実行する必要がある場合は、`begin` を使用してそれらをグループ化できます。

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

これにより、条件テストの結果に応じて複数の式またはステートメントを実行する必要がある、より複雑な状況を処理できるようになります。

さて、これは戻り値が埋め込まれ、実行プロセスの制御に使用されるライブラリ コードです。

### 戻り値を使用してリファクタリングされました

```scheme
;; 目的: メッセージをステータスバーに送り、成功時は #t を返す
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; 目的: メッセージをダイアログボックスに送り、成功時は #t を返す
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; 目的: メッセージを Error Console に送り、成功時は #t を返す
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; 目的: メッセージを terminal に送り、成功時は #t を返す
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; 目的: メッセージを適切な出力先に送り、成功時は #t を返す
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; 目的: メッセージが空でない文字列であることを検証し、有効なら #t を返す
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; 目的: 出力が有効な宛先であることを検証し、有効なら #t を返す
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## 結論

戻り値は、関数を柔軟かつ再利用可能にするための基本的な部分です。各関数が何を返すかを慎重に決定することで、関数が相互に適切に連携し、コードの残りの部分に有用な情報を提供できるようになります。 `#t` や `#f` を返す場合でも、より具体的なものを返す場合でも、戻り値はプログラムのフローを制御し、さまざまな結果を処理する方法を提供します。