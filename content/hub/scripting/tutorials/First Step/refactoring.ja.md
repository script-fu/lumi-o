---
title: "リファクタリング"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bc83f55511f34e6f099f8de8c6af3bba5e459974aa4bff6265ae70d679517646
---
関数が動作するようになったら、一歩下がって、コードをどのように構成するのが最適かを考えることができます。目標は、プラグインをできるだけ明確で、理解しやすく、保守しやすいものにすることです。既存のコードの動作を変更せずにその構造を改善および改良するこのプロセスは、リファクタリングとして知られています。

ここでも初期関数を示します。

```scheme
(define (scheme-hello-world)
  ;; メッセージハンドラを設定し、メッセージを GUI ダイアログボックスに出力する
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; メッセージハンドラを設定し、メッセージを Error Console に出力する
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; メッセージを terminal（Lumi を起動した OS ウィンドウ）に送る
  (display "Hello world!\n"))
```

function-name は関数の名前で、parameter は関数が入力として受け入れるものです。本体は、関数が呼び出されたときに実行されるコードのブロックです。

抽象的な形式:

```scheme
(define (function-name parameter)
  body)
```

### コードの繰り返し

繰り返しは早めに削除してください。 `(lumi-message "Hello world!\n")` が 2 回繰り返され、メッセージ文字列が 3 回繰り返されます。変数は繰り返される文字列を解決します。

### 変数

Scheme では、変数には変数が認識されている「スコープ」があり、そのスコープは `let` ステートメントを使用して設定されます。変数はバインディング部分の値にバインドされており、変数のスコープは let 本体にあります。この変数は let ブロック内でのみ認識され、let ブロックの外からはアクセスできません。

```scheme
(let ((variable value))
  body)
```

「message」という変数を導入します。

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; メッセージハンドラを設定し、メッセージを GUI ダイアログボックスに出力する
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; メッセージハンドラを設定し、メッセージを Error Console に出力する
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; メッセージを terminal（Lumi を起動した OS ウィンドウ）に送る
    (display message)))
```

この例では、文字列「Hello world!\n」にバインドされた「message」という変数を使用しました。これにより、メッセージの内容を 3 回ではなく 1 回変更できるようになり、エラーの可能性が減り、コードがより柔軟になります。

### 関数の抽出

関数型プログラミングでは、コードをリファクタリングして再利用可能なロジックを個別の関数に抽出するのが一般的です。これにより、**メイン関数**はよりシンプルになり、その高レベルの目標に重点が置かれるようになりますが、**抽出された関数**は詳細なロジックを処理するため、より複雑に見えます。これは意図的なものであり、モジュール性、関心事の分離、読みやすさなどの関数型プログラミングの中核原則と一致しています。リファクタリングされたものは次のとおりです
ハローワールド!抽出後。

ロジックを抽出すると、次のようになります。
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; メイン関数
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; さまざまな出力先へのメッセージ出力を処理する関数
(define (send-message message output)
  (cond
    ;; Error Console に送信
    ((eq? output 'error-console)
       ;; ハンドラを Error Console に設定する
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

  ;; 既定のメッセージハンドラを Error Console に戻す
  (lumi-message-set-handler 2))

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

#### 記号

上の例では、「gui」などのシンボルと呼ばれるデータ型が使用されています。シンボルはパラ​​メーターとして send-message 関数に渡され、単純な条件決定を行うために使用できます。シンボリック キーと同様に、これらは一意の識別子です。シンボルの詳細については、[このページ](/hub/scripting/fundamentals/variables-and-scope/symbols/) をご覧ください。

### main 関数の簡略化

元の (scheme-hello-world) 関数では、さまざまな出力 (GUI、エラー コンソール、ターミナル) にメッセージを送信するためのすべてのロジックが main 関数に組み込まれていました。リファクタリング後、メイン関数は単に**何を行う必要があるか**に焦点を当て、メッセージをさまざまな宛先に送信します。

リファクタリングされた main 関数はより単純です。

- 目的を明確に示しています: 同じメッセージを複数の出力に送信します。
- さまざまな出力にメッセージ ハンドラーを設定するなど、繰り返しコードでメイン ロジックが乱雑になるのを防ぎます。
- 一目で読みやすく、理解しやすくなりました。

### 抽出された関数の複雑さ

対照的に、**(メッセージ送信) 関数** には詳細なロジックが存在します。各出力 (GUI、エラー コンソール、ターミナル) の動作の変化を処理できるようになりました。この機能は以前よりも少し複雑ですが、**集中化**され**分離されました**。

## これを関数型プログラミングに関連付ける

関数型プログラミングでは、関数は **第一級市民**として見なされます。つまり、関数を再利用したり、渡したり、組み合わせてより複雑な動作を形成したりすることができます。目標は次のとおりです。- **問題をより小さな独立した部分に分解**します。
- **複雑性を**、`send-message` など、特定のタスクを処理する小さな関数に分離します。
- **高レベルの関数をシンプルに保つ**ことで、各タスクの実行方法の詳細を知る必要がなく、データとアクションのフローの調整に集中できます。
- **懸念事項の分離**: この関数は、出力タイプに基づいてメッセージの送信方法を処理し、このロジックをメイン関数から分離します。
- **モジュール性**: すべてのメッセージ送信ロジックを 1 か所で処理することで、メイン関数を変更することなく、簡単に変更 (新しい出力オプションの追加など) を行うことができます。
- **再利用性**: `send-message` 関数は再利用可能です。つまり、コード内の他の場所で複数の出力にメッセージを送信する必要がある場合、同様のロジックを書き直すのではなく、単にこの関数を呼び出すことができます。

リファクタリングにより、この例の main 関数は何が起こっているかを示す**宣言** ステートメント (「3 か所にメッセージを送信する」) になり、これらのメッセージを送信する方法の複雑さは `send-message` 関数に抽象化されます。