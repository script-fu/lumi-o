---
title: "可変個引数関数"
type: docs
weight: 2
---
Scheme の **可変引数関数** は、可変数の引数を受け入れる関数です。これらの関数は汎用性が高く、柔軟で再利用可能なコードを作成できます。関数型プログラミングでは、可変引数関数を使用すると、数値リストの合計や文字列の連結など、任意の数の入力を処理する必要がある操作が簡素化されます。

可変引数関数は、次の場合に特に役立ちます。

- 引数の数は事前に決定できません。
- 同じ操作を入力の動的リストに適用する必要があります。
- データの集約または変換のためのユーティリティを作成します。

### 可変個引数関数の構文

可変引数関数は、最後のパラメーター名の前に `.` 記号を使用して定義されます。この最後のパラメータは、残りのすべての引数をリストに収集します。

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** 関数が受け入れる必須の固定引数。
- **`variadic-parameter`:** 追加の引数をリストとして収集する `.` が前に付く特別なパラメータ。
- **`body-expression`:** 関数が呼び出されたときに実行されるロジック。

### 可変個引数関数の例

#### 基本的な可変個引数関数

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **説明**:
  - `numbers` は、すべての引数をリストに収集します。
  - `apply` は、`+` 関数をリストのすべての要素に適用します。

**使用方法**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### 固定パラメーターを使用した可変個引数関数

固定パラメーターと可変引数パラメーターを組み合わせて、より柔軟な関数を作成できます。

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **説明**:
  - `prefix`は固定引数です。
  - `names` は、残りの引数をリストに収集します。
  - 各名前には、`map` および `lambda` を使用して指定された文字列が接頭辞として付けられます。

**使用方法**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### 固定ロジックと可変個ロジックの組み合わせ

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **説明**:
  - `collection-name` は固定パラメータです。
  - `items` は、追加の引数をリストに収集します。
  - この関数は、コレクション名と項目を 1 つの文字列に連結します。

**使用方法**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### 高度な使用例

#### 任意の入力の処理

可変引数関数は、任意のデータの処理に優れています。正の数値のみを合計する例を次に示します。

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- 合計する前に正でない数値をフィルターで除外します。

**使用方法**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### 再帰ロジックを使用した可変個引数関数

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **説明**:
  - `first` は最初の引数を処理します。
  - `rest` は、残りの引数をリストに収集します。
  - 最大値を再帰的に計算します。

**使用方法**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### 可変個引数関数の利点

- **柔軟性:** 幅広い入力ケースを処理します。
- **簡潔さ:** 複数のオーバーロードされた関数の必要性を減らします。
- **動的操作:** 引数の数を事前に知らなくても、ランタイム データ処理を有効にします。

### 可変引数関数を使用する場合

次の場合に可変引数関数を使用します。

- 関数は未知の数の引数を処理する必要があります。
- 1 つの操作がすべての入力に適用されます (合計、連結、マッピングなど)。
- 動的引数を使用した高次ロジックの簡素化。

次の場合は、可変個引数関数を避けてください。

- 入力検証または型チェックが複雑です。
- 必要なロジックには固定引数で十分です。
- 操作が複雑すぎるため、可読性が損なわれます。

＃＃＃ 結論Scheme の可変引数関数は、動的入力を処理するための堅牢なメカニズムを提供します。構文と使用法を理解することで、さまざまなシナリオに適応する柔軟で強力なスクリプトを作成できます。可変個引数関数を高階関数と組み合わせると、コードがより簡潔で表現力豊かになります。