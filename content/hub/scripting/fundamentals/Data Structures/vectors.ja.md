---
title: "ベクトル"
type: docs
weight: 5
---
Scheme では、ベクトルは値をグループ化するために使用されるもう 1 つの基本的なデータ構造です。リストとは異なり、ベクトルは固定サイズのインデックス付き要素コレクションであり、より高速なランダム アクセスと更新を提供します。ベクトル内の各要素は、別のベクトルを含め、任意のタイプにすることができます。ベクトルは、# に続く括弧を使用して表されます。 `#(1 2 3)`

ベクトルとリストは似ているように見えますが、Scheme プログラミングでは異なる目的を果たします。

- リストは、リンクされたノードの実装により、再帰的分解を通じてリストの開始とトラバースを効率的に操作できるため、再帰的操作や動的構造によく使用されます。

- 一方、ベクターは、要素へのランダム アクセスや特定のインデックスでの更新が必要なシナリオ向けに最適化されており、ルックアップ テーブル、固定サイズの構成、またはパフォーマンスが重要なインデックス操作などのユースケースにより適しています。

基本的に、再帰アルゴリズムや動的サイズのデータ​​にはリストが自然な選択ですが、固定サイズまたはインデックス付きのアクセス パターンが最も重要な場合にはベクトルが最適です。

### 単純なベクトル

```scheme
(vector 1 2 3)
```

- `1`、`2`、`3` の 3 つの要素からなるベクトルを作成します。

結果: **`#(1 2 3)`**

#### ベクトル要素へのアクセス

ベクトル内の要素には `vector-ref` プロシージャを使用してアクセスし、指定されたインデックス (`0` から始まる) で要素を取得します。

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### 反復: ベクトル内の各要素の処理

ループまたは再帰を使用してベクトルを反復処理できます。スキームは、ベクトルのサイズを決定する `vector-length` を提供します。以下は、ベクター内のすべての要素を出力する単純なループです。

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **基本ケース:** インデックス `i` がベクトルの長さに達すると、ループを停止します。
- **再帰的な場合:** インデックス `i` の要素を出力し、`i` をインクリメントします。

#### 使用例

```scheme
(print-elements (vector 1 2 3))
```

結果:

- `"1"`
- `"2"`
- `"3"`

結果:「完了」

### 混合ベクトル

ベクトルには、文字列、ブール値、数値、他のベクトル、さらには式の結果など、さまざまなタイプの要素を含めることができます。

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

これにより、次のベクトルが作成されます。
  - 数字 (`42`)
  - 文字列 (`"hello"`)
  - ブール値 (`#t`)
  - 別ベクトル (`#(1 2)`)
  - 式の結果 (`(+ 3 4)`、`7` と評価される)

結果: **`#(42 "hello" #t #(1 2) 7)`**

### ベクトルの構築

ベクトルは `vector` を使用して作成されるか、`make-vector` を使用して初期値を持つ固定サイズのベクトルを作成します。

```scheme
(make-vector 5 0)
```

すべての要素が `0` に初期化された、サイズ `5` のベクトルを作成します。

結果: `#(0 0 0 0 0)`

### ベクターの更新

`vector-set!` プロシージャは、指定されたインデックスにあるベクトル内の要素を更新します。

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

結果: `#(1 42 3)`

### ベクトルのチェック

`vector?` プロシージャは、指定された値がベクトルであるかどうかをチェックします。

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

結果:

- `(vector? (vector 1 2 3))` は `#t` (true) を返します
- `(vector? 42)` は `#f` (偽) を返します

### ベクトルと参照渡しの動作Scheme では、ベクトルは変更可能であり、参照によって渡されます。これは、ベクトルを関数に渡すと、関数が元のベクトルを直接変更できることを意味します。関数内でベクトルに加えられた変更は、関数の外部にも反映されます。この動作は、複数の機能間でデータを効率的に共有および更新するのに役立ちますが、意図しない副作用を避けるために注意も必要です。

#### 例: 関数内のベクトルの変更

以下は、ベクトルが参照によって渡され、変更される方法を示す例です。

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

結果: `#(10 99 30)`

#### 段階的な説明

1. **ベクターの作成:** `my-vector` は、値 `10`、`20`、および `30` で初期化されます。
2. **関数に渡す:** `my-vector` は、インデックスと更新する新しい値とともに `modify-vector` に渡されます。
3. **関数内の変更:** `vector-set!` プロシージャは、元のベクトル内の指定されたインデックスの値を直接更新します。
4. **変更の反映:** ベクトルは参照によって渡されるため、関数内で行われた変更は元のベクトルに反映されます。

#### 参照渡しの影響

- **パフォーマンス:** 参照によるベクトルの受け渡しは、大きな構造のコピーを回避できるため効率的です。
- **副作用:** 関数間でベクトルを共有する場合は、共有データへの意図しない変更を避けるために注意してください。

### ベクトルの演算

Scheme には、ベクトルを操作するための次のような組み込みプロシージャがいくつか用意されています。

- `vector-length`: ベクトル内の要素の数を返します。
- `vector->list`: ベクトルをリストに変換します。
- `list->vector`: リストをベクトルに変換します。

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

結果:

- `(vector-length (vector 1 2 3))` は `3` を返します
- `(vector->list (vector 1 2 3))` は `(1 2 3)` を返します
- `(list->vector (list 1 2 3))` は `#(1 2 3)` を返します

### 入れ子になったベクトル

Scheme 内のベクトルには、他のベクトルを要素として含めることができ、入れ子構造を作成します。

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

3 つの要素からなるベクトルを作成します。各要素はそれ自体ベクトルです。

結果: **`#(#(1 2) #(3 4) #(5))`**

#### ネストされたデータへのアクセス

ネストされたベクトル内の要素にアクセスするには、`vector-ref` を複数回使用して構造内を移動します。

#### 例: 要素へのアクセス

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### 概要

- Scheme の **Vector** は、固定サイズのインデックス付きデータ構造です。
- `vector` を使用してベクターを作成し、`vector-ref` を使用して要素にアクセスし、`vector-set!` を使用して要素を更新します。
- `vector-length`、`vector->list`、`list->vector` などの組み込みプロシージャにより、柔軟な運用が可能です。
- ネストされたベクトルにより、複雑な階層データ構造が可能になります。