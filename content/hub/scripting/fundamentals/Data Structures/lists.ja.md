---
title: "リスト"
type: docs
weight: 4
---
Scheme では、**リスト** は値をグループ化するために使用される基本的なデータ構造です。リストは要素の順序付けられたコレクションであり、各要素は別のリストを含む任意の型にすることができます。リストは、Scheme でデータ ストレージとプログラム構造の両方に広く使用されています。

### 例 1: 単純なリスト

```scheme
(list 1 2 3)
```

- 3 つの要素のリストを作成します: `1`、`2`、および `3`。

結果: **`(1 2 3)`**

---

#### リスト要素へのアクセス

リスト内の要素には、`car` および `cdr` プロシージャを使用してアクセスします。

- `car` は、リストの最初の要素を取得します。
- `cdr` は、リストの残りの部分 (最初の要素を除くすべて) を取得します。

#### 例

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

結果:

- `(car my-list)` は `1` を返します
- `(cdr my-list)` は `(2 3)` を返します

---

#### 単純な再帰: リストの反復処理

リストの `cdr` で `car` を再帰的に呼び出すことにより、リストを走査するまで各要素を 1 つずつ処理できます。これは、多くのリスト処理アルゴリズムの基礎を形成します。

#### 例: リストの各要素の出力

以下は、リスト内のすべての要素を出力する単純な再帰関数です。

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **基本ケース:** リストが空の場合 (`null? lst`)、再帰を停止します。
- **再帰的な場合:** 最初の要素 (`car lst`) を出力し、リストの残りの関数を呼び出します (`cdr lst`)。

#### 使用例

```scheme
(print-elements (list 1 2 3))
```

出力:

- `"1"`
- `"2"`
- `"3"`

結果:「完了」

---

#### 仕組み

1. この関数は、`car` を使用してリストの最初の要素を取得し、それを処理します。
2. 次に、リストの残りの部分を使用して自分自身を呼び出します (`cdr`)。
3. このプロセスは、リストが空になるまで繰り返されます (`null? lst`)。

---

### 例 2: 混合型

リストには、文字列、ブール値、数値、他のリスト、さらには式の結果など、さまざまなタイプの要素を含めることができます。

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- これにより、以下のリストが作成されます。
  - 数字 (`42`)
  - 文字列 (`"hello"`)
  - ブール値 (`#t`)
  - 別のリスト (`(1 2)`)
  - 式の結果 (`(+ 3 4)`、`7` と評価される)

結果: **`(42 "hello" #t (1 2) 7)`**

---

これらの例は、Scheme のリストの多用途性を示し、リストがデータの整理と操作のための強力なツールとなることを示しています。

### リストの構築

`cons` プロシージャは、要素を既存のリストと組み合わせて新しいリストを構築するために使用されます。

```scheme
(cons new-element existing-list)
```

#### 例

```scheme
(cons 0 (list 1 2 3))
```

- `0` をリスト `(1 2 3)` の先頭に追加します。

結果: **`(0 1 2 3)`**

---

### リストのチェック

`list?` プロシージャは、指定された値がリストであるかどうかを確認します。

```scheme
(list? value)
```

#### 例: リスト?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

結果:

- `(list? (list 1 2 3))` は `#t` (true) を返します
- `(list? 42)` は `#f` (偽) を返します

---

### リストの操作

Scheme には、リストを操作するための次のような組み込みプロシージャがいくつか用意されています。

- `length`: リスト内の要素の数を返します。
- `append`: 2 つ以上のリストを 1 つに結合します。
- `reverse`: 要素を逆順にした新しいリストを返します。

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

結果:

- `(length (list 1 2 3))` は `3` を返します
- `(append (list 1 2) (list 3 4))` は `(1 2 3 4)` を返します
- `(reverse (list 1 2 3))` は `(3 2 1)` を返します#### `list-ref` を使用する

`list-ref` プロシージャは、リストの指定されたインデックス (0 から始まるインデックス) にある要素を取得します。

```scheme
(list-ref lst index)
```

- **`lst`**: 要素を取得するリスト。
- **`index`**: どの要素を返すかを示す 0 から始まるインデックス。

##### 例: リスト参照

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

結果: `30`

---

### ネストされたリスト

Scheme のリストには他のリストを要素として含めることができ、入れ子構造を作成します。

#### 例: ネストされたリストの作成

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- 3 つの要素のリストを作成します。各要素はそれ自体がリストです。

結果: **`((1 2) (3 4) (5))`**

---

#### ネストされたデータへのアクセス

ネストされたリスト内の要素にアクセスするには、`car` と `cdr` の組み合わせを使用して構造内を移動できます。

#### 例: 要素へのアクセス

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### 説明

1. **`car nested-list`**:
   - `nested-list` の最初の要素 (`(1 2)`) を取得します。

2. **`car (car nested-list)`**:
   - `(1 2)` の最初の要素 (`1`) を取得します。

3. **`cdr (car nested-list)`**:
   - `(1 2)` の残り、つまり `(2)` を取得します。

4. **`car (cdr (car nested-list))`**:
   - `(2)` の最初の要素 (`2`) を取得します。

---

#### 例: 他のサブリストからの要素へのアクセス

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

このアプローチにより、ネストされたリスト内の特定の要素に体系的に移動してアクセスできるため、階層データを操作するための強力な柔軟性が得られます。

### 概要

- Scheme の **リスト** は多用途で不可欠なデータ構造です。
- `list` を使用してリストを作成し、`car` および `cdr` を使用して要素にアクセスし、`cons` を使用してリストを構築します。
- `length`、`append`、`reverse`、`list-ref` などの組み込みプロシージャにより、リスト操作が簡単かつ効率的になります。
- リストはネストできるため、高度なユースケース向けに複雑なデータ構造が可能になります。