---
title: "関連リスト (アリスト)"
type: docs
weight: 6
---
**アソシエーション リスト** (または **アリスト**) は、キーと値のペアのコレクションを表すために使用される Scheme の基本的なデータ構造です。これはペアのリストとして実装され、各ペアがキー (通常はシンボル) と値を関連付けます。リストはシンプルかつ柔軟で、小規模から中規模のデータセットに適しています。

### 関連リストの構造

alist は、各要素が **ペア** (`cons` で構築される) であるリストです。各ペアは次のもので構成されます。

- **キー**: 最初の要素 (通常はシンボル)。
- **値**: 2 番目の要素。任意のデータ型を使用できます。

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **キー**: `'name`、`'age`、`'city`
- **値**: `"Alice"`、`30`、`"Paris"`
- **構造**: ペアのリスト:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### エイリストの作成

手動でペアを構築するか、`cons` を使用してプログラムでペアを構築することで、アリスリストを作成できます。

#### 一重引用符の使用 (`'`)

一重引用符 (`'`) は **引用** の短縮形であり、Scheme による式の評価を防ぎます。これは、すべてのキーと値がハードコーディングされた静的リストの作成に最適です。

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**結果**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### 逆引用符の使用 (`` ` ``) and Comma (`,`)

逆引用符 (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`) 演算子。これは、キーまたは値が実行時に計算されるリストを作成する場合に便利です。

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**結果**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### 比較例

`'` を使用した静的 alist:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

`` ` `` and `,` を使用した動的 alist:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Alist 内のデータへのアクセス

アリリストから値を取得するには、キーでペアを検索する `assoc` 関数を使用できます。

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### 値の抽出

`assoc` を使用してペアを取得したら、`cdr` を使用して値を抽出します。

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### 主な機能の概要

- **一重引用符 (`'`)**: すべての要素がリテラル データである静的 alist を作成します。
- **バッククォート (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`)。
- **ドット表記 (`.`)**: ペアを構築し、キーを alist 内の値に関連付けるために使用されます。