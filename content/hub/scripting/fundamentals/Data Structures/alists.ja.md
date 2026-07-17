---
title: "連想リスト（Alists）"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
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
;; alist を手動で定義
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; プログラムで新しいペアを追加
(define updated-alist (cons '(country . "France") alist))
```

**結果**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### 逆引用符 (`` ` ``) とカンマ (`,`) の使用

逆引用符 (`` ` ``) はシングルクォートに似ていますが、コンマ (`,`) を使って評価済み式を動的に挿入できる演算子。これは、キーまたは値が実行時に計算されるリストを作成する場合に便利です。

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
(assoc 'name alist)   ; (name . "Alice") を返す
(assoc 'country alist) ; #f を返す（キーが見つからない）
```

### 値の抽出

`assoc` を使用してペアを取得したら、`cdr` を使用して値を抽出します。

```scheme
(cdr (assoc 'name alist))   ; "Alice" を返す
```

### 主な機能の概要

- **一重引用符 (`'`)**: すべての要素がリテラル データである静的 alist を作成します。
- **バッククォート (`` ` ``)**: 静的要素と評価済み式を組み合わせて alist を動的に作成できます（`,` を使用）。
- **ドット表記 (`.`)**: ペアを構築し、キーを alist 内の値に関連付けるために使用されます。