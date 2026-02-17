---
title: "いつ"
type: docs
weight: 5
---
Scheme では、`if` はエレガントで多用途ですが、明示的な `else` なしで使用すると混乱を招く可能性があります。これは、`false` の場合に代替アクションを行わずに、条件が true の場合にのみコードの単一分岐を実行することを目的としている場合に特に当てはまります。このようなシナリオでは、`when` 構造がより明確で簡潔な代替手段を提供します。

`when` の基本的な形式は次のようになります。

```scheme
(when test-is-true
  do-this
  do-that)
```

- `test` が true (`#t`) と評価された場合、`when` 構造体の本体内のすべての式が順番に実行されます。
- `test` が false (`#f`) と評価された場合、何も起こらず、値は返されません。

### 例

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### `if` と `when` の対照

`if` と `when` の違いをよりよく理解するために、両方を一緒に使用する次の例を考えてみましょう。

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### 説明:

1. **`if` 条件**:
   - テスト `(= 0 1)` は、0 が 1 に等しいかどうかをチェックします。
   - false(`#f`)なので、`if`の`else`ブランチが実行されます。

2. **`else` ブランチの `when` コンストラクト**:
   - `when` テスト `(< 0 1)` は、0 が 1 より小さいかどうかをチェックします。
   - これは true (`#t`) なので、`when` の本文内のすべての式が順番に実行されます。
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### ここで `when` を使用する理由

- 別の `if` の代わりに `when` を使用すると、条件の明示的な `else` 分岐が必要ない場合にロジックが簡素化されます。
- `when` は、真のブランチのみが関連することを明確にし、潜在的な混乱を軽減します。

### 概要

- true と false の両方の分岐が必要な場合は、`if` を使用します。
- 実際のケースに分岐が 1 つしかない場合、特に複数のアクションを実行する必要がある場合は、`when` を使用します。
- `if` と `when` を組み合わせると、より複雑な条件を明確かつ簡潔に構造化するのに役立ちます。