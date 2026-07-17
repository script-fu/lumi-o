---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
Scheme では、`if` はエレガントで多用途ですが、明示的な `else` なしで使用すると混乱を招く可能性があります。これは、`偽` の場合に代替アクションを行わずに、条件が 真 の場合にのみコードの単一分岐を実行することを目的としている場合に特に当てはまります。このようなシナリオでは、`when` 構造がより明確で簡潔な代替手段を提供します。

`when` の基本的な形式は次のようになります。

```scheme
(when test-is-true
  do-this
  do-that)
```

- `test` が 真 (`#t`) と評価された場合、`when` 構造体の本体内のすべての式が順番に実行されます。
- `test` が 偽 (`#f`) と評価された場合、何も起こらず、値は返されません。

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
   - 偽(`#f`)なので、`if`の`else`ブランチが実行されます。

2. **`else` ブランチの `when` コンストラクト**:
   - `when` テスト `(< 0 1)` は、0 が 1 より小さいかどうかをチェックします。
   - これは 真 (`#t`) なので、`when` の本文内のすべての式が順番に実行されます。
     - まず `"The 'when' condition is true!"` を出力します。
     - 次に `"Executing multiple actions within 'when'."` を出力します。

#### ここで `when` を使用する理由

- 別の `if` の代わりに `when` を使用すると、条件の明示的な `else` 分岐が必要ない場合にロジックが簡素化されます。
- `when` は、真のブランチのみが関連することを明確にし、潜在的な混乱を軽減します。

### 概要

- 真 と 偽 の両方の分岐が必要な場合は、`if` を使用します。
- 実際のケースに分岐が 1 つしかない場合、特に複数のアクションを実行する必要がある場合は、`when` を使用します。
- `if` と `when` を組み合わせると、より複雑な条件を明確かつ簡潔に構造化するのに役立ちます。