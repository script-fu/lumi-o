---
title: "条件"
type: docs
weight: 5
---
Scheme では、`cond` 条件は、複数のテストに基づいて、実行する可能性のあるコード ブロックの 1 つを選択するために使用されます。これはマルチブランチ `if` に似ており、一致するものが見つかるまで各ブランチが順番にチェックされます。

### 構文

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- 各テストは、書かれた順序で評価されます。
- テストが true (`#t`) と評価されると、対応する **結果** が実行され、`cond` 式はそれ以降のテストの評価を停止します。
- `else` 句はオプションであり、どのテストも true と評価されない場合のフォールバックとして機能します。

### 仕組み

1. **各条件をテスト**:
   - `cond` は、リストされている順序でテストを評価します。

2. **一致する結果を実行**:
   - true と評価される最初のテスト (`#t`) が見つかると、その **結果** が実行されます。
   - true と評価されるテストがなく、`else` 句がある場合、**fallback-consequent** が実行されます。

### 例

#### 例 1: 単一の式の帰結

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- 最初のテスト `(< 3 2)` は false (`#f`) と評価されます。
- 2 番目のテスト `(= 3 3)` は true (`#t`) と評価されるため、`"This will run"` が返されます。
- 一致がすでに見つかっているため、`else` 句は実行されません。

結果: **「これは実行されます」**

#### 例 2: `begin` を使用した複数のアクション

結果に複数のアクションが含まれる場合は、`begin` を使用してそれらをグループ化します。

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- 最初のテスト `(< 5 3)` は false (`#f`) と評価されます。
- 2 番目のテスト `(> 5 3)` は true (`#t`) と評価されます。
  - `"Condition met"` と印刷されます。
  - 次に、`(* 5 5)` を計算し、`25` を返します。

結果: **「条件が満たされました」を出力し、25 を返します。**

#### 例 3: 後件での `let` ブロックの使用

ローカル変数を導入する必要がある場合は、`let` ブロックを使用します。

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- 最初のテスト `(< 0 -1)` は false です。
- 2 番目のテスト `(> 0 -1)` は true なので、次のようになります。
  - `let` ブロックが実行され、`y` を `20` にバインドします。
  - `"Positive condition met"` と印刷されます。
  - 次に、`(+ y y)` を計算し、`40` を返します。

結果: **「肯定的な条件が満たされました」を出力し、40 を返します。**

#### 例 4: `else` によるフォールバック

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- 最初の 2 つのテストはいずれも true と評価されません。
- `else` 句が実行され、`"Fallback value"` が返されます。

結果: **「フォールバック値」**

### 概要

- 複数の条件を明確かつ簡潔に処理するには、`cond` を使用します。
- 結果は、単一の式または `begin` を使用したグループ化されたアクションにすることができます。
- 結果部で `let` を使用して、計算用のローカル変数を宣言します。
- 予期しないケースに対処するためのフォールバックとして、常に `else` 句を含めます。

この柔軟性により、`cond` は、複雑な分岐ロジックを処理するための強力で読みやすいツールになります。