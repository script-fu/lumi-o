---
title: "條件"
type: docs
weight: 5
---
在Scheme中，`cond`條件用於根據多個測試選擇要執行的幾個可能的程式碼區塊之一。它就像一個多分支`if`，按順序檢查每個分支，直到找到匹配項。

### 語法

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- 每個測試均按其編寫順序進行評估。
- 當測試計算結果為 true (`#t`) 時，執行其對應的 **結果**，並且 `cond` 表達式停止評估進一步的測試。
- `else` 子句是可選的，如果沒有一個測試評估為 true，則用作後備。

### 它是如何運作的

1. **測試每個條件**：
   - `cond` 依測試列出的順序評估測試。

2. **執行匹配結果**：
   - 當找到第一個評估為 true 的測試 (`#t`) 時，將執行其**結果**。
   - 如果沒有測試評估為 true 並且存在 `else` 子句，則執行 **後備結果**。

### 範例

#### 範例 1：單一表達式結果

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- 第一個測試 `(< 3 2)` 的計算結果為 false (`#f`)。
- 第二個測試 `(= 3 3)` 的計算結果為 true (`#t`)，因此傳回 `"This will run"`。
- `else` 子句未執行，因為已找到符合項目。

結果：**“這將運行”**

#### 範例 2：使用 `begin` 的多個操作

當後續涉及多個動作時，使用`begin`將它們分組：

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

- 第一個測試 `(< 5 3)` 的計算結果為 false (`#f`)。
- 第二個測試 `(> 5 3)` 計算結果為 true (`#t`)：
  - 它印出`"Condition met"`。
  - 然後計算`(* 5 5)`並返回`25`。

結果：**列印“滿足條件”並返回 25。 **

#### 範例 3：在後續結果中使用 `let` 區塊

當您需要引入局部變數時，請使用 `let` 區塊：

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

- 第一個測試`(< 0 -1)` 是錯誤的。
- 第二個測試 `(> 0 -1)` 為真，因此：
  - 執行`let` 區塊，將`y` 綁定到`20`。
  - 它列印`"Positive condition met"`。
  - 然後計算`(+ y y)`並返回`40`。

結果：**列印“滿足肯定條件”並返回 40。 **

#### 範例 4：使用 `else` 回退

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- 前兩個測試的評估結果都不為 true。
-執行 `else` 子句並回傳`"Fallback value"`。

結果：**「後備值」**

### 總結

- 使用`cond` 以清晰簡潔的方式處理多個條件。
- 結果可以是單一表達式或使用 `begin` 的分組操作。
- 在結果中使用`let`來宣告用於計算的局部變數。
- 始終包含 `else` 子句作為處理意外情況的後備措施。

這種靈活性使 `cond` 成為處理複雜分支邏輯的強大且可讀的工具。