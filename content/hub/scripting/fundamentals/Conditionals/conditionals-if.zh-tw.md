---
title: "如果"
type: docs
weight: 4
---
以最簡單的形式，Scheme 中的 `if` 條件評估測試，並根據結果執行兩個可能的程式碼區塊之一。最簡單的形式如下：

```scheme
(if test-is-true
  do-this)
```

- 如果 `test` 計算結果為 true (`#t`)，則執行後續程式碼中的 **程式碼區塊**。該區塊可以傳回一個值或執行其他操作，例如分配變數或列印輸出。

### 範例

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- 在本例中，`test` 是`(< 0 1)`（檢查 0 是否小於 1）。
- 由於測試結果為 true (`#t`)，因此執行程式碼區塊`(lumi-message "True!")`，列印`"True!"`。

### 新增 Else 條件：`if-else`

當將 `if` 條件與替代代碼區塊（`else` 情況）一起使用時，結構如下所示：

```scheme
(if test
  do-this
  else-do-this)
```

- 如果 `test` 計算結果為 true (`#t`)，則執行 **後續** 程式碼區塊。
- 如果 `test` 的計算結果為 false (`#f`)，則執行 **替代** 程式碼區塊。

```scheme
(if test
  consequent
  alternative)
```

### 它是如何運作的

1. **測試表達**：
   - 先評估`test` 表達式。

2. **基於測試的結果**：
   - 如果 `test` 計算結果為 true (`#t`)，執行 **後續程式碼區塊**。
   - 如果 `test` 的計算結果為 false (`#f`)，則執行 **替代程式碼區塊**。

`consequent` 和`alternative` 程式碼區塊都可以執行任何有效的Scheme 操作，包括傳回值、修改變數或執行過程。

### 範例

#### 範例 1：傳回值

```scheme
(if (< 0 1)
  1
  0)
```

- 這裡，`test`是`(< 0 1)`（檢查0是否小於1）。
- 由於測試評估結果為 true (`#t`)，因此將執行 **後續** 區塊 (`1`) 並傳回其值。

結果：**1**

#### 範例 2：評估開始區塊

如果您需要在條件為 true 或 false 時執行多個操作，可以使用 `begin` 或 `let` 將它們組合在一起。

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- 在此範例中，`test` 是`(= 0 1)`（檢查 0 是否等於 1）。
- 由於測試結果為 false (`#f`)，因此執行 **alternative** 區塊：
  - 首先，它印出`"False condition met, calculating..."`。
  - 然後，它計算`(* 3 4)`並返回`12`。

結果：**列印「滿足錯誤條件，計算...」並傳回 12。 **

#### 範例 3：評估 let 語句

使用 `let` 允許我們在程式碼區塊中宣告局部範圍變數。

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- 在此範例中，`test` 是 `(= 1 1)`（檢查 1 是否等於 1）。
- 由於測試結果為 true (`#t`)，因此執行 **consequence** 區塊：
  - 首先，它印出`"True condition met, calculating..."`。
  - 然後，它計算`(* -1 10)`並返回`-10`。

結果：**列印“滿足真實條件，計算...”並返回 -10。 **

### 總結- `if` 條件是Scheme 中用於評估測試和執行相應程式碼區塊的強大工具。
- 它可以處理簡單的表達式和傳回值、修改變數或執行副作用的複雜程式碼區塊。
- 請記住：如果沒有明確的 `else` 區塊，則 `if` 僅在測試為真時評估並執行 **結果**。否則，它會評估並執行**替代方案**。