---
title: "什麼時候"
type: docs
weight: 5
---
在Scheme中，雖然`if`是優雅且通用的，但在沒有顯式`else`的情況下使用時可能會變得混亂。當目的是僅在條件為真時執行單一程式碼分支，而對於 `false` 情況沒有替代操作時，尤其如此。在這種情況下，`when` 構造提供了更清晰、更簡潔的替代方案。

`when` 的基本形式如下：

```scheme
(when test-is-true
  do-this
  do-that)
```

- 如果 `test` 的計算結果為 true (`#t`)，則 `when` 構造體中的所有表達式將依序執行。
- 如果 `test` 的計算結果為 false (`#f`)，則不會發生任何情況，並且不會傳回任何值。

### 範例

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### 對比`if` 和`when`

為了更好地理解 `if` 和 `when` 之間的區別，請考慮以下兩者一起使用的範例：

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### 說明：

1. **`if` 條件**：
   - 測試`(= 0 1)` 檢查 0 是否等於 1。
   - 由於這是 false (`#f`)，因此執行`if` 的`else` 分支。

2. **`else` 分支中的 `when` 構造**：
   - `when` 測試 `(< 0 1)` 檢查 0 是否小於 1。
   - 由於這是 true (`#t`)，`when` 主體內的所有表達式都會依序執行：
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### 為什麼在這裡使用`when`？

- 當條件不需要明確`else` 分支時，使用`when` 取代另一個`if` 可以簡化邏輯。
- `when` 明確表示只有真正的分支才是相關的，從而減少了潛在的混亂。

### 總結

- 當您需要 true 和 false 分支時，請使用 `if`。
- 當真實情況只有一個分支時，尤其是需要執行多個操作時，請使用`when`。
- 組合 `if` 和 `when` 可以幫助清晰簡潔地建構更複雜的條件。