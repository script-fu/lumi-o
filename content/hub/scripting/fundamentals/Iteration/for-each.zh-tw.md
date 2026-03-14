---
title: "對於每個"
type: docs
weight: 5
---
Scheme 中的 `for-each` 函數用於將過程應用於列表（或多個列表）的每個元素。與 `map` 傳回包含結果的新清單不同，`for-each` 用於其**副作用**，例如列印或更新變數。

`for-each` 最簡單的形式如下：

```scheme
(for-each procedure list)
```

- **過程**：應用於清單中每個元素的函數。
- **清單**：將處理其元素的清單。

---

### 範例：列印列表

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- 此處，函數`print-item` 應用於列表`(1 2 3 4)` 的每個元素。
- 這會導致按順序列印每個數字。

**輸出**：`1 2 3 4`

---

### 它是如何運作的

1. **迭代每個元素**：
   - 所提供的過程會依序對清單中的每個元素執行。

2. **產生副作用**：
   - 常見的副作用包括列印、記錄或修改外部變數。與`map` 不同，`for-each` 不傳回新清單。

---

#### 範例：與多個清單一起使用

如果提供了多個列表，`for-each` 會處理每個列表中的對應元素。

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- 函數`sum-and-print` 對兩個列表中的對應元素求和並列印結果。

**輸出**：`5 7 9`

---

### 總結

- `for-each` 函數對於對清單的每個元素執行副作用非常有用。
- 與`map` 不同，`for-each` 不會產生新清單－它只專注於過程的副作用。
- 它可以同時處理多個列表，將過程應用於相應的元素。

透過使用`for-each`，當目標是執行操作而不是轉換資料時，您可以有效地處理清單。