---
title: "向量"
type: docs
weight: 5
---
在Scheme中，向量是另一個用於將值分組的基本資料結構。與列表不同，向量是固定大小的索引元素集合，提供更快的隨機存取和更新。向量中的每個元素可以是任何類型，包括另一個向量。向量使用 # 後面跟著括號來表示。 `#(1 2 3)`

雖然向量和列表可能看起來相似，但它們在方案編程中具有不同的用途：

- 列表更常用於遞歸操作和動態結構，因為它們的連結節點實現允許透過遞歸分解有效地操縱它們的開始和遍歷。

- 另一方面，向量針對需要隨機存取元素或更新特定索引的場景進行了最佳化，使它們更適合查找表、固定大小配置或效能關鍵的索引操作等用例。

從本質上講，清單是遞歸演算法和動態大小資料的自然選擇，而當固定大小或索引存取模式至關重要時，向量就會大放異彩。

### 簡單向量

```scheme
(vector 1 2 3)
```

- 建立包含三個元素的向量：`1`、`2` 和`3`。

結果：**`#(1 2 3)`**

#### 存取向量元素

使用 `vector-ref` 程序存取向量中的元素，它會擷取指定索引處的元素（從`0` 開始）。

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### 迭代：處理向量中的每個元素

您可以使用循環或遞歸來迭代向量。 Scheme 提供`vector-length` 來決定向量的大小。這是一個列印向量中每個元素的簡單循環：

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **基本情況：** 如果索引 `i` 達到向量的長度，則停止循環。
- **遞迴情況：** 列印索引 `i` 處的元素，然後遞增 `i`。

#### 用法範例

```scheme
(print-elements (vector 1 2 3))
```

結果：

- `"1"`
- `"2"`
- `"3"`

結果：“完成”

### 混合向量

向量可以包含不同類型的元素，包括字串、布林值、數字、其他向量，甚至是表達式的結果：

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

這將創建一個向量：
  - 一個數字 (`42`)
  - 字串 (`"hello"`)
  - 布林值 (`#t`)
  - 另一個向量 (`#(1 2)`)
  - 表達式的結果（`(+ 3 4)`，其計算結果為`7`）

結果：**`#(42 "hello" #t #(1 2) 7)`**

### 建構向量

向量是使用`vector`建立的，或使用`make-vector`建立具有初始值的固定大小的向量。

```scheme
(make-vector 5 0)
```

建立大小為`5` 的向量，所有元素都初始化為`0`。

結果：`#(0 0 0 0 0)`

### 更新向量

`vector-set!` 程序更新向量中指定索引處的元素。

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

結果：`#(1 42 3)`

### 檢查向量

`vector?` 過程檢查給定值是否為向量。

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

結果：

- `(vector? (vector 1 2 3))` 返回`#t`（真）
- `(vector? 42)` 返回`#f` (假)

### 向量與引用傳遞行為在Scheme中，向量是可變的並且透過引用傳遞。這意味著當您將向量傳遞給函數時，該函數可以直接修改原始向量。對函數內部向量所做的任何更改也將反映在函數外部。此行為對於跨多個函數有效共享和更新資料很有用，但也需要謹慎以避免意外的副作用。

#### 範例：修改函數中的向量

下面的範例示範如何透過引用傳遞向量並進行修改：

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

結果：`#(10 99 30)`

#### 逐步說明

1. **建立向量：** `my-vector` 使用值 `10`、`20` 和 `30` 初始化。
2. **傳遞到函數：** `my-vector` 連同要更新的索引和新值一起傳遞到 `modify-vector`。
3. **在函數中修改：** `vector-set!` 程序直接更新原始向量中指定索引處的值。
4. **反映變更：** 由於向量是透過引用傳遞的，因此函數內所做的變更將反映在原始向量中。

#### 引用傳遞的意義

- **效能：** 透過引用傳遞向量非常高效，因為它避免了複製大型結構。
- **副作用：** 跨函數共享向量時要小心，以避免對共享資料進行意外修改。

### 向量運算

Scheme 提供了幾個用於處理向量的內建流程，包括：

- `vector-length`：傳迴向量中的元素數量。
- `vector->list`：將向量轉換為列表。
- `list->vector`：將列表轉換為向量。

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

結果：

- `(vector-length (vector 1 2 3))` 返回`3`
- `(vector->list (vector 1 2 3))` 返回`(1 2 3)`
- `(list->vector (list 1 2 3))` 返回`#(1 2 3)`

### 巢狀向量

Scheme 中的向量可以包含其他向量作為元素，從而建立巢狀結構。

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

創建一個由三個元素組成的向量，每個元素本身就是一個向量。

結果：**`#(#(1 2) #(3 4) #(5))`**

#### 存取嵌套數據

若要存取巢狀向量內的元素，請多次使用 `vector-ref` 來瀏覽結構。

#### 範例：存取元素

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### 總結

-Scheme 中的**向量**是固定大小的索引資料結構。
- 使用`vector` 建立向量，使用 `vector-ref` 存取元素，使用`vector-set!` 更新元素。
- `vector-length`、`vector->list` 和`list->vector` 等內建過程可實現靈活的操作。
- 巢狀向量允許複雜的分層資料結構。