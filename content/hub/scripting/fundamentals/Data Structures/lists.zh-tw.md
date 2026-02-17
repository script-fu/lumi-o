---
title: "清單"
type: docs
weight: 4
---
在Scheme中，**列表**是用於將值分組的基本資料結構。列表是元素的有序集合，其中每個元素可以是任何類型，包括另一個列表。列表在Scheme中廣泛用於資料儲存和程式結構。

### 範例 1：簡單列表

```scheme
(list 1 2 3)
```

- 建立包含三個元素的清單：`1`、`2` 和 `3`。

結果：**`(1 2 3)`**

---

#### 存取清單元素

使用 `car` 和 `cdr` 程序存取清單中的元素：

- `car` 檢索清單的第一個元素。
- `cdr` 檢索清單的其餘部分（除第一個元素之外的所有內容）。

#### 範例

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

結果：

- `(car my-list)` 返回`1`
- `(cdr my-list)` 返回`(2 3)`

---

#### 簡單遞歸：遍歷列表

透過對列表的`cdr`遞歸呼叫`car`，可以逐一處理每個元素，直到遍歷完列表。這構成了許多列表處理演算法的基礎。

#### 範例：列印清單的每個元素

這是一個簡單的遞歸函數，用於列印清單中的每個元素：

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **基本情況：** 若清單為空 (`null? lst`)，則停止遞迴。
- **遞歸情況：** 列印第一個元素 (`car lst`)，然後呼叫清單其餘部分 (`cdr lst`) 上的函數。

#### 用法範例

```scheme
(print-elements (list 1 2 3))
```

輸出：

-`"1"`
- `"2"`
- `"3"`

結果：“完成”

---

#### 它是如何運作的

1. 此函數使用 `car` 檢索列表的第一個元素並對其進行處理。
2. 然後它用列表的其餘部分呼叫自身 (`cdr`)。
3. 重複此過程，直到清單為空 (`null? lst`)。

---

### 範例 2：混合型別

列表可以包含不同類型的元素，包括字串、布林值、數字、其他列表，甚至是表達式的結果：

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- 這將建立一個清單：
  - 一個數字 (`42`)
  - 字串 (`"hello"`)
  - 布林值 (`#t`)
  - 另一個清單 (`(1 2)`)
  - 表達式的結果（`(+ 3 4)`，其計算結果為`7`）

結果：**`(42 "hello" #t (1 2) 7)`**

---

這些範例展示了Scheme中清單的多功能性，使它們成為組織和操作資料的強大工具。

### 建置列表

`cons` 流程用於透過將元素與現有清單組合來建構新清單。

```scheme
(cons new-element existing-list)
```

#### 範例

```scheme
(cons 0 (list 1 2 3))
```

- 將 `0` 加到列表`(1 2 3)` 的開頭。

結果：**`(0 1 2 3)`**

---

### 檢查列表

`list?` 過程檢查給定值是否為列表。

```scheme
(list? value)
```

#### 範例：列表？

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

結果：

- `(list? (list 1 2 3))` 返回`#t`（真）
- `(list? 42)` 返回`#f` (假)

---

### 清單操作

Scheme 提供了幾個用於處理清單的內建流程，包括：

- `length`：傳回清單中的元素數量。
- `append`：將兩個或多個清單合併為一個。
- `reverse`：傳回一個新列表，其中元素以相反順序排列。

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

結果：

- `(length (list 1 2 3))` 返回`3`
- `(append (list 1 2) (list 3 4))` 返回`(1 2 3 4)`
- `(reverse (list 1 2 3))` 返回`(3 2 1)`#### 使用`list-ref`

`list-ref` 程序檢索清單指定索引處的元素（從零開始的索引）。

```scheme
(list-ref lst index)
```

- **`lst`**：從中檢索元素的清單。
- **`index`**：從零開始的索引，指示要傳回哪個元素。

##### 範例：清單引用

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

結果：`30`

---

### 嵌套列表

Scheme 中的清單可以包含其他清單作為元素，從而建立嵌套結構。

#### 範例：建立巢狀列表

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- 建立一個包含三個元素的列表，每個元素本身就是一個列表。

結果：**`((1 2) (3 4) (5))`**

---

#### 存取嵌套數據

若要存取巢狀清單中的元素，您可以使用 `car` 和 `cdr` 的組合來瀏覽結構。

#### 範例：存取元素

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### 解釋

1. **`car nested-list`**：
   - 檢索`nested-list` 的第一個元素，即`(1 2)`。

2. **`car (car nested-list)`**：
   - 檢索`(1 2)` 的第一個元素，即`1`。

3. **`cdr (car nested-list)`**：
   - 檢索`(1 2)` 的其餘部分，即`(2)`。

4. **`car (cdr (car nested-list))`**：
   - 檢索`(2)` 的第一個元素，即`2`。

---

#### 範例：存取其他子清單中的元素

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

這種方法可讓您有系統地導航和存取嵌套清單中的特定元素，為處理分層資料提供強大的靈活性。

### 總結

-Scheme 中的**列表**是通用且重要的資料結構。
- 使用`list` 建立列表，使用`car` 和`cdr` 存取元素，使用`cons` 建立列表。
- `length`、`append`、`reverse` 和 `list-ref` 等內建程式可讓清單操作簡單且有效率。
- 清單可以嵌套，從而為高級用例啟用複雜的資料結構。