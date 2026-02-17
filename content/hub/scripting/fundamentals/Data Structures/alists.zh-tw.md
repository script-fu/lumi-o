---
title: "協會列表（Alists）"
type: docs
weight: 6
---
**關聯清單**（或**alist**）是Scheme中的基本資料結構，用於表示鍵值對的集合。它被實現為一個對的列表，其中每對將一個鍵（通常是一個符號）與一個值相關聯。清單簡單、靈活，非常適合中小型資料集。

### 關聯清單的結構

alist 是一個列表，其中每個元素都是一個**對**（使用 `cons` 構造）。每對包括：

- **鍵**：第一個元素（通常是符號）。
- **Value**：第二個元素，可以是任何資料型態。

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **金鑰**：`'name`、`'age`、`'city`
- **值**：`"Alice"`、`30`、`"Paris"`
- **結構**：對列表：
  `((name . "Alice") (age . 30) (city . "Paris"))`

### 建立一個列表

您可以透過手動建立對或使用 `cons` 以程式設計方式建立來建立清單。

#### 使用單引號 (`'`)

單引號 (`'`) 是 **quoting** 的簡寫，它會阻止Scheme 計算表達式。這使得它非常適合創建所有鍵和值都被硬編碼的靜態列表。

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**結果**：
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### 使用反引號 (`` ` ``) and Comma (`,`)

反引號 (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`) 運算子。這對於建立在運行時計算鍵或值的清單很有用。

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**結果**：
`((name . "Alice") (age . 30) (city . "Paris"))`

### 範例比較

使用 `'` 的靜態清單：

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

使用 `` ` `` and `,` 的動態清單：

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### 存取清單中的數據

若要從清單中檢索值，您可以使用 `assoc` 函數，該函數透過鍵找到一對。

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### 提取值

使用 `assoc` 檢索一對後，請使用 `cdr` 提取該值：

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### 主要功能總結

- **單引號 (`'`)**：建立一個靜態列表，其中所有元素都是文字資料。
- **反引號 (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`)。
- **點符號 (`.`)**：用於建構對，將鍵與列表中的值關聯起來。