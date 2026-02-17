---
title: "可變參數函數"
type: docs
weight: 2
---
Scheme 中的**可變參數函數**是接受可變數量參數的函數。這些函數用途廣泛，可讓您建立靈活且可重複使用的程式碼。在函數式程式設計中，可變參數函數簡化了需要處理任意數量輸入的操作，例如對數字列表求和或連接字串。

可變參數函數在以下情況下特別有用：

- 參數的數量無法預先決定。
- 您需要對動態輸入清單套用相同的操作。
- 編寫用於資料聚合或轉換的實用程式。

### 可變參數函數的語法

可變參數函數是在最後一個參數名稱之前使用 `.` 符號定義的。最後一個參數將所有剩餘的參數收集到一個清單中。

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** 函數接受的任何必要的固定參數。
- **`variadic-parameter`:** 前面帶有 `.` 的特殊參數，用於將其他參數收集為列表。
- **`body-expression`:** 呼叫函數時執行的邏輯。

### 可變參數函數的範例

#### 基本可變參數函數

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **解釋**：
  - `numbers` 將所有參數收集到清單中。
  - `apply` 將 `+` 函數套用至列表的所有元素。

**用法**：
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### 具有固定參數的可變參數函數

您可以將固定參數與可變參數結合以建立更靈活的函數。

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **解釋**：
  - `prefix` 是固定參數。
  - `names` 將剩餘參數收集到清單中。
  - 每個名稱都使用 `map` 和 `lambda` 以給定字串為前綴。

**用法**：
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### 結合固定邏輯和可變邏輯

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **解釋**：
  - `collection-name` 是固定參數。
  - `items` 將其他參數收集到清單中。
  - 此函數將集合名稱和項目連接成單一字串。

**用法**：
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### 進階用例

#### 處理任意輸入

可變參數函數擅長處理任意資料。以下是僅對正數求和的範例：

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- 在求和之前過濾掉非正數。

**用法**：
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### 具有遞歸邏輯的可變參數函數

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **解釋**：
  - `first` 處理第一個參數。
  - `rest` 將剩餘參數收集到清單中。
  - 遞歸計算最大值。

**用法**：
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### 可變參數函數的好處

- **靈活性：** 它們處理各種輸入情況。
- **簡潔性：**減少對多個重載函數的需求。
- **動態操作：** 在事先不知道參數計數的情況下啟用運行時資料處理。

### 何時使用可變參數函數

在以下情況下使用可變參數函數：

- 此函數需要處理未知數量的參數。
- 單一操作適用於所有輸入（例如求和、連接或對應）。
- 使用動態參數簡化高階邏輯。

在以下情況下避免使用可變函數：

- 輸入驗證或類型檢查很複雜。
- 固定參數足以滿足所需的邏輯。
- 由於操作過於複雜，可讀性受到影響。

＃＃＃ 結論Scheme 中的可變參數函數提供了處理動態輸入的強大機制。透過了解它們的語法和用法，您可以建立靈活且強大的腳本，以適應各種場景。與高階函數結合，可變參數函數使您的程式碼更加簡潔和富有表現力。