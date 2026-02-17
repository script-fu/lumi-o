---
title: "符號"
type: docs
weight: 6
---
符號是Scheme中的核心資料類型之一，代表唯一的、不可變的識別碼。它們主要用作程式中的鍵、標記或占位符，這使得它們對於編寫乾淨且富有表現力的程式碼至關重要。

Scheme 中的符號類似於字串，但不同之處在於符號是**唯一**和**原子**。這意味著具有相同名稱的兩個符號保證是同一個對象，從而允許快速相等檢查和資料結構的有效使用。

### 語法

符號被寫成字元序列：

- 以字母開頭，後跟字母、數字或特殊字符，例如`-`、`+` 或`*`。
- 預設情況下，符號區分大小寫。

範例：

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## 建立符號

符號通常是使用 **quote** 運算子 (`'`) 創建的，它告訴Scheme 將名稱視為符號，而不是將其作為變數或函數進行計算。

### 範例

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

您也可以使用 `string->symbol` 程序以程式設計方式建立符號，該程序將字串轉換為符號。

```scheme
(string->symbol "dynamic-symbol")
```

**結果**：`'dynamic-symbol`


## 比較符號

由於符號是唯一的，因此您可以使用 `eq?` 有效地比較它們。

### 範例

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

這使得符號非常適合用作資料結構中的鍵或代碼中的標記。

## 使用符號

符號在方案中常用於：

1. **關聯列表中的鍵：**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **程式碼中的識別碼：**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## 使用符號的過程

Scheme 提供了使用符號的內建流程：

|程序 |描述 |
|--------------------------------|------------------------------------------------------------------------------------------------|
| **`symbol?`** |檢查一個物件是否是一個符號。                                            |
| **`eq?`** |比較兩個符號的同一性（快速比較）。                       |
| **`symbol->string`** |將符號轉換為字串（對於顯示或偵錯有用）。          |
| **`string->symbol`** |將字串轉換為符號（對於動態建立識別碼很有用）。 |

### 範例

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## 總結

符號是在Scheme中表示標識符、鍵和標記的一種輕量級、高效的方式。它們的不變性和快速身份檢查使它們成為許多程式設計任務的理想選擇。了解如何有效地使用符號將增強您編寫簡潔且富有表現力的Scheme程式碼的能力。