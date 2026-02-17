---
title: "拉姆達函數"
type: docs
weight: 1
---
Scheme 中的 **Lambda 函數** 是匿名函數，這表示它們是沒有名稱的函數。這些函數是內聯定義的，通常用於簡短的一次性操作。 `lambda` 建構是函數式程式設計中的強大工具，可讓您動態建立簡潔且靈活的邏輯。

Lambda 函數在以下情況下特別有用：

- 您需要一個小功能來實現特定的臨時目的。
- 將函數作為參數傳遞給高階函數，例如`map`、`filter` 或`fold`。
- 從其他函數傳回函數。

### Lambda 函數的語法

Lambda 函數可以自己定義...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...或立即調用：

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** 函數接受的參數。
- **`body-expression`:** 呼叫函數時執行的邏輯。
- **立即呼叫：** 第二種形式顯示立即使用參數呼叫 lambda。

### Lambda 函數範例

#### 使用 Lambda 進行簡單計算

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

這裡：

- 建立一個 lambda 函數來新增兩個數字（`x` 和 `y`）。
- 使用參數`3` 和`5` 立即呼叫函數。

#### 內聯 Lambda 函數

以下範例示範如何將 `for-each` 與命名函數和 lambda 函數一起使用：

**使用命名函數：**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **解釋**：
  - `print-item` 是一個命名函數，它將數字轉換為字串 (`number->string`) 並使用 `lumi-message` 列印它。
  - `for-each` 將`print-item` 套用至清單`(1 2 3 4)` 中的每個元素。

**輸出**：1 2 3 4

**使用 Lambda 函數：**

可以使用 lambda 函數內聯編寫相同的邏輯，從而避免需要單獨的命名函數：

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **解釋**：
  - `(lambda (x) (lumi-message (number->string x)))` 定義匿名函數。
  - 此函數透過 `for-each` 套用於列表 `(1 2 3 4)` 的每個元素。

**輸出**：1 2 3 4

#### Lambda 函數作為參數

Lambda 函數通常直接傳遞給高階函數，例如 `map` 或 `filter`。

#### 將數字列表平方

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- `lambda` 函數將清單中的每個元素平方。
- `map` 函數將`lambda` 套用於每個元素。

#### Lambda 函數作為回傳值

您可以從另一個函數傳回 lambda 函數來建立動態行為。

#### 產生加法器函數

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` 產生一個新的 lambda 函數，該函數會新增特定數字 (`n`)。
- 傳回的 lambda 儲存在 `add5` 中，這會將 `5` 加到其輸入。

#### 將 Lambda 與 `let` 一起使用

Lambda 通常與 `let` 一起使用來建立本地範圍的暫存函數。

#### 用於加法的本機 Lambda

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- `let` 將 lambda 函數綁定到名稱 `add`。
- 然後將 lambda 用作 `let` 範圍內的普通函數。

#### 將 Lambda 與高階函數結合

Lambda 與高階函數結合執行複雜的資料轉換時會表現優異。

#### 過濾偶數

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- `lambda` 檢查數字是否為偶數。
- `filter` 函數使用 lambda 只保留清單中的偶數。

### Lambda 函數的優點

- **簡潔性：** Lambda 無需定義單獨的命名函數，從而減少了樣板程式碼。
- **靈活性：** 您可以在需要的地方定義和使用它們，使程式碼更加模組化。
- **提高可讀性：** 對於簡短的特定任務，lambda 可以明確意圖，而不會因附加命名函數而使程式碼變得混亂。

### 何時使用 Lambda 函數

在以下情況下使用 lambda 函數：

- 邏輯簡短且獨立。
- 此功能僅暫時或在特定範圍內需要。
- 您正在使用`map`、`filter` 或`reduce` 等高階函數。

避免將 lambda 用於複雜的多行邏輯，因為這會降低可讀性。對於更廣泛的操作，請改用命名函數。

### 結論

Scheme 中的 Lambda 函數提供了一種簡潔而強大的方法來為特定任務定義匿名函數。它們的靈活性和易用性使它們成為任何Scheme程式設計師的必備工具。了解如何有效地使用 `lambda` 將幫助您編寫更清晰、更模組化、更有效率的腳本。