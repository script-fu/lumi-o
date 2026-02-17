---
title: "命名為 let 或 Local Define"
type: docs
weight: 5
---
**名為`let`** 和**本地`define`@** 都是Scheme 中用於建立程式碼的強大工具，但它們有不同的用途。了解何時使用每種腳本有助於創建乾淨、模組化且高效的腳本。

### 概述

- **名為`let`@**：在本地範圍內結合變數綁定和遞歸的構造，通常用於迭代或遞歸計算。
- **本地`define`@**：一種在封閉函數範圍內定義輔助函數或變數的方法，使它們可以在該函數的不同部分中重複使用。

---

### 命名為`let`

#### 特點：
1. 將變數綁定和遞歸組合到單一構造中。
2. 範圍限定為 `let` 區塊的主體。
3. 非常適合**局部遞歸**或特定於單一任務的迭代過程。

#### 語法
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### 範例：對清單的元素求和
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**結果**：`10`

- **工作原理**：`loop` 函數在 `let` 中定義，允許使用更新的綁定進行遞歸呼叫。

---

### 本地`define`

#### 特點：
1. 允許建立可在封閉函數內重複使用的輔助函數或變數。
2. 作用域為封閉函數，但在其整個函數體中可見。
3. 非常適合模組化具有多個步驟或可重複使用邏輯的程式碼。

#### 語法
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### 範例：處理多個值
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**結果**：`41`（計算\(2^2 + 3^3 + 4^2\)）

- **工作原理**：輔助函數 `square` 和 `cube` 可在 `process-values` 函數內重複使用，從而實現模組化邏輯。

---

### 主要區別

| **方面** | **命名為`let`** | **本地`define`** |
|------------------------------------------------|------------------------------------------------|------------------------------------------------|
| **目的** |以局部方式結合遞歸和迭代。 |定義可重複使用的輔助函數或變數。 |
| **範圍** |僅限`let` 區塊的主體。           |在整個封閉功能中可見。      |
| **可重複使用性** |在 `let` 區塊之外不可重複使用。             |可在函數內多次重複使用。    |
| **最佳用例** |與單一任務相關的本地化遞歸或迭代。 |透過多個可重複使用步驟來模組化程式碼。 |
| **語法** |將綁定和遞歸結合在一個構造中。  |明確定義函數或變數。      |

---

### 何時使用命名 `let`

1. **一次性邏輯**：當遞歸或迭代特定於單一計算時。
2. **封裝**：避免在封閉函數的命名空間增加額外的函數名稱。
3. **迭代**：在循環構造中管理中間變數時。

**範例：階乘計算**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**結果**：`120`

---

### 何時使用本地`define`

1. **可重複使用助手**：當邏輯需要在函數的多個部分中重複使用時。
2. **模組化設計**：將複雜的計算分解為更小的、命名的子任務。
3. **多個步驟**：當計算的不同部分需要多個輔助函數時。**範例：處理輸入**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**結果**：`(13 36)`（計算 \(2^2 + 3^2\) 和 \(2^2 \cdot 3^2\)）

---

### 在命名`let`中組合宣告和輸入

命名的 `let` 最強大的功能之一是它能夠將遞歸的**局部變數宣告**和**輸入參數**組合到單一構造中。這使得命名的 `let` 對於迭代或遞歸任務來說既簡潔又富有表現力。

#### 局部變數聲明
在命名的`let` 中，括號中的綁定充當使用特定值初始化的**局部變數**。這些變數的作用域為 `let` 的主體。

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` 和 `y`** 是作為 `let` 的一部分定義和初始化的局部變數。

---

#### 遞歸的輸入參數
相同的變數也充當對命名 `let` 的遞歸呼叫的**輸入參數**。當命名的 `let` 呼叫自身時，它會使用新值更新這些變數。

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **第一次迭代**：`x = 1`、`y = 2`
- **第二次迭代**：`x = 2`、`y = 4`
- **第三次迭代**：`x = 3`、`y = 8` 等等...

---

#### 等效於使用本地 `define`

命名的 `let` 包含變數初始化作為其語法的一部分。這樣就不需要單獨的步驟來設定初始值。以下兩個範例是等效的：

##### 使用命名`let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### 使用本地`define`
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

兩者執行相同的計算，但名為 `let` 將變數宣告和遞歸設定合併為一個簡潔的構造。

---

#### 宣告與輸入結合的優點

1. **簡潔**：命名為 `let` 透過將變數初始化和遞歸合併到單一構造中來減少樣板檔案。
2. **清晰度**：它清楚地表明遞歸是 `let` 的本地遞歸並與特定任務相關聯。
3. **封裝**：遞迴邏輯保持獨立，不會污染封閉函數的命名空間。

命名`let` 的雙重用途性質（既作為變數宣告又作為遞歸輸入機制）使其成為Scheme 程式設計中強大而獨特的功能。

### 總結

- 使用**命名`let`**進行**局部遞歸**或**迭代**，特別是當邏輯與單一任務緊密耦合時。
- 使用**本地`define`**透過可重複使用的輔助函數或變數來**模組化程式碼**。

透過了解它們的差異，您可以編寫更簡潔、有組織、可維護的Scheme程式。