---
title: "簡單遞迴"
type: docs
weight: 5
---
遞歸是Scheme 中一個強大的概念，其中函數呼叫自身來解決原始問題的較小子問題。 **簡單遞歸**模式涉及停止遞歸的基本情況和減少問題的遞歸情況。

遞歸函數的一般結構如下所示：

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **基本條件**：停止遞迴。
- **基本結果**：滿足基本條件時傳回的值。
- **遞歸呼叫**：使用修改後的參數呼叫函數本身，使計算更接近基本情況。

---

### 範例：數字總和（1 到 n）

一個簡單的遞歸函數，用於計算從 1 到 n 的數字總和：

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### 工作原理：分解和重新組裝

遞歸的工作原理是將原始問題分解為更小的部分。對函數的每次呼叫都會處理一個部分並傳遞其餘部分。一旦達到最簡單的情況，計算完成時結果將重新組合。

#### sum-to-n 3 的逐步追踪

1. **初始呼叫**：*sum-to-n 3*
   → *(+ 3 (和 n 2))*

2. **第二次呼叫**：*sum-to-n 2*
   → *(+ 2 (和 n 1))*

3. **第三次呼叫**：*sum-to-n 1*
   → *(+ 1 (和到 n 0))*

4. **基本情況**：*sum-to-n 0*
   → *0*

---

#### 重新組合最終結果

一旦解決了最簡單的情況，每一層計算就完成了：

1. *sum-to-n 0* 得到 *0*
2. *sum-to-n 1* 變成 *(+ 1 0) = 1*
3. *sum-to-n 2* 變成 *(+ 2 1) = 3*
4. *sum-to-n 3* 變成 *(+ 3 3) = 6*

---

### 範例：列印清單的每個元素

這是一個簡單的遞歸函數，用於列印清單中的每個元素：

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **基本情況：** 如果清單為空（*null？lst*），則停止遞迴。
- **遞歸情況：** 列印第一個元素 (*car lst*)，然後呼叫清單其餘部分的函數 (*cdr lst*)。

#### 用法範例

```scheme
(print-elements (list 1 2 3))
```

輸出：

- *“1”*
- *“2”*
- *“3”*

結果：*「完成」*

---

#### 它是如何運作的

1. 此函數使用 *car* 檢索列表的第一個元素並對其進行處理。
2. 然後它用列表的其餘部分呼叫自身 (*cdr*)。
3. 重複此程序直到清單為空 (*null? lst*)。

---

### 總結

- 簡單的遞迴包括：
  1. **基本情況**：停止遞迴。
  2. **遞歸情況**：將問題減少到基本情況。
- 每個遞歸呼叫都會使計算逐漸完成。
- 一旦達到基本情況，結果將在遞歸完成時合併。

遞歸反映了問題的結構並提供了清晰的邏輯流程。始終確保基本情況以避免無限遞歸。