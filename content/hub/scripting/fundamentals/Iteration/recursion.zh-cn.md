---
title: "简单递归"
type: docs
weight: 5
---
递归是Scheme 中一个强大的概念，其中函数调用自身来解决原始问题的较小子问题。 **简单递归**模式涉及停止递归的基本情况和减少问题的递归情况。

递归函数的一般结构如下所示：

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **基本条件**：停止递归。
- **基本结果**：满足基本条件时返回的值。
- **递归调用**：使用修改后的参数调用函数本身，使计算更接近基本情况。

---

### 示例：数字之和（1 到 n）

一个简单的递归函数，用于计算从 1 到 n 的数字之和：

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### 工作原理：分解和重新组装

递归的工作原理是将原始问题分解为更小的部分。对函数的每次调用都会处理一个部分并传递其余部分。一旦达到最简单的情况，计算完成时结果将被重新组合。

#### sum-to-n 3 的逐步追踪

1. **初始调用**：*sum-to-n 3*
   → *(+ 3 (和 n 2))*

2. **第二次调用**：*sum-to-n 2*
   → *(+ 2 (和 n 1))*

3. **第三次调用**：*sum-to-n 1*
   → *(+ 1 (和到 n 0))*

4. **基本情况**：*sum-to-n 0*
   → *0*

---

#### 重新组合最终结果

一旦解决了最简单的情况，每一层计算就完成了：

1. *sum-to-n 0* 得到 *0*
2. *sum-to-n 1* 变为 *(+ 1 0) = 1*
3. *sum-to-n 2* 变为 *(+ 2 1) = 3*
4. *sum-to-n 3* 变为 *(+ 3 3) = 6*

---

### 示例：打印列表的每个元素

这是一个简单的递归函数，用于打印列表中的每个元素：

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **基本情况：** 如果列表为空（*null？lst*），则停止递归。
- **递归情况：** 打印第一个元素 (*car lst*)，然后调用列表其余部分的函数 (*cdr lst*)。

#### 用法示例

```scheme
(print-elements (list 1 2 3))
```

输出：

- *“1”*
- *“2”*
- *“3”*

结果：*“完成”*

---

#### 它是如何工作的

1. 该函数使用 *car* 检索列表的第一个元素并对其进行处理。
2. 然后它用列表的其余部分调用自身 (*cdr*)。
3. 重复此过程直到列表为空 (*null? lst*)。

---

### 总结

- 简单的递归包括：
  1. **基本情况**：停止递归。
  2. **递归情况**：将问题减少到基本情况。
- 每个递归调用都会使计算逐渐完成。
- 一旦达到基本情况，结果将在递归完成时合并。

递归反映了问题的结构并提供了清晰的逻辑流程。始终确保基本情况以避免无限递归。