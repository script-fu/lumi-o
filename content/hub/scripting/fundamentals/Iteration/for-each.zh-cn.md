---
title: "对于每个"
type: docs
weight: 5
---
Scheme 中的 `for-each` 函数用于将过程应用于列表（或多个列表）的每个元素。与 `map` 返回包含结果的新列表不同，`for-each` 用于其**副作用**，例如打印或更新变量。

`for-each` 最简单的形式如下所示：

```scheme
(for-each procedure list)
```

- **过程**：应用于列表中每个元素的函数。
- **列表**：将处理其元素的列表。

---

### 示例：打印列表

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- 此处，函数`print-item` 应用于列表`(1 2 3 4)` 的每个元素。
- 这会导致按顺序打印每个数字。

**输出**：`1 2 3 4`

---

### 它是如何运作的

1. **迭代每个元素**：
   - 所提供的过程按顺序对列表中的每个元素执行。

2. **产生副作用**：
   - 常见的副作用包括打印、记录或修改外部变量。与`map` 不同，`for-each` 不返回新列表。

---

#### 示例：与多个列表一起使用

如果提供了多个列表，`for-each` 会处理每个列表中的相应元素。

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- 函数`sum-and-print` 对两个列表中的相应元素求和并打印结果。

**输出**：`5 7 9`

---

### 总结

- `for-each` 函数对于对列表的每个元素执行副作用非常有用。
- 与`map` 不同，`for-each` 不会生成新列表——它仅关注过程的副作用。
- 它可以同时处理多个列表，将过程应用于相应的元素。

通过使用`for-each`，当目标是执行操作而不是转换数据时，您可以有效地处理列表。