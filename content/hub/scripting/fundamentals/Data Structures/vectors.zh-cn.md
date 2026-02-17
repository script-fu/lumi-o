---
title: "向量"
type: docs
weight: 5
---
在Scheme中，向量是另一种用于对值进行分组的基本数据结构。与列表不同，向量是固定大小的索引元素集合，提供更快的随机访问和更新。向量中的每个元素可以是任何类型，包括另一个向量。向量使用 # 后跟括号来表示。 `#(1 2 3)`

虽然向量和列表可能看起来相似，但它们在方案编程中具有不同的用途：

- 列表更常用于递归操作和动态结构，因为它们的链接节点实现允许通过递归分解有效地操纵它们的开始和遍历。

- 另一方面，向量针对需要随机访问元素或更新特定索引的场景进行了优化，使它们更适合查找表、固定大小配置或性能关键的索引操作等用例。

从本质上讲，列表是递归算法和动态大小数据的自然选择，而当固定大小或索引访问模式至关重要时，向量就会大放异彩。

### 简单向量

```scheme
(vector 1 2 3)
```

- 创建包含三个元素的向量：`1`、`2` 和`3`。

结果：**`#(1 2 3)`**

#### 访问向量元素

使用`vector-ref` 过程访问向量中的元素，它检索指定索引处的元素（从`0` 开始）。

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### 迭代：处理向量中的每个元素

您可以使用循环或递归来迭代向量。 Scheme 提供`vector-length` 来确定向量的大小。这是一个打印向量中每个元素的简单循环：

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **基本情况：** 如果索引 `i` 达到向量的长度，则停止循环。
- **递归情况：** 打印索引 `i` 处的元素，然后递增 `i`。

#### 用法示例

```scheme
(print-elements (vector 1 2 3))
```

结果：

- `"1"`
- `"2"`
- `"3"`

结果：“完成”

### 混合向量

向量可以包含不同类型的元素，包括字符串、布尔值、数字、其他向量，甚至表达式的结果：

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

这将创建一个向量：
  - 一个数字 (`42`)
  - 字符串 (`"hello"`)
  - 布尔值 (`#t`)
  - 另一个向量 (`#(1 2)`)
  - 表达式的结果（`(+ 3 4)`，其计算结果为`7`）

结果：**`#(42 "hello" #t #(1 2) 7)`**

### 构造向量

向量是使用`vector`创建的，或者使用`make-vector`创建具有初始值的固定大小的向量。

```scheme
(make-vector 5 0)
```

创建大小为`5` 的向量，所有元素都初始化为`0`。

结果：`#(0 0 0 0 0)`

### 更新向量

`vector-set!` 过程更新向量中指定索引处的元素。

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

结果：`#(1 42 3)`

### 检查向量

`vector?` 过程检查给定值是否是向量。

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

结果：

- `(vector? (vector 1 2 3))` 返回`#t`（真）
- `(vector? 42)` 返回`#f` (假)

### 向量和引用传递行为在Scheme中，向量是可变的并且通过引用传递。这意味着当您将向量传递给函数时，该函数可以直接修改原始向量。对函数内部向量所做的任何更改也将反映在函数外部。此行为对于跨多个函数有效共享和更新数据很有用，但也需要谨慎以避免意外的副作用。

#### 示例：修改函数中的向量

下面的示例演示了如何通过引用传递向量并进行修改：

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

结果：`#(10 99 30)`

#### 分步说明

1. **创建向量：** `my-vector` 使用值 `10`、`20` 和 `30` 进行初始化。
2. **传递到函数：** `my-vector` 连同要更新的索引和新值一起传递到 `modify-vector`。
3. **在函数中修改：** `vector-set!` 过程直接更新原始向量中指定索引处的值。
4. **反映更改：** 由于向量是通过引用传递的，因此函数内所做的更改将反映在原始向量中。

#### 引用传递的含义

- **性能：** 通过引用传递向量非常高效，因为它避免了复制大型结构。
- **副作用：** 跨函数共享向量时要小心，以避免对共享数据进行意外修改。

### 向量运算

Scheme 提供了几个用于处理向量的内置过程，包括：

- `vector-length`：返回向量中的元素数量。
- `vector->list`：将向量转换为列表。
- `list->vector`：将列表转换为向量。

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

结果：

- `(vector-length (vector 1 2 3))` 返回`3`
- `(vector->list (vector 1 2 3))` 返回`(1 2 3)`
- `(list->vector (list 1 2 3))` 返回`#(1 2 3)`

### 嵌套向量

Scheme 中的向量可以包含其他向量作为元素，从而创建嵌套结构。

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

创建一个由三个元素组成的向量，每个元素本身就是一个向量。

结果：**`#(#(1 2) #(3 4) #(5))`**

#### 访问嵌套数据

要访问嵌套向量内的元素，请多次使用 `vector-ref` 来浏览结构。

#### 示例：访问元素

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### 总结

-Scheme 中的**向量**是固定大小的索引数据结构。
- 使用`vector` 创建向量，使用`vector-ref` 访问元素，使用`vector-set!` 更新元素。
- `vector-length`、`vector->list` 和`list->vector` 等内置过程可实现灵活的操作。
- 嵌套向量允许复杂的分层数据结构。