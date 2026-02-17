---
title: "列表"
type: docs
weight: 4
---
在Scheme中，**列表**是用于对值进行分组的基本数据结构。列表是元素的有序集合，其中每个元素可以是任何类型，包括另一个列表。列表在Scheme中广泛用于数据存储和程序结构。

### 示例 1：简单列表

```scheme
(list 1 2 3)
```

- 创建包含三个元素的列表：`1`、`2` 和 `3`。

结果：**`(1 2 3)`**

---

#### 访问列表元素

使用 `car` 和 `cdr` 过程访问列表中的元素：

- `car` 检索列表的第一个元素。
- `cdr` 检索列表的其余部分（除第一个元素之外的所有内容）。

#### 示例

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

结果：

- `(car my-list)` 返回`1`
- `(cdr my-list)` 返回`(2 3)`

---

#### 简单递归：遍历列表

通过对列表的`cdr`递归调用`car`，可以逐一处理每个元素，直到遍历完列表。这构成了许多列表处理算法的基础。

#### 示例：打印列表的每个元素

这是一个简单的递归函数，用于打印列表中的每个元素：

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **基本情况：** 如果列表为空 (`null? lst`)，则停止递归。
- **递归情况：** 打印第一个元素 (`car lst`)，然后调用列表其余部分 (`cdr lst`) 上的函数。

#### 用法示例

```scheme
(print-elements (list 1 2 3))
```

输出：

-`"1"`
- `"2"`
- `"3"`

结果：“完成”

---

#### 它是如何工作的

1. 该函数使用 `car` 检索列表的第一个元素并对其进行处理。
2. 然后它用列表的其余部分调用自身 (`cdr`)。
3. 重复此过程，直到列表为空 (`null? lst`)。

---

### 示例 2：混合类型

列表可以包含不同类型的元素，包括字符串、布尔值、数字、其他列表，甚至表达式的结果：

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- 这将创建一个列表：
  - 一个数字 (`42`)
  - 字符串 (`"hello"`)
  - 布尔值 (`#t`)
  - 另一个列表 (`(1 2)`)
  - 表达式的结果（`(+ 3 4)`，其计算结果为`7`）

结果：**`(42 "hello" #t (1 2) 7)`**

---

这些例子展示了Scheme中列表的多功能性，使它们成为组织和操作数据的强大工具。

### 构建列表

`cons` 过程用于通过将元素与现有列表组合来构造新列表。

```scheme
(cons new-element existing-list)
```

#### 示例

```scheme
(cons 0 (list 1 2 3))
```

- 将`0` 添加到列表`(1 2 3)` 的开头。

结果：**`(0 1 2 3)`**

---

### 检查列表

`list?` 过程检查给定值是否是列表。

```scheme
(list? value)
```

#### 示例：列表？

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

结果：

- `(list? (list 1 2 3))` 返回`#t`（真）
- `(list? 42)` 返回`#f` (假)

---

### 列表操作

Scheme 提供了几个用于处理列表的内置过程，包括：

- `length`：返回列表中的元素数量。
- `append`：将两个或多个列表合并为一个。
- `reverse`：返回一个新列表，其中元素按相反顺序排列。

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

结果：

- `(length (list 1 2 3))` 返回`3`
- `(append (list 1 2) (list 3 4))` 返回`(1 2 3 4)`
- `(reverse (list 1 2 3))` 返回`(3 2 1)`#### 使用`list-ref`

`list-ref` 过程检索列表指定索引处的元素（从零开始的索引）。

```scheme
(list-ref lst index)
```

- **`lst`**：从中检索元素的列表。
- **`index`**：从零开始的索引，指示要返回哪个元素。

##### 示例：列表引用

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

结果：`30`

---

### 嵌套列表

Scheme 中的列表可以包含其他列表作为元素，从而创建嵌套结构。

#### 示例：创建嵌套列表

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- 创建一个包含三个元素的列表，每个元素本身就是一个列表。

结果：**`((1 2) (3 4) (5))`**

---

#### 访问嵌套数据

要访问嵌套列表中的元素，您可以使用 `car` 和 `cdr` 的组合来浏览结构。

#### 示例：访问元素

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### 解释

1. **`car nested-list`**：
   - 检索`nested-list` 的第一个元素，即`(1 2)`。

2. **`car (car nested-list)`**：
   - 检索`(1 2)` 的第一个元素，即`1`。

3. **`cdr (car nested-list)`**：
   - 检索`(1 2)` 的其余部分，即`(2)`。

4. **`car (cdr (car nested-list))`**：
   - 检索`(2)` 的第一个元素，即`2`。

---

#### 示例：访问其他子列表中的元素

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

这种方法允许您系统地导航和访问嵌套列表中的特定元素，为处理分层数据提供强大的灵活性。

### 总结

-Scheme 中的**列表**是通用且重要的数据结构。
- 使用`list` 创建列表，使用`car` 和`cdr` 访问元素，使用`cons` 构建列表。
- `length`、`append`、`reverse` 和 `list-ref` 等内置程序使列表操作变得简单高效。
- 列表可以嵌套，从而为高级用例启用复杂的数据结构。