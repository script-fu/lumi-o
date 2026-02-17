---
title: "协会列表（Alists）"
type: docs
weight: 6
---
**关联列表**（或**alist**）是Scheme中的一种基本数据结构，用于表示键值对的集合。它被实现为一个对的列表，其中每对将一个键（通常是一个符号）与一个值相关联。列表简单、灵活，非常适合中小型数据集。

### 关联列表的结构

alist 是一个列表，其中每个元素都是一个**对**（使用 `cons` 构造）。每对包括：

- **键**：第一个元素（通常是符号）。
- **Value**：第二个元素，可以是任何数据类型。

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **密钥**：`'name`、`'age`、`'city`
- **值**：`"Alice"`、`30`、`"Paris"`
- **结构**：对列表：
  `((name . "Alice") (age . 30) (city . "Paris"))`

### 创建一个列表

您可以通过手动构建对或使用 `cons` 以编程方式构建来创建列表。

#### 使用单引号 (`'`)

单引号 (`'`) 是 **quoting** 的简写，它会阻止Scheme 计算表达式。这使得它非常适合创建所有键和值都被硬编码的静态列表。

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**结果**：
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### 使用反引号 (`` ` ``) and Comma (`,`)

反引号 (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`) 运算符。这对于创建在运行时计算键或值的列表很有用。

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**结果**：
`((name . "Alice") (age . 30) (city . "Paris"))`

### 示例比较

使用 `'` 的静态列表：

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

使用 `` ` `` and `,` 的动态列表：

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### 访问列表中的数据

要从列表中检索值，您可以使用 `assoc` 函数，该函数通过键查找一对。

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### 提取值

使用 `assoc` 检索一对后，请使用 `cdr` 提取该值：

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### 主要功能总结

- **单引号 (`'`)**：创建一个静态列表，其中所有元素都是文字数据。
- **反引号 (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`)。
- **点符号 (`.`)**：用于构造对，将键与列表中的值关联起来。