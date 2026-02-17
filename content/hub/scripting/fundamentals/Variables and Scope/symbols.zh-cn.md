---
title: "符号"
type: docs
weight: 6
---
符号是Scheme中的核心数据类型之一，代表唯一的、不可变的标识符。它们主要用作程序中的键、标记或占位符，这使得它们对于编写干净且富有表现力的代码至关重要。

Scheme 中的符号类似于字符串，但不同之处在于符号是**唯一**和**原子**。这意味着具有相同名称的两个符号保证是同一个对象，从而允许快速相等检查和数据结构的有效使用。

### 语法

符号被写成字符序列：

- 以字母开头，后跟字母、数字或特殊字符，例如`-`、`+` 或`*`。
- 默认情况下，符号区分大小写。

示例：

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## 创建符号

符号通常是使用 **quote** 运算符 (`'`) 创建的，它告诉Scheme 将名称视为符号，而不是将其作为变量或函数进行计算。

### 示例

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

您还可以使用 `string->symbol` 过程以编程方式创建符号，该过程将字符串转换为符号。

```scheme
(string->symbol "dynamic-symbol")
```

**结果**：`'dynamic-symbol`


## 比较符号

由于符号是唯一的，因此您可以使用 `eq?` 有效地比较它们。

### 示例

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

这使得符号非常适合用作数据结构中的键或代码中的标记。

## 使用符号

符号在方案中经常用于：

1. **关联列表中的键：**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **代码中的标识符：**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## 使用符号的过程

Scheme 提供了使用符号的内置过程：

|程序 |描述 |
|--------------------------------|--------------------------------------------------------------------------------------------|
| **`symbol?`** |检查一个对象是否是一个符号。                                            |
| **`eq?`** |比较两个符号的同一性（快速比较）。                       |
| **`symbol->string`** |将符号转换为字符串（对于显示或调试有用）。          |
| **`string->symbol`** |将字符串转换为符号（对于动态创建标识符很有用）。 |

### 示例

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## 总结

符号是在Scheme中表示标识符、键和标记的一种轻量级、高效的方式。它们的不变性和快速身份检查使它们成为许多编程任务的理想选择。了解如何有效地使用符号将增强您编写简洁且富有表现力的Scheme代码的能力。