---
title: "如果"
type: docs
weight: 4
---
以最简单的形式，Scheme 中的 `if` 条件评估测试，并根据结果执行两个可能的代码块之一。最简单的形式如下所示：

```scheme
(if test-is-true
  do-this)
```

- 如果 `test` 计算结果为 true (`#t`)，则执行后续代码中的 **代码块**。该块可以返回一个值或执行其他操作，例如分配变量或打印输出。

### 示例

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- 在本例中，`test` 是`(< 0 1)`（检查 0 是否小于 1）。
- 由于测试结果为 true (`#t`)，因此执行代码块`(lumi-message "True!")`，打印`"True!"`。

### 添加 Else 条件：`if-else`

当将 `if` 条件与替代代码块（`else` 情况）一起使用时，结构如下所示：

```scheme
(if test
  do-this
  else-do-this)
```

- 如果 `test` 计算结果为 true (`#t`)，则执行 **后续** 代码块。
- 如果 `test` 的计算结果为 false (`#f`)，则执行 **替代** 代码块。

```scheme
(if test
  consequent
  alternative)
```

### 它是如何运作的

1. **测试表达**：
   - 首先评估`test` 表达式。

2. **基于测试的结果**：
   - 如果 `test` 计算结果为 true (`#t`)，则执行 **后续代码块**。
   - 如果 `test` 的计算结果为 false (`#f`)，则执行 **替代代码块**。

`consequent` 和`alternative` 代码块都可以执行任何有效的Scheme 操作，包括返回值、修改变量或运行过程。

### 示例

#### 示例 1：返回值

```scheme
(if (< 0 1)
  1
  0)
```

- 这里，`test`是`(< 0 1)`（检查0是否小于1）。
- 由于测试评估结果为 true (`#t`)，因此将执行 **后续** 块 (`1`) 并返回其值。

结果：**1**

#### 示例 2：评估开始块

如果您需要在条件为 true 或 false 时执行多个操作，可以使用 `begin` 或 `let` 将它们组合在一起。

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- 在此示例中，`test` 是`(= 0 1)`（检查 0 是否等于 1）。
- 由于测试结果为 false (`#f`)，因此执行 **alternative** 块：
  - 首先，它打印`"False condition met, calculating..."`。
  - 然后，它计算`(* 3 4)`并返回`12`。

结果：**打印“满足错误条件，计算...”并返回 12。**

#### 示例 3：评估 let 语句

使用 `let` 允许我们在代码块中声明局部范围变量。

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- 在此示例中，`test` 是 `(= 1 1)`（检查 1 是否等于 1）。
- 由于测试结果为 true (`#t`)，因此执行 **consequence** 块：
  - 首先，它打印`"True condition met, calculating..."`。
  - 然后，它计算`(* -1 10)`并返回`-10`。

结果：**打印“满足真实条件，计算...”并返回 -10。**

### 总结- `if` 条件是Scheme 中用于评估测试和执行相应代码块的强大工具。
- 它可以处理简单的表达式和返回值、修改变量或执行副作用的复杂代码块。
- 请记住：如果没有显式的 `else` 块，则 `if` 仅在测试为真时评估并执行 **结果**。否则，它会评估并执行**替代方案**。