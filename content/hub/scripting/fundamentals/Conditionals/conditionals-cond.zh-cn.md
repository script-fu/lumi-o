---
title: "条件"
type: docs
weight: 5
---
在Scheme中，`cond`条件用于根据多个测试选择要执行的几个可能的代码块之一。它就像一个多分支`if`，按顺序检查每个分支，直到找到匹配项。

### 语法

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- 每个测试均按其编写顺序进行评估。
- 当测试计算结果为 true (`#t`) 时，执行其相应的 **结果**，并且 `cond` 表达式停止评估进一步的测试。
- `else` 子句是可选的，如果没有一个测试评估为 true，则用作后备。

### 它是如何运作的

1. **测试每个条件**：
   - `cond` 按测试列出的顺序评估测试。

2. **执行匹配结果**：
   - 当找到第一个评估为 true 的测试 (`#t`) 时，将执行其**结果**。
   - 如果没有测试评估为 true 并且存在 `else` 子句，则执行 **后备结果**。

### 示例

#### 示例 1：单个表达式结果

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- 第一个测试 `(< 3 2)` 的计算结果为 false (`#f`)。
- 第二个测试 `(= 3 3)` 的计算结果为 true (`#t`)，因此返回 `"This will run"`。
- `else` 子句未执行，因为已找到匹配项。

结果：**“这将运行”**

#### 示例 2：使用 `begin` 的多个操作

当后续涉及多个动作时，使用`begin`将它们分组：

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- 第一个测试 `(< 5 3)` 的计算结果为 false (`#f`)。
- 第二个测试 `(> 5 3)` 计算结果为 true (`#t`)：
  - 它打印`"Condition met"`。
  - 然后计算`(* 5 5)`并返回`25`。

结果：**打印“满足条件”并返回 25。**

#### 示例 3：在后续结果中使用 `let` 块

当您需要引入局部变量时，请使用 `let` 块：

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- 第一个测试`(< 0 -1)` 是错误的。
- 第二个测试 `(> 0 -1)` 为真，因此：
  - 执行`let` 块，将`y` 绑定到`20`。
  - 它打印`"Positive condition met"`。
  - 然后计算`(+ y y)`并返回`40`。

结果：**打印“满足肯定条件”并返回 40。**

#### 示例 4：使用 `else` 回退

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- 前两个测试的评估结果都不为 true。
- 执行`else` 子句并返回`"Fallback value"`。

结果：**“后备值”**

### 总结

- 使用`cond` 以清晰简洁的方式处理多个条件。
- 结果可以是单个表达式或使用 `begin` 的分组操作。
- 在结果中使用`let`来声明用于计算的局部变量。
- 始终包含 `else` 子句作为处理意外情况的后备措施。

这种灵活性使 `cond` 成为处理复杂分支逻辑的强大且可读的工具。