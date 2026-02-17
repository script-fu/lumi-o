---
title: "什么时候"
type: docs
weight: 5
---
在Scheme中，虽然`if`是优雅且通用的，但在没有显式`else`的情况下使用时可能会变得混乱。当目的是仅在条件为真时执行单个代码分支，而对于 `false` 情况没有替代操作时，尤其如此。在这种情况下，`when` 构造提供了更清晰、更简洁的替代方案。

`when` 的基本形式如下所示：

```scheme
(when test-is-true
  do-this
  do-that)
```

- 如果 `test` 的计算结果为 true (`#t`)，则 `when` 构造体中的所有表达式将按顺序执行。
- 如果 `test` 的计算结果为 false (`#f`)，则不会发生任何情况，并且不会返回任何值。

### 示例

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### 对比`if` 和`when`

为了更好地理解 `if` 和 `when` 之间的区别，请考虑以下两者一起使用的示例：

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### 说明：

1. **`if` 条件**：
   - 测试`(= 0 1)` 检查 0 是否等于 1。
   - 由于这是 false (`#f`)，因此执行`if` 的`else` 分支。

2. **`else` 分支中的 `when` 构造**：
   - `when` 测试 `(< 0 1)` 检查 0 是否小于 1。
   - 由于这是 true (`#t`)，`when` 主体内的所有表达式都会按顺序执行：
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### 为什么在这里使用`when`？

- 当条件不需要显式`else` 分支时，使用`when` 代替另一个`if` 可以简化逻辑。
- `when` 明确表示只有真正的分支才是相关的，从而减少了潜在的混乱。

### 总结

- 当您需要 true 和 false 分支时，请使用 `if`。
- 当真实情况只有一个分支时，尤其是需要执行多个操作时，请使用`when`。
- 组合 `if` 和 `when` 可以帮助清晰简洁地构造更复杂的条件。