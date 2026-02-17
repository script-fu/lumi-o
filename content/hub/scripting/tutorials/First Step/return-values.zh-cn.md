---
title: "返回值"
type: docs
weight: 8
---
返回值很重要，因为它们可以让您控制流程而无需额外的状态。在Scheme中，最后计算的表达式成为返回值。

此页面使用消息传递示例中的验证助手来展示显式返回值如何使代码更易于编写。

### 什么是返回值？

在Scheme中，函数的返回值由函数计算的最后一个表达式确定。这意味着函数中最后一行代码的计算结果将作为函数的结果返回。如果未显式返回任何值，则该函数将返回 `#f` (false) 或 `undefined`。

让我们回顾一下验证函数，（is-valid-string？）

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

在此函数中，如果消息无效，则会引发错误。但是，如果消息有效，则不会给出显式返回值，函数默认返回`#f`。

### 使返回值显式化

我们可以通过使返回值更加明确来改进这一点。例如，如果消息有效，我们可以返回 `#t` (true)：

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

在此版本中，当消息有效时，该函数将返回`#t`，提供明确的结果。这使得该函数可以在需要布尔结果的其他上下文中更灵活地使用。

### 有效使用返回值

通过决定我们的函数返回什么，我们可以使它们更加可预测和有用。返回 `#t`、`#f` 等值或特定结果使我们能够更好地控制函数与其余代码的交互方式。例如，您可以使用返回值在调用函数中做出进一步的决定或将其作为参数传递给另一个函数。

这是一个使用返回值来控制逻辑流程的简单示例：

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

在这种情况下，(send-message)依赖(is-valid-output-display?)的返回值来决定是否继续。
如果第一次测试失败，将跳过条件语句`cond`。另外，请注意它如何以相当自然的方式读取，如果输出显示有效？

## Scheme 中的 If 语句逻辑

在重构库示例之前，先快速回顾一下条件逻辑。方案使用 `if` 在两条路径之间进行选择。

以下是 `if` 语句的简单形式：

```scheme
(if (conditional test)
  do if true
  do if false)
```

该结构检查条件，如果条件为真，则执行第一个操作。如果条件为假，则执行第二个操作。

如果您需要在条件为 true 或 false 时执行多个操作，可以使用 `begin` 将它们组合在一起：

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

这使您可以处理更复杂的情况，其中需要根据条件测试的结果执行多个表达式或语句。

好的，这是嵌入返回值的库代码，用于控制执行过程。

### 用返回值重构

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## 结论

返回值是使函数灵活和可重用的基本部分。通过仔细决定每个函数应该返回什么，我们可以确保我们的函数彼此良好地交互，并为其余代码提供有用的信息。无论是返回`#t`还是`#f`，或者更具体的东西，返回值都为我们提供了一种控制程序流程并处理各种结果的方法。