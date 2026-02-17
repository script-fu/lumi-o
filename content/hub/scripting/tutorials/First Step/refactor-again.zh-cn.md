---
title: "再次重构"
type: docs
weight: 5
---
随着帮助程序库的增长，一目了然地跟踪变得越来越困难。再次重构以保持每个功能较小且单一用途。

### 打破复杂性

为了使该功能更易于遵循和维护，请将其分解为较小的、集中的功能。首先将验证与消息路由分开。

### 创建验证函数

我们可以将验证 `message` 和 `output` 参数的函数部分移至单独的函数中。这样，核心`send-message`函数就不需要担心验证，从而更容易理解。

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### 简化消息发送

现在验证已移至单独的函数，`send-message` 函数可以专注于发送消息。它会简单得多，因为它只处理将消息定向到正确目的地的特定任务。

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 进一步细分：分离每个输出处理程序

每种类型的消息输出（GUI、错误控制台、终端）都可以移至其自己的函数中。这使得将来的测试、修改和潜在扩展变得更加容易。

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 在每个发送函数中重用验证

由于验证是确保消息和输出正确的重要部分，因此每个 `send-*` 函数执行自己的验证是有意义的。这确保了无论调用哪个输出，我们总是首先检查输入。

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

请注意，我们已经从发送消息函数中删除了验证，并将责任转移到每个单独的输出函数。此更改可确保每个目标（GUI、错误控制台、终端）处理自己的验证，简化发送消息功能并使验证逻辑更接近需要的位置。

这种方法可以简化发送消息函数，使其成为_dispatcher_，同时确保每个发送到*函数在处理之前正确验证消息。

通过将验证转移到每个 send-to-* 函数中，我们使它们可以作为独立函数重复使用。这意味着我们可以直接调用任何 send-to-gui、send-to-error-console 或 send-to-terminal 函数，而无需依赖发送消息调度程序函数。现在，这些函数中的每一个都可以完全处理自己的逻辑，并且可以在代码的其他部分或其他插件中独立使用，使您的代码更加模块化和灵活。

## 重构的好处

- **明确的关注点分离**：每个函数现在仅处理一项职责，使代码更易于理解。
- **可扩展性**：添加新的输出类型非常简单。您只需定义一个新函数，例如 `send-to-file` 或 `send-to-logger`，然后在 `cond` 语句中添加一个 case。
- **可重用性**：这些输出处理函数中的每一个都可以在项目的其他地方重用或在多个插件之间共享。
- **一致性**：通过在每个 `send-to-*` 函数中重用验证函数，您可以确保所有输出都得到正确验证，从而使代码更加健壮。

重构的库版本：

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

这就是我们能做的一切吗？不！还有更多工作要做，请继续阅读。