---
title: "验证"
type: docs
weight: 4
---
在构建强大的插件时，重要的是要确保我们的函数能够优雅地处理错误并按预期工作，即使在误用或意外输入的情况下也是如此。验证有助于保护功能的完整性并防止崩溃或意外行为。

让我们看看如何通过添加验证检查来改进 `send-message` 函数，以确保它正确处理输入。

### 验证输入

在发送消息之前，我们应该确保传递给`send-message`函数的`output`参数是有效的。我们可以添加一个检查来确认输出目标是预期值之一（gui、错误控制台或终端）。

示例：

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

在此示例中，我们使用 `member` 来检查 `output` 参数是否有效。如果不是，该函数会引发错误并显示明确的消息，以防止无效值引起问题。

### 处理空消息

确保 `message` 参数有效也很有用。例如，如果将空字符串或 #f (false) 作为消息传递，则函数应妥善处理此问题。

处理空消息的示例：

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

这种方法可确保函数始终接收有效输入，从而提高其可靠性并防止意外行为。

### 组合验证示例

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

在此版本中：
- 该函数首先检查`message` 是否为空或无效。如果消息有效，则继续检查 `output` 是否为可接受的值之一（`gui`、`error-console` 或 `terminal`）。
- 如果两项检查都通过，则消息将发送到适当的输出。否则，会出现错误消息并给出明确的解释。
- 进行额外检查以确保消息也是字符串。

这种组合的验证功能使代码更加清晰，并确保在采取任何操作之前验证两个输入，从而使该功能更加健壮。请注意，我们还构建了一个调试消息系统。当
代码失败，我们得到一个原因，一个我们自己编写的原因。

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```