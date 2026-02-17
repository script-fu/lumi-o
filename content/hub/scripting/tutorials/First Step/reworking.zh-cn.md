---
title: "返工"
type: docs
weight: 7
---
此步骤修复了消息传递示例中的一个微妙行为。

我们传递字符串“Hello world!\n”作为消息。 “\n”是一种特殊的字符，是“转义”字符。它告诉输出打印开始换行。在Scheme中，它还会强制发送到状态栏的消息以GUI框的形式弹出。

帮助器 `send-to-gui` 将消息发送到 Lumi 对话框。

更新消息内容和目标，以便示例行为一致。

删除转义字符并扩展功能：
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

将幻数替换为 Lumi 提供的常量（例如 `MESSAGE-BOX` 和 `ERROR-CONSOLE`）。

然后将验证拆分为两个函数，以便可以从多个调用站点重用。

- (is-valid-string?) 在 send-to* 函数中检查字符串是否为字符串而不是空字符串。
- (is-valid-output-display?) 在发送消息函数中检查给定的输出目的地是否有效。

重新设计库：

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## 结论

通过重新设计我们的消息传递库，我们使其更加强大和可靠。我们修复了换行符的隐藏问题，引入了常数以提高清晰度，并通过添加对状态栏和对话框输出的支持来扩展功能。此外，将验证逻辑分成更小的、集中的函数可以确保我们的代码在将来更容易维护和扩展。

这次返工展示了微小的变化如何增强我们库的整体结构和功能，为随着我们项目的发展提供更大的灵活性和可重用性铺平道路。