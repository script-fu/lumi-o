---
title: "重构"
type: docs
weight: 2
---
一旦我们的函数可以工作，我们就可以退一步思考如何最好地构建我们的代码。目标是使我们的插件尽可能清晰、易于理解和可维护。这种在不改变现有代码行为的情况下改进和细化现有代码结构的过程称为重构。

这是初始函数：

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

function-name 是函数的名称，parameter 是函数接受的输入内容。主体是调用函数时运行的代码块。

摘要形式：

```scheme
(define (function-name parameter)
  body)
```

### 代码重复

尽早消除重复。 `(lumi-message "Hello world!\n")` 重复两次，消息字符串重复三次。变量解决了重复的字符串。

### 变量

在Scheme中，变量有一个已知的“范围”，并且该范围是使用`let`语句设置的。该变量在绑定部分绑定到一个值，并且该变量在let主体中具有作用域。该变量仅在 let 块内部已知，无法在其外部访问。

```scheme
(let ((variable value))
  body)
```

引入一个名为“message”的变量：

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

在我们的示例中，我们使用了一个名为“message”的变量，该变量绑定到字符串“Hello world!\n”。这使我们可以更改消息内容一次而不是三次，从而减少了出错的机会并使代码更加灵活。

### 提取函数

在函数式编程中，重构代码以将可重用逻辑提取到单独的函数中是一种常见的做法。通过这样做，**主函数**变得更加简单，并且更加专注于其高级目标，而**提取函数**显得更加复杂，因为它处理详细的逻辑。这是有意为之的，并且符合函数式编程的核心原则，例如模块化、关注点分离和可读性。这是重构的
世界你好！提取后。

提取逻辑：
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### 符号
在上面的示例中，使用了一种称为符号的数据类型，例如“gui”。符号作为参数传递给发送消息函数，可用于做出简单的条件决策。与符号键一样，它们是唯一标识符。有关符号的更多信息，请访问[this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### 简化主要功能

在原始 (scheme-hello-world) 函数中，将消息发送到不同输出（GUI、错误控制台、终端）的所有逻辑都混合到主函数中。重构后，主函数只是专注于**需要做什么**，将消息发送到不同的目的地。

重构后的main函数更加简单：

- 它清楚地说明了其目的：将相同的消息发送到多个输出。
- 它避免了重复代码使主逻辑混乱，例如为不同的输出设置消息处理程序。
- 一目了然，更容易阅读和理解。

### 提取函数的复杂度

相反，**（发送消息）函数**是详细逻辑所在的地方。它现在可以处理每个输出（GUI、错误控制台、终端）的行为变化。功能比以前复杂一点，但现在是**集中**和**隔离**。

## 将其与函数式编程联系起来

在函数式编程中，函数被视为**一等公民**，这意味着它们可以重用、传递和组合以形成更复杂的行为。目标是：- **将问题**分解为更小的、独立的部分。
- **将复杂性**隔离为处理特定任务的较小函数，例如`send-message`。
- **保持较高级别的功能简单**，以便他们可以专注于编排数据流和操作，而无需了解每个任务如何完成的详细信息。
- **关注点分离**：该函数根据输出类型负责如何发送消息，从而将此逻辑与主函数隔离。
- **模块化**：通过在一个地方处理所有消息发送逻辑，我们可以轻松地进行更改（例如添加新的输出选项），​​而无需更改主功能。
- **可重用性**：`send-message` 函数是可重用的，这意味着如果我们需要将消息发送到代码中其他位置的多个输出，我们可以简单地调用此函数，而不用重写类似的逻辑。

通过重构，此示例中的主函数变成了正在发生的事情的**声明性**语句（“向三个地方发送消息”），而如何发送这些消息的复杂性则被抽象到 `send-message` 函数中。