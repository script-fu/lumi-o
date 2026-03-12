---
title: "再次重構"
type: docs
weight: 5
---
随着帮助程序库的增长，一目了然地跟踪变得越来越困难。再次重構以保持每個功能較小且單一用途。

### 打破複雜性

为了使该功能更易于遵循和维护，请将其分解为较小的、集中的功能。首先将验证与消息路由分开。

### 建立驗證函數

我们可以将验证 `message` 和 `output` 参数的函数部分移至单独的函数中。這樣，核心`send-message`函數就不需要擔心驗證，從而更容易理解。

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### 簡化訊息發送

現在驗證已移至單獨的函數，`send-message` 函數可以專注於傳送訊息。它會簡單得多，因為它只處理將訊息定向到正確目的地的特定任務。

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

### 進一步細分：分離每個輸出處理程序

每種類型的消息輸出（GUI、錯誤控制台、終端）都可以移至自己的函數中。這使得將來的測試、修改和潛在擴展變得更加容易。

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

由於驗證是確保訊息和輸出正確的重要部分，因此每個 `send-*` 函數執行自己的驗證是有意義的。這確保了無論調用哪個輸出，我們總是先檢查輸入。

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

請注意，我們已經從發送訊息函數中刪除了驗證，並將責任轉移到每個單獨的輸出函數。此更改可确保每个目标（GUI、错误控制台、终端）处理自己的验证，简化发送消息功能并使验证逻辑更接近需要的位置。

这种方法可以简化发送消息函数，使其成为_dispatcher_，同时确保每个发送到*函数在处理之前正确验证消息。

透過將驗證轉移到每個 send-to-* 函數中，我們使它們可以作為獨立函數重複使用。这意味着我们可以直接调用任何 send-to-gui、send-to-error-console 或 send-to-terminal 函数，而无需依赖发送消息调度程序函数。现在，这些函数中的每一个都可以完全处理自己的逻辑，并且可以在代码的其他部分或其他插件中独立使用，使您的代码更加模块化和灵活。

## 重構的好處

- **明確的關注點分離**：每個函數現在只處理一項職責，使程式碼更易於理解。
- **可擴展性**：新增新的輸出類型非常簡單。您只需定義一個新函數，例如 `send-to-file` 或 `send-to-logger`，然後在 `cond` 語句中新增一個 case。
- **可重複使用性**：這些輸出處理函數中的每一個都可以在專案的其他地方重複使用或在多個插件之間共用。
- **一致性**：通过在每个 `send-to-*` 函数中重用验证函数，您可以确保所有输出都得到正确验证，从而使代码更加健壮。

重構的庫版本：

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