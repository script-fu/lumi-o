---
title: "驗證"
type: docs
weight: 4
---
在建立強大的插件時，重要的是要確保我們的函數能夠優雅地處理錯誤並按預期工作，即使在誤用或意外輸入的情況下也是如此。驗證有助於保護功能的完整性並防止崩潰或意外行為。

讓我們看看如何透過新增驗證檢查來改進 `send-message` 函數，以確保它正確處理輸入。

### 驗證輸入

在發送訊息之前，我們應該確保傳遞給`send-message`函數的`output`參數是有效的。我們可以新增一個檢查來確認輸出目標是預期值之一（gui、錯誤控制台或終端）。

範例：

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

在此範例中，我們使用 `member` 檢查 `output` 參數是否有效。如果不是，函數會引發錯誤並顯示明確的訊息，以防止無效值造成問題。

### 處理空訊息

確保 `message` 參數有效也很有用。例如，如果將空字串或 #f (false) 作為訊息傳遞，則函數應妥善處理此問題。

處理空訊息的範例：

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

這種方法可確保函數始終接收有效輸入，從而提高其可靠性並防止意外行為。

### 組合驗證範例

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
- 函數首先檢查`message` 是否為空或無效。如果訊息有效，則繼續檢查 `output` 是否為可接受的值之一（`gui`、`error-console` 或 `terminal`）。
- 如果兩項檢查都通過，則訊息將發送到適當的輸出。否則，會出現錯誤訊息並給予明確的解釋。
- 進行額外檢查以確保訊息也是字串。

這種組合的驗證功能使程式碼更加清晰，並確保在採取任何操作之前驗證兩個輸入，從而使該功能更加健壯。請注意，我們也建置了一個偵錯訊息系統。當
程式碼失敗，我們得到一個原因，一個我們自己寫的原因。

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```