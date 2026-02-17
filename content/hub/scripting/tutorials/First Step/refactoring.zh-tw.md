---
title: "重構"
type: docs
weight: 2
---
一旦我們的函數可以工作，我們就可以退一步思考如何最好地建立我們的程式碼。目標是使我們的插件盡可能清晰、易於理解和可維護。這種在不改變現有程式碼行為的情況下改進和細化現有程式碼結構的過程稱為重構。

這是初始函數：

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

function-name 是函數的名稱，parameter 是函數接受的輸入內容。主體是呼叫函數時運行的程式碼區塊。

摘要形式：

```scheme
(define (function-name parameter)
  body)
```

### 程式碼重複

儘早消除重複。 `(lumi-message "Hello world!\n")` 重複兩次，訊息字串重複三次。變數解決了重複的字串。

### 變數

在Scheme中，變數有一個已知的“範圍”，且該範圍是使用`let`語句設定的。該變數在綁定部分綁定到一個值，並且該變數在let主體中具有作用域。此變數僅在 let 區塊內部已知，無法在其外部存取。

```scheme
(let ((variable value))
  body)
```

引入一個名為“message”的變數：

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

在我們的範例中，我們使用了一個名為「message」的變量，該變數綁定到字串「Hello world!\n」。這使我們可以更改訊息內容一次而不是三次，從而減少了出錯的機會並使程式碼更加靈活。

### 提取函數

在函數式程式設計中，重構程式碼以將可重複使用邏輯提取到單獨的函數中是一種常見的做法。透過這樣做，**主函數**變得更加簡單，並且更加專注於其高級目標，而**提取函數**顯得更加複雜，因為它處理詳細的邏輯。這是有意為之的，並且符合函數式程式設計的核心原則，例如模組化、關注點分離和可讀性。這是重構的
世界你好！提取後。

提取邏輯：
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

#### 符號
在上面的範例中，使用了一種稱為符號的資料類型，例如“gui”。符號作為參數傳遞給發送訊息函數，可用於做出簡單的條件決策。與符號鍵一樣，它們是唯一識別碼。有關符號的更多信息，請訪問[this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### 簡化主要功能

在原始 (scheme-hello-world) 函數中，將訊息傳送到不同輸出（GUI、錯誤控制台、終端）的所有邏輯都混合到主函數中。重構後，主函數只是專注於**需要做什麼**，將訊息傳送到不同的目的地。

重構後的main函數更加簡單：

- 它清楚地說明了其目的：將相同的訊息發送到多個輸出。
- 它避免了重複程式碼使主邏輯混亂，例如為不同的輸出設定訊息處理程序。
- 一目了然，更容易閱讀和理解。

### 提取函數的複雜度

相反，**（發送訊息）函數**是詳細邏輯所在的地方。現在它可以處理每個輸出（GUI、錯誤控制台、終端）的行為變化。功能比以前複雜一點，但現在是**集中**和**隔離**。

## 將其與函數式程式設計連結起來

在函數式程式設計中，函數被視為**一等公民**，這意味著它們可以重複使用、傳遞和組合以形成更複雜的行為。目標是：- **將問題**分解為較小的、獨立的部分。
- **將複雜性**隔離為處理特定任務的較小函數，例如`send-message`。
- **保持較高層級的功能簡單**，以便他們可以專注於編排資料流和操作，而無需了解每個任務如何完成的詳細資訊。
- **關注點分離**：此函數根據輸出類型負責如何傳送訊息，從而將此邏輯與主函數隔離。
- **模組化**：透過在一個地方處理所有訊息發送邏輯，我們可以輕鬆地進行更改（例如添加新的輸出選項），而無需更改主功能。
- **可重用性**：`send-message` 函數是可重複使用的，這表示如果我們需要將訊息傳送到程式碼中其他位置的多個輸出，我們可以簡單地呼叫此函數，而不會重寫類似的邏輯。

透過重構，此範例中的主函數變成了正在發生的事情的**聲明性**語句（「向三個地方發送訊息」），而如何發送這些訊息的複雜性則被抽像到 `send-message` 函數中。