---
title: "傳回值"
type: docs
weight: 8
---
傳回值很重要，因為它們可以讓您控制流程而無需額外的狀態。在Scheme中，最後計算的表達式成為回傳值。

此頁面使用訊息傳遞範例中的驗證助手來展示明確傳回值如何使程式碼更易於編寫。

### 什麼是回傳值？

在Scheme中，函數的傳回值由函數計算的最後一個表達式決定。這意味著函數中最後一行程式碼的計算結果將作為函數的結果傳回。如果未明確傳回任何值，則函數將傳回 `#f` (false) 或 `undefined`。

讓我們回顧一下驗證函數，（is-valid-string？）

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

在此函數中，如果訊息無效，則會引發錯誤。但是，如果訊息有效，則不會給出明確傳回值，函數預設回傳`#f`。

### 使回傳值明確化

我們可以透過使返回值更加明確來改進這一點。例如，如果訊息有效，我們可以返回 `#t` (true)：

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

在此版本中，當訊息有效時，函數將傳回`#t`，提供明確的結果。這使得該函數可以在需要布林結果的其他上下文中更靈活地使用。

### 有效使用回傳值

透過決定我們的函數返回什麼，我們可以使它們更加可預測和有用。返回 `#t`、`#f` 等值或特定結果使我們能夠更好地控制函數與其餘程式碼的互動方式。例如，您可以使用返回值在呼叫函數中做出進一步的決定或將其作為參數傳遞給另一個函數。

這是一個使用傳回值來控制邏輯流程的簡單範例：

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

在這種情況下，(send-message)依賴(is-valid-output-display?)的回傳值來決定是否繼續。
如果第一次測試失敗，將跳過條件語句`cond`。另外，請注意它如何以相當自然的方式讀取，如果輸出顯示有效？

## Scheme 中的 If 語句邏輯

在重構庫範例之前，先快速回顧一下條件邏輯。方案使用 `if` 在兩條路徑之間進行選擇。

以下是 `if` 語句的簡單形式：

```scheme
(if (conditional test)
  do if true
  do if false)
```

此結構檢查條件，如果條件為真，則執行第一個操作。如果條件為假，則執行第二個操作。

如果您需要在條件為 true 或 false 時執行多個操作，可以使用 `begin` 將它們組合在一起：

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

這使您可以處理更複雜的情況，其中需要根據條件測試的結果執行多個表達式或語句。

好的，這是嵌入返回值的庫程式碼，用於控制執行過程。

### 用回傳值重構

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

## 結論

傳回值是使函數靈活和可重複使用的基本部分。透過仔細決定每個函數應該返回什麼，我們可以確保我們的函數彼此良好地交互，並為其餘程式碼提供有用的信息。無論是返回`#t`或`#f`，或是更具體的東西，返回值都為我們提供了一種控製程式流程並處理各種結果的方法。