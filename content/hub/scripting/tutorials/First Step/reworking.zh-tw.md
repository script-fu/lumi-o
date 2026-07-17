---
title: "返工"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 324662811965905bad18a135ac343a3eb8120da180149b19bc212a6af61a4bb7
---
此步驟修復了訊息傳遞範例中的一個微妙行為。

我們傳遞字串“Hello world!\n”作為訊息。 “\n”是一種特殊的字符，是“轉義”字符。它告訴輸出列印開始換行。在Scheme中，它還會強制發送到狀態列的訊息以GUI框的形式彈出。

幫助器 `send-to-gui` 將訊息傳送到 Lumi 對話框。

更新訊息內容和目標，以便範例行為一致。

刪除轉義字元並擴展功能：
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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

將幻數替換為 Lumi 提供的常數（例如 `MESSAGE-BOX` 和 `ERROR-CONSOLE`）。

然後將驗證拆分為兩個函數，以便可以從多個呼叫站點重複使用。

- (is-valid-string?) 在 send-to* 函數中檢查字串是否為字串而不是空字串。
- (is-valid-output-display?) 在傳送訊息函數中檢查給定的輸出目的地是否有效。

重新設計庫：

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; 附加換行以強制顯示訊息方塊
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

;; 用途：將訊息分發到適當的輸出目標
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; 用途：驗證訊息為非空字串
(define (is-valid-string? message)
  ;; 檢查訊息是否為非空字串
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; 用途：驗證訊息是否傳送到有效輸出
(define (is-valid-output-display? output)
  ;; 檢查輸出是否為預期的顯示目標之一
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## 結論

透過重新設計我們的訊息傳遞庫，我們使其更加強大和可靠。我們修復了換行符的隱藏問題，引入了常數以提高清晰度，並透過添加對狀態列和對話方塊輸出的支援來擴展功能。此外，將驗證邏輯分成更小的、集中的函數可以確保我們的程式碼在將來更容易維護和擴展。

這次返工展示了微小的變化如何增強我們庫的整體結構和功能，為隨著我們專案的發展提供更大的靈活性和可重用性鋪平道路。