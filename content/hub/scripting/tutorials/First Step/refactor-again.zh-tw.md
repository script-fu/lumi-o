---
title: "再次重構"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4563817b27aa107aa948c9bb7fb53f358c663dfbc6f070c4a4b725b0d1d600f0
---
隨著幫助程式庫的成長，一目了然地追蹤變得越來越困難。再次重構以保持每個功能較小且單一用途。

### 打破複雜性

為了使該功能更易於遵循和維護，請將其分解為較小的、集中的功能。首先將驗證與訊息路由分開。

### 建立驗證函數

我們可以將驗證 `message` 和 `output` 參數的函數部分移至單獨的函數中。這樣，核心`send-message`函數就不需要擔心驗證，從而更容易理解。

```scheme
(define (validate-message message output)
  ;; 檢查訊息是否為非空字串
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; 檢查輸出是否為預期目標之一
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### 簡化訊息發送

現在驗證已移至單獨的函數，`send-message` 函數可以專注於傳送訊息。它會簡單得多，因為它只處理將訊息定向到正確目的地的特定任務。

```scheme
(define (send-message message output)
  ;; 繼續前先呼叫驗證函式
  (validate-message message output)

  (cond
    ;; 傳送到 Message console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; 傳送到 GUI 對話方塊
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; 傳送到終端機視窗
    ((eq? output 'terminal)
       (display message)))

  ;; 將預設訊息處理常式還原為 Message console
  (lumi-message-set-handler 2))
```

### 進一步細分：分離每個輸出處理程序

每種類型的消息輸出（GUI、訊息控制台、終端）都可以移至自己的函數中。這使得將來的測試、修改和潛在擴展變得更加容易。

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
  ;; 傳送到適當的輸出
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; 將預設訊息處理常式還原為 Message console
  (lumi-message-set-handler 2))
```

### 在每個發送函數中重複使用驗證

由於驗證是確保訊息和輸出正確的重要部分，因此每個 `send-*` 函數執行自己的驗證是有意義的。這確保了無論調用哪個輸出，我們總是先檢查輸入。

```scheme
(define (send-to-gui message)
  ;; 繼續前先驗證訊息
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; 繼續前先驗證訊息
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; 繼續前先驗證訊息
  (validate-message message 'terminal)
  (display message))
```

請注意，我們已經從發送訊息函數中刪除了驗證，並將責任轉移到每個單獨的輸出函數。此變更可確保每個目標（GUI、訊息控制台、終端）處理自己的驗證，簡化傳送訊息功能並使驗證邏輯更接近需要的位置。

這種方法可以簡化發送訊息函數，使其成為_dispatcher_，同時確保每個發送到*函數在處理之前正確驗證訊息。

透過將驗證轉移到每個 send-to-* 函數中，我們使它們可以作為獨立函數重複使用。這意味著我們可以直接呼叫任何 send-to-gui、send-to-error-console 或 send-to-terminal 函數，而無需依賴發送訊息調度程式函數。現在，這些函數中的每一個都可以完全處理自己的邏輯，並且可以在程式碼的其他部分或其他外掛程式中獨立使用，使您的程式碼更加模組化和靈活。

## 重構的好處

- **明確的關注點分離**：每個函數現在只處理一項職責，使程式碼更易於理解。
- **可擴展性**：新增新的輸出類型非常簡單。您只需定義一個新函數，例如 `send-to-file` 或 `send-to-logger`，然後在 `cond` 語句中新增一個 case。
- **可重複使用性**：這些輸出處理函數中的每一個都可以在專案的其他地方重複使用或在多個插件之間共用。
- **一致性**：透過在每個 `send-to-*` 函數中重複使用驗證函數，您可以確保所有輸出都得到正確驗證，從而使程式碼更加健全。

重構的庫版本：

```scheme
;; 用途：向 GUI 對話方塊傳送訊息
(define (send-to-gui message)
  ;; 繼續前先驗證訊息
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; 用途：向 Message console 傳送訊息
(define (send-to-error-console message)
  ;; 繼續前先驗證訊息
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; 用途：向 terminal 視窗傳送訊息
(define (send-to-terminal message)
  ;; 繼續前先驗證訊息
  (validate-message message 'terminal)
  (display message))

;; 用途：將訊息分發到適當的輸出目標
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; 將預設訊息處理常式還原為 Message console
  (lumi-message-set-handler 2))

;; 用途：驗證訊息為非空字串且輸出有效
(define (validate-message message output)
  ;; 檢查訊息是否為非空字串
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; 檢查輸出是否為預期目標之一
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

這就是我們能做的一切嗎？不！還有更多工作要做，請繼續閱讀。