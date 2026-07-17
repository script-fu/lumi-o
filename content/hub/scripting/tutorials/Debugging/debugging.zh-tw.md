---
title: "偵錯"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
---
在腳本編寫中，沒有任何函數是絕對正確的。當遇到意外的輸入或條件時，即使是最可靠的命令也可能會失敗。為了防止這種情況，我們可以實現自訂偵錯系統並採用防禦性程式設計技術。透過用錯誤處理機制包裝標準函數並提供資訊回饋，我們可以使我們的腳本更加健壯並且更容易排除故障。

該策略的關鍵部分是使用全域偵錯標誌來控制詳細輸出，使我們能夠在需要時啟用詳細的偵錯訊息，同時在正常執行期間保持輸出乾淨。

## 全域偵錯標誌

全域偵錯標誌是控制腳本執行期間資訊輸出等級的簡單而有效的方法。啟用後，它會提供詳細的偵錯訊息，這對於追蹤問題非常有用。禁用後，它可以保持輸出簡潔以供生產使用。

```scheme
;; 用途：控制偵錯輸出的全域旗標。
(define debug #f)
```

預設情況下，調試是關閉的。若要在開發期間啟用詳細輸出，只需將標誌設為`#t`：

```scheme
;; 用途：控制偵錯輸出的全域旗標。
(define debug #t)
```

我們也可以使用輔助函數臨時啟用或停用特定程式碼部分的偵錯。

### 本機偵錯控制

為了更好地控制，我們可以使用輔助函數在腳本的特定部分中開啟或關閉偵錯。

```scheme
;; 用途：關閉某段程式碼的偵錯模式。
(define (debug-off)
  (set! debug #f))

;; 用途：開啟某段程式碼的偵錯模式。
(define (debug-on)
  (set! debug #t))
```

這使我們能夠動態控制調試：

```scheme
(debug-on)  ;; 啟用詳細輸出

;; 此處為一些指令碼邏輯

(debug-off) ;; 停用詳細輸出
```

## 偵錯訊息系統

為了有效地處理Scheme中的偵錯輸出，我們使用了涉及多個輔助函數的結構化方法。這些功能可確保偵錯和警告訊息清晰、可讀且可維護。

### 偵錯訊息系統概述

我們的偵錯訊息系統由以下元件組成：

1. `debug-message` – 啟用偵錯時顯示偵錯訊息。
2. `serialize-item` – 將各種Scheme 資料型別轉換為字串表示形式。
3. `concat` – 將多個項目連接成一個字串。
4. `list->string` – 將清單格式化為可讀字串。
5. `message` – 在 Lumi 的訊息控制台中顯示輸出。
6. `warning-message` – 啟用警告時顯示警告訊息。

每個函數都在格式化和顯示結構化訊息方面發揮作用。

---

### 偵錯訊息功能

`debug-message` 函數是顯示偵錯輸出的核心方法。它確保僅在啟用偵錯時顯示訊息。

```scheme
;; 用途：顯示偵錯訊息。
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug` 條件確保僅在啟用偵錯時才顯示訊息。
- 為了清楚起見，訊息以 `"> "` 為前綴。
- 函數使用`concat`來格式化訊息內容。
- 最後，它呼叫 `message` 將輸出傳送到 Lumi 的訊息控制台。

用法範例：

```scheme
;; 用途：回傳項目的樹狀位置，若項目無效則回傳 #f
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

啟用調試後，輸出可能是：

```scheme
> item: background-layer has tree position : 3
```

### 序列化偵錯訊息數據

訊息可能包含不同的資料類型，例如清單、向量和數字。為了確保它們的格式正確，我們使用`serialize-item`。

```scheme
;; 用途：轉換各種 Scheme 資料型別（清單、向量、配對等）
;;          轉換為字串表示。
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; 空清單
    ((and (string? item) (string=? item "")) "\"\"")  ; 空字串
    ((list? item) (list->string item))                ; 巢狀清單
    ((vector? item)                                   ; 處理向量
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; 處理配對
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; 數字
    ((symbol? item) (symbol->string item))            ; 符號
    ((boolean? item) (if item "#t" "#f"))             ; 布林值
    ((string? item) item)                             ; 字串
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

用法範例：

```scheme
(serialize-item '(1 2 3))
```

輸出：

```scheme
list:
1
2
3
```

### 訊息串聯

要將多個訊息組件合併為單一字串，我們使用`concat`。

```scheme
;; 用途：將多個項目連接成單一字串。
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

用法範例：

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### 將清單格式化為字串

`list->string` 函數將清單轉換為格式化字串。

```scheme
;; 用途：將項目清單轉換為可讀字串。
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### 警告訊息`warning-message` 函數的工作方式與 `debug-message` 類似，但即使停用偵錯，它也會顯示警告。

```scheme
;; 用途：顯示警告訊息。
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- 確保僅在啟用警告時顯示訊息（`warning` 標誌在`common.scm` 中設定為`#t`）。
- 呼叫`concat`來格式化訊息內容。
- 使用`message` 將輸出傳送到Lumi。

## 增強標準功能

一旦調試系統到位，我們就可以透過合併詳細訊息來增強我們的函數庫。這提供了對專案狀態、變數值和函數呼叫的深入了解。

一個常見的範例是`item-is-valid?`，它包裝`lumi-item-id-is-valid`以返回`#t`或`#f`。如果返回`#f`，我們可以在呼叫程式碼中觸發`warning-message`，如果輸入不是數字，我們可以在函數中發出警告。

```scheme
;; 用途：檢查項目是否有效，回傳 #t 或 #f。
;;          若項目不是數字則發出警告。
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## 實際使用

在開發Scheme插件時，以這種方式包裝函數可以顯著減少偵錯時間並確保程式碼的健全性、可維護性。有了我們的偵錯系統，我們只需輕按一下開關就可以在錯誤控制台中產生結構化的偵錯流。

在此偵錯流中，函數呼叫標有星號 (*)，從而更容易追蹤腳本執行和找出故障，特別是在複雜的插件中。這種可見性有助於我們了解操作流程並有效診斷意外行為。

我們的訊息函數的包裝器使用 `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

`call` 實際使用的範例：

```scheme
;; 用途：對給定的群組遮罩清單套用紋理處理
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

作為插件執行的調試流範例：

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

這種結構化日誌提供了函數呼叫和資料變更的清晰時間表，使偵錯和效能分析變得更加容易。

## 結論

透過實施結構化偵錯系統，我們創建了更安全、更易於維護的腳本，可以即時洞察其執行情況。

### 要點

- **控制詳細程度** – 使用全域偵錯標誌來管理輸出等級。
- **提供清晰的回饋** – 用資訊豐富的調試訊息包裝標準功能。
- **增強穩健性** – 妥善處理意外輸入以防止錯誤。
- **簡化故障排除** – 結構化偵錯訊息使診斷和修復問題變得更加容易。

透過這種方法，我們的腳本在處理資料時可以有效地“自我解釋”，從而減少挫折感並提高工作流程效率。調試成為一種主動的工具，而不是被動的苦差事，使我們的腳本編寫過程更加順利和更有價值。