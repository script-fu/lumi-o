---
title: "過濾器插件"
type: docs
weight: 2
---
我們在[First Step](../../first-step/) 教學中使用了_procedure_ 外掛程式。這些類型的插件無需圖像或可繪製物件作為輸入即可運作。通常，我們使用插件來更改圖像及其可繪製物件。像這樣的插件稱為_filter_插件。

### 什麼是可繪製物件？

Lumi 中的 **drawable** 是指可以在其上繪製的圖像元素，例如圖層或通道。過濾器插件通常會對這些元素進行操作。

### 一個簡單的過濾器外掛範例

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

複製文字並將其以 `simple-filter-plug-in.scm` 形式保存在 Lumi 插件資料夾之一名為 `simple-filter-plug-in` 的資料夾中。 Lumi 插件資料夾是下面列出的 _any_ 資料夾：
 **Lumi > 編輯 > 首選項 > 資料夾 > 插件**

在 Linux 中，右鍵點擊 `simple-filter-plug-in.scm` 文件，前往 **屬性 > 權限**，然後選取 **允許將檔案作為程式執行**。一旦檔案位於正確的位置、可執行且沒有語法錯誤，當 Lumi 重新啟動時，它將出現在頂部選單標題列中名為 **插件** 的選單內。

### 運行插件

1. 開啟一張圖片（此濾鏡外掛程式需要圖片才能運作）。
2. 開啟 **Windows > 可停靠對話方塊 > 錯誤控制台** 以檢視訊息。
3. 從 **插件** 選單中選擇 **簡單過濾器插件演示**。
4. 所選圖層之一的顏色將會反轉，並且一則訊息將會列印到錯誤控制台。

### 編輯插件

您可以透過編輯 `.scm` 檔案來自訂外掛程式。例如，要變更顯示的訊息：

1. 開啟檔案並找到定義 `message` 的行。
2. 將`"hello, world"` 替換為您的自訂文字。
3. 儲存文件。

在 Lumi 版本 3 中，插件不需要刷新即可使已儲存的變更生效。只需重新運行插件即可查看更新的訊息。

### 外掛程式檢查

#### 舍邦線

第一行確保腳本在 Lumi 3 中作為插件運行：

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### 流程定義

此過程接受兩個參數：活動影像和選定的可繪製物件。

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### 核心邏輯

`let` 語句定義一個變數並對可繪製物件執行操作。

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### 外掛程式註冊

該插件註冊到Lumi作為濾鏡插件：

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### 選單註冊
此行指定插件的選單位置：

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### 故障排除

如果外掛程式未出現，請檢查其位置、名稱和可執行屬性。

該位置必須位於插件搜尋路徑中。
檔案名稱必須與包含資料夾的名稱相符。
該文件必須設定為可執行檔。


**錯誤控制台**是用於對自訂外掛程式進行故障排除的寶貴工具。如果您的外掛程式未如預期運行，請在此處檢查錯誤訊息或日誌。 **終端機**視窗也可以提供偵錯資訊並報告載入問題。