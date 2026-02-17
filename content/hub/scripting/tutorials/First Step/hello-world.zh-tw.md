---
title: "你好世界！"
type: docs
weight: 1
---
本教學將介紹Scheme 插件的最小結構。有些行是「樣板檔案」：Lumi 載入檔案需要它們，即使您還沒有完全理解它們。

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

在高層次上，您將：

1. 定義一個函數
2. 註冊它，使其出現在過程資料庫中
3.（可選）新增選單項
4. 將檔案安裝到插件資料夾中

### 定義一個函數

函數，也稱為_procedure_，是具有名稱和用途的一段程式碼，它接受輸入並產生輸出。

**輸入** > **_函數_** > **輸出**

### 註冊函數

註冊是將函數名稱放在列表中以便 Lumi 知道的行為。

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### 選單鏈接

這告訴 Lumi 在其選單系統中哪裡可以找到您的功能。

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

這將在主選單欄中顯示選單“Funky”。更改路徑以將插件放置在其他位置。路徑`<Image>/Funky`表示插件將出現在**圖像**選單類別下。您可以將 `<Image>` 更改為 `<Tools>`、`<Filters>` 等，取決於您希望插件出現的位置。

### 評論

在Scheme（Scheme 的基本語言）中，註釋通常是透過在有用的文字行前加上`;;` 來完成的。您對註釋的使用取決於您作為編碼員的流暢程度 - 如果您偶爾編碼，更多註釋會有所幫助。如果您一直編碼，那麼程式碼就像註釋一樣容易閱讀。此外，在進行函數式程式設計時，程式碼往往具有足夠的描述性，可以像腳本一樣閱讀。

### 語法

程式碼對於如何將項目放置在一行中往往沒有任何規則，以便我們可以輕鬆地閱讀該行。例如，句子中逗號或句號後面可能有空格。它有助於可讀性。

程式碼可能會以類似的方式安排事物，乍看之下可能看起來很奇怪：

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## 範例程式碼

這是完整的範例。大多數 Lumi 過程都以 `lumi-` 為前綴。例如，`lumi-message` 將字串列印到配置的訊息處理程序。

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


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

### 安裝插件

1. 前往 **Lumi -> 編輯 -> 首選項 -> 資料夾 -> 外掛程式**。
2. 將[repo](/hub/scripting/tools/git) 外掛程式資料夾新增至清單。
3. 為外掛程式建立一個資料夾，並將上面的範例程式碼儲存為`hello-world.scm`：
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. 右鍵點選`hello-world.scm` 檔案。
5. 前往**屬性 -> 權限 -> 允許將檔案作為程式執行**。
6. 重新啟動 Lumi。

### 嘗試插件

該插件現在應該出現在 Lumi 主視窗的“Funky”選單下。點擊它，它應該顯示“Hello world！”訊息。嘗試修改程式碼，例如更改訊息文本，然後儲存檔案。當您再次運行該插件時，您的更改將得到反映，而無需重新啟動 Lumi。

嘗試透過更改選單路徑進行試驗。例如，`"<Image>/File"` 會將其放入「檔案」選單中，`"<Image>/File/Funky"` 將在「檔案」選單中建立新部分。這是自訂插件顯示位置和組織工具的好方法。