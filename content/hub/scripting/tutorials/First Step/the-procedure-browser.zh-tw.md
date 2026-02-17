---
title: "過程瀏覽器"
type: docs
weight: 1
---
**Lumi 過程瀏覽器** 可讓您搜尋可用的流程（內建和提供的插件）並檢查它們的參數和傳回值。

### 在哪裡可以找到 Lumi 程式瀏覽器

您可以透過 **幫助** 選單存取 Lumi 中的過程瀏覽器：

- **幫助** -> **程式瀏覽器**

### 過程瀏覽器的作用

程序瀏覽器列出了 Lumi 的所有內部程序，以及透過插件添加的程序，包括您剛剛安裝的程序。每個過程條目都提供有用的信息，包括：

- 過程名稱。
- 對其作用的描述。
- 它接受的參數（輸入值）。
- 傳回值（輸出）。

當您需要驗證呼叫簽名或確認確切的過程名稱時，可以按關鍵字或過程名稱進行搜尋。

程式瀏覽器中的 #### (lumi-message)

搜尋`lumi-message` 以查看其參數和返回值。

### 找到你的插件

安裝“Hello World!”後插件，您可以在過程瀏覽器中找到它。只需搜尋您在 Lumi 中註冊的函數名稱，在本例中為「scheme-hello-world」。該條目將顯示與插件關聯的參數和任何返回值，以及簡要說明。您還將看到您在註冊過程中作為輸入參數輸入的一些文字行顯示在“**附加資訊**”部分下。

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

這樣可以輕鬆驗證您的插件是否已正確註冊，並為您提供了一種快速方法來查看它如何與 Lumi 中的其他程式互動。過程瀏覽器是一個強大的工具，用於透過探索 Lumi 中的所有可用流程來調試和擴展您的插件。