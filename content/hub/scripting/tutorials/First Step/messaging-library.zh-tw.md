---
title: "訊息傳遞庫"
type: docs
weight: 6
---
隨著時間的推移，最初作為發送訊息的單一函數已經發展成為相關函數的集合。這些函數現在構成了**訊息傳遞庫**的基礎，旨在處理到不同目的地的輸出，例如 GUI、錯誤控制台和終端。

### 為什麼要使用訊息傳遞庫？

隨著我們需求的成長，跨多個輸出處理訊息需要更加模組化和可擴展的方法。我們不再用單一函數完成所有事情，而是將流程分解為可重複使用的元件，從而實現更大的靈活性。該庫現在可以用作通用訊息傳遞工具，其他外掛程式或函數可以藉用。

### 訊息傳遞庫有什麼作用？

訊息傳遞庫目前包括以下功能：

- **send-to-gui**：將訊息傳送到 Lumi GUI 對話方塊。
- **傳送到錯誤控制台**：將訊息傳送到 Lumi 錯誤控制台。
- **傳送到終端機**：將訊息傳送到終端機視窗。
- **發送訊息**：將訊息定向到適當輸出的調度程序功能。
- **驗證訊息**：在發送之前確保訊息和輸出有效。

### 擴充庫

**訊息傳遞庫**可以輕鬆擴展以支援額外的輸出。例如：

- **傳送到檔案**：將訊息儲存到日誌檔案。
- **傳送到記錄器**：與外部日誌記錄系統整合。
- **傳送到通知**：將訊息顯示為系統通知。

透過遵循相同的模組化設計和可重複使用功能模式，該程式庫可以發展成為處理各種訊息傳遞任務的綜合工具。

## 訊息傳遞庫的好處

- **可重複使用**：功能可以在不同的外掛程式或專案中重複使用。
- **模組化**：每個函數處理一項特定任務，使程式碼更易於維護和擴展。
- **一致性**：使用相同的驗證和訊息處理函數可確保整個應用程式的行為一致。

**訊息傳遞庫**是更廣泛框架的開始，可以簡化專案中訊息的管理方式。隨著庫的增長，新的插件可以輕鬆地利用它來將訊息發送到任何需要的地方。

我們可以調整文件結構：

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

並且記得調整主插件中的`load`：

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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