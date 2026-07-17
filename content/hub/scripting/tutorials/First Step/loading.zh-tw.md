---
title: "載入中"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
一旦輔助函數增長，就將其移動到一個小的庫檔案中。這可以使插件保持專注，並使幫助程序可以在多個插件之間重複使用。

### 建立一個庫函數

我們可以使用發送訊息函數並以其內容建立一個新檔案。將檔案儲存到您的儲存庫資料夾中，而不是插件部分，可能靠近頂層；

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**：這是儲存Scheme程式碼的主目錄。
  - **library/**：這是 `send-message.scm` 等共享函數的所在。
  - **plug-ins/**：這是儲存您的個人插件的位置。
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

庫函數 send-message.scm 的範例

```scheme
;; 處理向各種目標輸出訊息的函式
(define (send-message message output)
  (cond
    ;; 傳送到 Message console
    ((eq? output 'error-console)
       ;; 將處理常式設定為 Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; 傳送到 GUI 對話方塊
    ((eq? output 'gui)
       ;; 將處理常式設定為 GUI 對話方塊
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; 傳送到終端機視窗
    ((eq? output 'terminal)
       ;; terminal 輸出透過 display 處理
       (display message)))

  ;; 將預設訊息處理常式還原為 Message console
  (lumi-message-set-handler 2))
```

### 載入函式庫函數

We can load that library function with the Scheme `load` command;

載入庫檔案：

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

嘿！我們現在有了更簡單、更短的內容，可以閱讀，無需評論即可進行自我描述。 This is the satisfying conclusion of refactoring.