---
title: "載入中"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
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
    - **hello-world/**: 特定「Hello World!」plug-in 的資料夾。
      - **hello-world.scm**: plug-in 的指令碼檔案。

庫函數 send-message.scm 的範例

```scheme
;; 處理向各種目標輸出訊息的函式
(define (send-message message output)
  (cond
    ;; 傳送到 訊息主控台
    ((eq? output 'error-console)
       ;; 將處理常式設定為 訊息主控台
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

  ;; 將預設訊息處理常式還原為 訊息主控台
  (lumi-message-set-handler 2))
```

### 載入函式庫函數

我們可以使用Scheme `load`命令來載入該函式庫函數；

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

嘿！我們現在有了更簡單、更短的內容，可以閱讀，無需評論即可進行自我描述。這就是重構的令人滿意的結論。