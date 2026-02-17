---
title: "載入中"
type: docs
weight: 3
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
;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 載入函式庫函數

我們可以使用Scheme `load`命令來載入該函式庫函數；

載入庫檔案：

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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