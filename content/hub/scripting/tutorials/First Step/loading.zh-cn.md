---
title: "加载中"
type: docs
weight: 3
---
一旦辅助函数增长，就将其移动到一个小的库文件中。这可以使插件保持专注，并使帮助程序可以在多个插件之间重复使用。

### 创建一个库函数

我们可以使用发送消息函数并以其内容创建一个新文件。将文件保存到您的存储库文件夹中，而不是插件部分，可能靠近顶层；

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**：这是存储Scheme代码的主目录。
  - **library/**：这是 `send-message.scm` 等共享函数的所在。
  - **plug-ins/**：这是存储您的个人插件的位置。
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

库函数 send-message.scm 的示例

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

### 加载库函数

我们可以使用Scheme `load`命令加载该库函数；

加载库文件：

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

嘿！我们现在有了更简单、更短的内容，可以阅读，无需评论即可进行自我描述。这就是重构的令人满意的结论。