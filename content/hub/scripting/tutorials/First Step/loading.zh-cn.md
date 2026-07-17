---
title: "加载中"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
一旦辅助函数增长，就将其移动到一个小的库文件中。这可以使插件保持专注，并使帮助程序可以在多个插件之间重复使用。

### 创建一个库函数

我们可以使用发送消息函数并以其内容创建一个新文件。 Save the file into your repo folder, not the plugins part, perhaps near the top level;

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
;; 处理向各种目标输出消息的函数
(define (send-message message output)
  (cond
    ;; 发送到 Message console
    ((eq? output 'error-console)
       ;; 将处理程序设置为 Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; 发送到 GUI 对话框
    ((eq? output 'gui)
       ;; 将处理程序设置为 GUI 对话框
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; 发送到终端窗口
    ((eq? output 'terminal)
       ;; terminal 输出通过 display 处理
       (display message)))

  ;; 将默认消息处理程序恢复为 Message console
  (lumi-message-set-handler 2))
```

### 加载库函数

We can load that library function with the Scheme `load` command;

加载库文件：

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

嘿！我们现在有了更简单、更短的内容，可以阅读，无需评论即可进行自我描述。 This is the satisfying conclusion of refactoring.