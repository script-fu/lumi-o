---
title: "消息传递库"
type: docs
weight: 6
---
随着时间的推移，最初作为发送消息的单个函数已经发展成为相关函数的集合。这些函数现在构成了**消息传递库**的基础，旨在处理到不同目的地的输出，例如 GUI、错误控制台和终端。

### 为什么要使用消息传递库？

随着我们需求的增长，跨多个输出处理消息需要更加模块化和可扩展的方法。我们不再用单一函数完成所有事情，而是将流程分解为可重用的组件，从而实现更大的灵活性。该库现在可以用作通用消息传递工具，其他插件或函数可以借用。

### 消息传递库有什么作用？

消息传递库目前包括以下功能：

- **send-to-gui**：将消息发送到 Lumi GUI 对话框。
- **发送到错误控制台**：将消息发送到 Lumi 错误控制台。
- **发送到终端**：将消息发送到终端窗口。
- **发送消息**：将消息定向到适当输出的调度程序功能。
- **验证消息**：在发送之前确保消息和输出有效。

### 扩展库

**消息传递库**可以轻松扩展以支持额外的输出。例如：

- **发送到文件**：将消息保存到日志文件。
- **发送到记录器**：与外部日志记录系统集成。
- **发送到通知**：将消息显示为系统通知。

通过遵循相同的模块化设计和可重用功能模式，该库可以发展成为处理各种消息传递任务的综合工具。

## 消息传递库的好处

- **可重用性**：功能可以在不同的插件或项目中重用。
- **模块化**：每个函数处理一项特定任务，使代码更易于维护和扩展。
- **一致性**：使用相同的验证和消息处理函数可确保整个应用程序的行为一致。

**消息传递库**是更广泛框架的开始，可以简化项目中消息的管理方式。随着库的增长，新的插件可以轻松地利用它来将消息发送到任何需要的地方。

我们可以调整文件结构：

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

并且记得调整主插件中的`load`：

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