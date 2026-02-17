---
title: "过程浏览器"
type: docs
weight: 1
---
**Lumi 过程浏览器** 允许您搜索可用的过程（内置和提供的插件）并检查它们的参数和返回值。

### 在哪里可以找到 Lumi 程序浏览器

您可以通过 **帮助** 菜单访问 Lumi 中的过程浏览器：

- **帮助** -> **程序浏览器**

### 过程浏览器的作用

程序浏览器列出了 Lumi 的所有内部程序，以及通过插件添加的程序，包括您刚刚安装的程序。每个过程条目都提供有用的信息，包括：

- 过程名称。
- 对其作用的描述。
- 它接受的参数（输入值）。
- 返回值（输出）。

当您需要验证调用签名或确认确切的过程名称时，可以按关键字或过程名称进行搜索。

程序浏览器中的 #### (lumi-message)

搜索`lumi-message` 以查看其参数和返回值。

### 找到你的插件

安装“Hello World!”后插件，您可以在过程浏览器中找到它。只需搜索您在 Lumi 中注册的函数名称，在本例中为“scheme-hello-world”。该条目将显示与插件关联的参数和任何返回值，以及简要说明。您还将看到您在注册过程中作为输入参数输入的一些文本行显示在“**附加信息**”部分下。

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

这样可以轻松验证您的插件是否已正确注册，并为您提供了一种快速方法来查看它如何与 Lumi 中的其他程序交互。过程浏览器是一个强大的工具，用于通过探索 Lumi 中的所有可用过程来调试和扩展您的插件。