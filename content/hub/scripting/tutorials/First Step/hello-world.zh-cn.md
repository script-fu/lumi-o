---
title: "你好世界！"
type: docs
weight: 1
---
本教程将介绍Scheme 插件的最小结构。有些行是“样板文件”：Lumi 加载文件需要它们，即使您还没有完全理解它们。

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

在高层次上，您将：

1. 定义一个函数
2. 注册它，使其出现在过程数据库中
3.（可选）添加菜单项
4. 将文件安装到插件文件夹中

### 定义一个函数

函数，也称为_procedure_，是具有名称和用途的一段代码，它接受输入并产生输出。

**输入** > **_函数_** > **输出**

### 注册函数

注册是将函数名称放在列表中以便 Lumi 知道的行为。

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### 菜单链接

这告诉 Lumi 在其菜单系统中哪里可以找到您的功能。

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

这将在主菜单栏中显示菜单“Funky”。更改路径以将插件放置在其他位置。路径`<Image>/Funky`表示插件将出现在**图像**菜单类别下。您可以将 `<Image>` 更改为 `<Tools>`、`<Filters>` 等，具体取决于您希望插件出现的位置。

### 评论

在Scheme（Scheme 的基本语言）中，注释通常是通过在有用的文本行前加上`;;` 来完成的。您对注释的使用取决于您作为编码员的流畅程度 - 如果您偶尔编码，更多注释会有所帮助。如果您一直编码，那么代码就像注释一样容易阅读。此外，在进行函数式编程时，代码往往具有足够的描述性，可以像脚本一样阅读。

### 语法

代码对于如何将项目放置在一行中往往没有什么规则，以便我们可以轻松地阅读该行。例如，句子中逗号或句号后可能有空格。它有助于可读性。

代码可能会以类似的方式安排事物，乍一看可能看起来很奇怪：

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## 示例代码

这是完整的示例。大多数 Lumi 过程都以 `lumi-` 为前缀。例如，`lumi-message` 将字符串打印到配置的消息处理程序。

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

### 安装插件

1. 转到 **Lumi -> 编辑 -> 首选项 -> 文件夹 -> 插件**。
2. 将[repo](/hub/scripting/tools/git) 插件文件夹添加到列表中。
3. 为插件创建一个文件夹，并将上面的示例代码保存为`hello-world.scm`：
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. 右键单击`hello-world.scm` 文件。
5. 转到**属性 -> 权限 -> 允许将文件作为程序执行**。
6. 重新启动 Lumi。

### 尝试插件

该插件现在应该出现在 Lumi 主窗口的“Funky”菜单下。单击它，它应该显示“Hello world！”信息。尝试修改代码，例如更改消息文本，然后保存文件。当您再次运行该插件时，您的更改将得到反映，而无需重新启动 Lumi。

尝试通过更改菜单路径进行试验。例如，`"<Image>/File"` 会将其放入“文件”菜单中，`"<Image>/File/Funky"` 将在“文件”菜单中创建一个新部分。这是自定义插件显示位置和组织工具的好方法。