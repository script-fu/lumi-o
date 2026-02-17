---
title: "过滤器插件"
type: docs
weight: 2
---
我们在[First Step](../../first-step/) 教程中使用了_procedure_ 插件。这些类型的插件无需图像或可绘制对象作为输入即可工作。通常，我们使用插件来更改图像及其可绘制对象。像这样的插件称为_filter_插件。

### 什么是可绘制对象？

Lumi 中的 **drawable** 是指可以在其上绘制的图像元素，例如图层或通道。过滤器插件通常对这些元素进行操作。

### 一个简单的过滤器插件示例

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

复制文本并将其以 `simple-filter-plug-in.scm` 形式保存在 Lumi 插件文件夹之一名为 `simple-filter-plug-in` 的文件夹中。 Lumi 插件文件夹是下面列出的 _any_ 文件夹：
 **Lumi > 编辑 > 首选项 > 文件夹 > 插件**

在 Linux 中，右键单击 `simple-filter-plug-in.scm` 文件，转到 **属性 > 权限**，然后选中 **允许将文件作为程序执行**。一旦文件位于正确的位置、可执行且没有语法错误，当 Lumi 重新启动时，它将出现在顶部菜单标题栏中名为 **插件** 的菜单内。

### 运行插件

1. 打开一张图片（此滤镜插件需要图片才能工作）。
2. 打开 **Windows > 可停靠对话框 > 错误控制台** 以查看消息。
3. 从 **插件** 菜单中选择 **简单过滤器插件演示**。
4. 所选图层之一的颜色将反转，并且一条消息将打印到错误控制台。

### 编辑插件

您可以通过编辑 `.scm` 文件来自定义插件。例如，要更改显示的消息：

1. 打开文件并找到定义 `message` 的行。
2. 将`"hello, world"` 替换为您的自定义文本。
3. 保存文件。

在 Lumi 版本 3 中，插件不需要刷新即可使保存的更改生效。只需重新运行插件即可查看更新的消息。

### 插件检查

#### 舍邦线

第一行确保脚本在 Lumi 3 中作为插件运行：

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### 过程定义

该过程接受两个参数：活动图像和选定的可绘制对象。

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### 核心逻辑

`let` 语句定义一个变量并对可绘制对象执行操作。

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### 插件注册

该插件注册到Lumi作为滤镜插件：

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### 菜单注册
此行指定插件的菜单位置：

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### 故障排除

如果插件未出现，请检查其位置、名称和可执行属性。

该位置必须位于插件搜索路径中。
文件名必须与包含文件夹的名称匹配。
该文件必须设置为可执行文件。


**错误控制台**是用于对自定义插件进行故障排除的宝贵工具。如果您的插件未按预期运行，请在此处检查错误消息或日志。 **终端**窗口还可以提供调试信息并报告加载问题。