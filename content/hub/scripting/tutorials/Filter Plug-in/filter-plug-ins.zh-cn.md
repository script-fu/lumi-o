---
title: "过滤器插件"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e8eb69ed9dff7c65cc926ba4bfb4c333fdd8baa3832aa92765ba6bb19b17516d
---
我们在[第一步](../../first-step/) 教程中使用了_procedure_插件。这些类型的插件无需图像或可绘制对象作为输入即可工作。通常，我们使用插件来更改图像及其可绘制对象。像这样的插件称为_filter_插件。

### 什么是可绘制对象？

Lumi 中的 **drawable** 是指可以在其上绘制的图像元素，例如图层或通道。过滤器插件通常对这些元素进行操作。

### 一个简单的过滤器插件示例

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; 使用 let 语句定义消息变量和核心代码
  (let ((message "hello, world"))
    ;; 在 Lumi 的 Error Console 中显示消息
    (lumi-message message)
    ;; 反转第一个所选 drawable 的颜色
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; 注册 plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; 主过程名称
  "Simple Filter Plug-in Demo"             ;; 在 Lumi 菜单中显示的名称
  "Tests a basic Scheme filter plug-in"    ;; 工具提示描述
  "Author Name"                            ;; 给自己一点肯定
  "License"                                ;; 许可证
  "Date written"                           ;; 编写日期
  "*"                                      ;; 表示此插件需要图像
  SF-ONE-OR-MORE-DRAWABLE)                 ;; 需要一个或多个所选 drawable

;; 指定插件的菜单位置
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

复制文本并将其以 `simple-filter-plug-in.scm` 形式保存在 Lumi 插件文件夹之一名为 `simple-filter-plug-in` 的文件夹中。 Lumi 插件文件夹是下面列出的 _any_ 文件夹：
 **Lumi > 编辑 > 首选项 > 文件夹 > 插件**

在 Linux 中，右键单击 `simple-filter-plug-in.scm` 文件，转到 **属性 > 权限**，然后选中 **允许将文件作为程序执行**。一旦文件位于正确的位置、可执行且没有语法错误，当 Lumi 重新启动时，它将出现在顶部菜单标题栏中名为 **插件** 的菜单内。

### 运行插件

1. 打开一张图片（此滤镜插件需要图片才能工作）。
2. 打开 **工具 > 调试 > 消息控制台** 以查看消息。
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
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
  (lumi-message message) ;; 在 Lumi 的 Error Console 中显示消息
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; 反转第一个所选 drawable 的颜色
```

### 插件注册

该插件注册到Lumi作为滤镜插件：

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; 注册主过程
  "Simple Filter Plug-in Demo"             ;; 在 Lumi 菜单中显示的名称
  "Tests a basic Scheme filter plug-in"    ;; 工具提示描述
  "Author Name"                            ;; 作者姓名
  "License"                                ;; 许可证类型
  "Date written"                           ;; 编写日期
  "*"                                      ;; 表示插件需要图像
  SF-ONE-OR-MORE-DRAWABLE)                 ;; 需要一个或多个所选 drawable
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


**消息控制台**是用于对自定义插件进行故障排除的宝贵工具。如果您的插件未按预期运行，请在此处检查错误消息或日志。 **终端**窗口还可以提供调试信息并报告加载问题。