---
title: "实用工具浏览器"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 99abaafdc68cf3433959e5db87130b22c51cfbd5a98697fa807732b9fdae9ff0
url: "hub/scripting/reference/utility-browser"
---
实用工具浏览器可让您探索 Lumi 附带的内置 Scheme 实用程序标准库，而无需离开应用程序或查找源文件。

## 打开实用工具浏览器

转至 **帮助 → 编程 → 实用工具浏览器**。

窗口立即打开；无需提前加载任何插件。

## 它显示了什么

浏览器列出了 Lumi 在启动时自动加载的七个实用程序库导出的每个过程、变量和语法形式：

| 库 | 涵盖内容 |
|---|---|
| `common.scm` |通用帮助程序（字符串、数字、列表实用程序）|
| `files.scm` |文件和路径助手 |
| `gegl.scm` | GEGL 缓冲区和颜色助手 |
| `images.scm` |图像级助手（`image-get-open-list` 等）|
| `layers.scm` |图层和可绘制助手|
| `parasites.scm` | Parasite 读写助手 |
| `paths.scm` |路径和矢量助手|

这些库可在任何 Scheme 插件或 Scheme 控制台中使用。

## 搜索和过滤

- **搜索框**：在您键入时按名称过滤（不区分大小写的子字符串匹配）。
- **种类过滤器**：将结果缩小为 `procedure`、`variable` 或 `syntax`。

单击一个条目会显示其完整的文档字符串及其来源的库。

## Stdlib 作为包装器

实用程序库是包装模式的实际应用：每个帮助程序都为低级操作提供清晰的名称，隐藏样板文件，并在底层命令发生更改时提供一个更新位置。如果您想了解它们背后的设计方法，请参阅 **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})** 教程。

## 与过程浏览器的关系

实用工具浏览器与**过滤器→ Script-Fu → 控制台→ 浏览**（过程浏览器）是分开的。过程浏览器列出了 PDB 注册的过程。实用工具浏览器列出了有意位于 PDB 之外的 helper 定义：它们仅存在于 Scheme 中，没有 C 绑定。
