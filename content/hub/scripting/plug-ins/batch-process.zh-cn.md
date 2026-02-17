---
title: "批处理"
type: docs
---
一个实用的端到端示例，用于一次性处理多个文件。

## 它居住的地方

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## 它出现在 Lumi 中的位置

- **文件 → 批处理**

## 它展示了什么

- `SF-DIRNAME` 源/目标目录的参数
- 验证带有后备的 GUI 路径 (`validate-path-and-dir`)
- 递归目录扫描和迭代
- 长期运行操作的进度报告