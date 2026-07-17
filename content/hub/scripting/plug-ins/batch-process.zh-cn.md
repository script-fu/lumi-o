---
title: "批处理"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
一个实用的端到端示例，用于一次性处理多个文件。

## 源代码

- [查看源代码](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Lumi 中的菜单

- **文件 → 批处理**

## 演示内容

- `SF-DIRNAME` 源/目标目录的参数
- 验证带有后备的 GUI 路径 (`validate-path-and-dir`)
- 递归目录扫描和迭代
- 长期运行操作的进度报告
