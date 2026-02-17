---
title: "批次處理"
type: docs
---
一個實用的端到端範例，用於一次處理多個檔案。

## 它居住的地方

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## 它出現在 Lumi 中的位置

- **檔案 → 批次**

## 它展示了什麼

- `SF-DIRNAME` 來源/目標目錄的參數
- 驗證帶有後備的 GUI 路徑 (`validate-path-and-dir`)
- 遞歸目錄掃描與迭代
- 長期運行操作的進度報告