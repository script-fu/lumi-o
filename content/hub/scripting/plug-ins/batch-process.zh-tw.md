---
title: "批次處理"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
一個實用的端到端範例，用於一次處理多個檔案。

## 原始碼

- [檢視原始碼](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Lumi 中的選單

- **檔案 → 批次**

## 示範內容

- `SF-DIRNAME` 來源/目標目錄的參數
- 驗證帶有後備的 GUI 路徑 (`validate-path-and-dir`)
- 遞歸目錄掃描與迭代
- 長期運行操作的進度報告
