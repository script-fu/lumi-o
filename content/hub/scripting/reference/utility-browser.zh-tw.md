---
title: "實用工具瀏覽器"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 99abaafdc68cf3433959e5db87130b22c51cfbd5a98697fa807732b9fdae9ff0
url: "hub/scripting/reference/utility-browser"
---
實用程式瀏覽器可讓您探索 Lumi 隨附的內建 Scheme 實用程式標準庫，而無需離開應用程式或查找原始碼。

## 開啟實用程式瀏覽器

前往 **幫助 → 程式設計 → 實用程式瀏覽器**。

視窗立即開啟；無需提前加載任何插件。

## 它顯示了什麼

瀏覽器列出了 Lumi 在啟動時自動載入的七個實用程式庫導出的每個流程、變數和語法形式：

| 程式庫 | 涵蓋內容 |
|---|---|
| `common.scm` |通用幫助程式（字串、數字、列表實用程式）|
| `files.scm` |檔案與路徑助手 |
| `gegl.scm` | GEGL 缓冲区和颜色助手 |
| `images.scm` |影像級助手（`image-get-open-list` 等）|
| `layers.scm` |圖層與可繪製助手|
| `parasites.scm` | Parasite 讀寫助手 |
| `paths.scm` |路徑與向量小幫手|

這些程式庫可在任何 Scheme 外掛或 Scheme 控制台中使用。

## 搜尋和過濾

- **搜尋框**：在您鍵入時按名稱過濾（不區分大小寫的子字串匹配）。
- **種類過濾器**：將結果縮小為 `procedure`、`variable` 或 `syntax`。

按一下一個條目會顯示其完整的文件字串及其來源的庫。

## Stdlib 作為包裝器

實用程式庫是包裝模式的實際應用：每個幫助程式都為低階操作提供清晰的名稱，隱藏樣板文件，並在底層命令發生變更時提供一個更新位置。如果您想了解它們背後的設計方法，請參閱 **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})** 教程。

## 與過程瀏覽器的關係

實用程式瀏覽器與**過濾器→ Script-Fu → 控制台→ 瀏覽**（流程瀏覽器）是分開的。過程瀏覽器列出了 PDB 註冊的過程。實用程式瀏覽器列出了有意位於 PDB 之外的 helper 定義：它們僅存在於 Scheme 中，沒有 C 繫結。
