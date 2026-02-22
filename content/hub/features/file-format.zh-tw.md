---
title: "文件格式 (.lum)"
type: docs
---
Lumi 使用開放的、基於目錄的檔案格式 (`.lum`)，專為效能、可靠性和長期可訪問性而設計。

## 概述

`.lum` 檔案實際上是一個包含以下內容的目錄：
- **元資料**（圖層、混合模式、屬性）。
- **層緩衝區**（每層的單獨像素資料）。
- **遮罩**（圖層遮色片的灰階資料）。
- **恢復歷史記錄**（增量快照）。

這種結構可以實現快速保存、延遲載入大檔案以及即使在崩潰後也能恢復工作。

## 關鍵屬性

### 開啟且可讀

`.lum` 格式使用 XML 元資料和壓縮的二進位緩衝區。您可以以純​​文字形式檢查圖層結構、屬性和混合模式。無專有編解碼器；像素資料以標準 GEGL 緩衝區格式儲存。

### 增量儲蓄

必須在「另存為」對話方塊中為每個項目啟用增量儲存（「增量儲存」複選框和「最大儲存」旋轉按鈕）。啟用後，Ctrl+S 僅寫入修改的圖層，而不是重寫整個項目，從而大幅減少保存時間。此設定與項目一起儲存並在各個會話中持續存在。

### 延遲加載

大型專案開工速度快。僅當出現以下情況時才會從磁碟載入圖層像素：
- 此層變成可見。
- 你在圖層上繪畫。
- 圖層被導出或合成。

非常大的項目（500+層，數GB數據）仍然保持響應。延遲載入預設啟用，可以在 **編輯 → 首選項 → 效能 → 記憶體資源** 中切換。

### 自動儲存

Lumi 定期自動將變更儲存到**單獨的快取位置** (`~/.cache/lumi/autosave/`)。自動儲存獨立於工作文件並且不會修改它。間隔和快取位置可在 **編輯 → 首選項 → 效能** 中配置。

## 訪問

### 儲存並另存為

- **檔案** → **儲存** (Ctrl+S)：儲存到目前`.lum` 目錄。
- **檔案** → **另存為** (Shift+Ctrl+S)：儲存到新的 `.lum` 檔案。 「另存為」對話方塊包括壓縮類型選項和**增量儲存**開關（具有**最大儲存**限制），用於啟用或停用該項目的增量保存。

未儲存的變更在視窗標題中以星號 (*) 表示。

### 匯出

- **檔案** → **匯出為** (Shift+Ctrl+E)：匯出為 PNG、JPEG、TIFF 或其他格式。
- **檔案** → **覆蓋** (Ctrl+E)：重新匯出到上次匯出的檔案。

導出會展平可見圖層並從光譜轉換為 sRGB 色彩空間。

### 導入

- **檔案** → **開啟** (Ctrl+O)：載入 `.lum` 項目。
- **檔案** → **以圖層開啟** (Shift+Ctrl+O)：匯入 `.lum`、XCF 或 PSD 檔案作為新圖層。
- **文件** → **最近文件**：快速存取最近開啟的項目。

PSD 和 XCF 檔案在匯入時會轉換為 Lumi 的本機格式。

## 匯入和匯出相容性

### 支援的導入格式
- **.lum**：Lumi 原生格式。
- **.xcf**：GIMP 本機格式（保留圖層和基本屬性）。
- **.psd**：Photoshop 格式（保留圖層和混合模式）。
- **PNG、JPEG、TIFF 等**：平面影像導入。

### 支援的匯出格式
- **PNG**：無損，具有 Alpha 透明度。
- **JPEG**：有損、扁平化。
- **TIFF**：無損或 LZW 壓縮。
- **XCF**：GIMP 相容格式。僅供出口；保留圖層和基本屬性。

## 專案恢復Lumi 維護自動後台保存和手動增量檢查點，均可從 **檔案** → **恢復影像** 存取。有關完整詳細信息，請參閱[File Recovery](../recovery) 頁面。

## 組織

`.lum` 檔案是具有固定結構的目錄：

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

層緩衝區以圖層 (`layer-Background.geglbuf`) 命名，而不是依序編號。圖層名稱中的空格儲存為底線；圖層群組有一個 `-GROUP` 後綴。蒙版共享圖層名稱 (`mask-Background.geglbuf`)。

每個`recovery/primary-NN.lum/` 都是完整的基線保存。接著按 Ctrl+S 按下附加 `delta-NNNN.lum/` 子目錄，其中僅包含自上一個基線以來修改的緩衝區，無論項目大小如何，都能保持檢查點保存快速。

自動儲存遵循相同的結構，但單獨儲存在 `~/.cache/lumi/autosave/` 中，使工作文件保持不變。
- **非常大的項目**：具有 1000+ 層和 TB 資料的項目將從延遲加載中受益最多；但是，最終導出為平面圖像格式可能需要一些時間。
- **網路磁碟機**：支援儲存至網路安裝目錄，但由於 I/O 延遲，速度比本機儲存慢。