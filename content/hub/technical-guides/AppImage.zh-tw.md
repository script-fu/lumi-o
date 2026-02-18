---
title: "應用程式圖像"
type: docs
---
AppImage 是一個單一檔案 Linux 應用程式套件。您下載一個文件，將其標記為可執行文件，然後運行它，而無需在系統範圍內安裝軟體。

官方AppImage網站：https://appimage.org/

AppImage 提供了 Lumi 的便攜式版本，無需安裝或修改系統即可運作。對於想要立即使用該軟體而無需管理依賴項、編譯原始碼或配置開發環境的藝術家來說，它是理想的選擇。

作為一個獨立的可執行文件，AppImage 可以儲存在系統上的任何位置。這使得測試新版本、保留多個版本或在電腦之間移動軟體變得容易。

對於 Lumi 的開發過程，AppImage 充當便攜式測試構建，與持續整合輸出緊密匹配。這允許在一致的環境中進行可靠的測試，同時保持本地來源建置專注於開發工作。

注意：CI 使用 Lumi 的儲存庫內整合相依性來源 (BABL/GEGL/GTK3) 建構 AppImage，因此相依性堆疊與本機 `lumi-build-script.sh` 工作流程一致。

## 發布與開發 AppImage

- **發布AppImage**：尚不可用（Lumi 尚未發布）。
- **開發 AppImage（CI 工件）**：從正在進行的開發提交自動產生以進行測試。

本指南主要涵蓋**開發 AppImage** 工作流程。

當前工件頁面：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 下載基礎知識

CI 產生工件 zip 檔案（例如`lumi-appimage*.zip`）。

基本手動流程：

1. 下載最新的 CI 工件 zip。
2. 提取它。
3. 運行包含的`Lumi*.AppImage` 檔案。

下面的腳本是可選的幫助程序，可自動執行這些步驟。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 可選的說明腳本

- `lumi-appimage-unpack-zip.sh`
  - 在`~/Downloads`中找到最新的`lumi-appimage*.zip`
  - 將AppImage安裝到`~/AppImage/Lumi/Lumi_CI.AppImage`
  - 將桌面資源安裝到`~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - 在終端機中啟動 AppImage
  - 啟用運行時輸出 (`APPIMAGE_DEBUG=1`)

## 常用注意事項

- 如果您手動執行 AppImage（沒有幫助程式腳本），請先使其可執行：

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` 已自動套用執行權限。

- 如果 Lumi 已經從另一個版本運行，請在啟動 AppImage 之前將其關閉。