---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage 是單一檔案的 Linux 應用程式套件。下載一個檔案，將其設為可執行，即可執行，無需在系統範圍內安裝軟體。

官方 AppImage 網站：https://appimage.org/

AppImage 提供無需安裝或修改系統即可執行的 Lumi 可攜版。對於希望立即使用軟體、而不想管理相依性、編譯原始碼或設定開發環境的藝術家來說，它是理想選擇。

作為自包含的可執行檔，AppImage 可以保存在系統任意位置。這使測試新版本、保留多個版本或在電腦之間移動軟體變得容易。

在 Lumi 的開發流程中，AppImage 充當與持續整合輸出緊密匹配的可攜測試建置。這能在保持一致環境進行可靠測試的同時，讓本機原始碼建置專注於開發工作。

注意：CI 使用 Lumi 的儲存庫內整合相依性來源（BABL/GEGL/GTK3）建置 AppImage，因此相依性堆疊與本機 `lumi-build-script.sh` 工作流程一致。

## 發行版與開發版 AppImage

- **發行 AppImage**：尚不可用（Lumi 尚未發行）。
- **開發 AppImage（CI 成品）**：從進行中的開發提交自動產生，供測試使用。

本指南主要介紹 **開發 AppImage** 工作流程。

目前成品頁面：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 下載基礎

CI 會產生成品 zip 檔案（例如 `lumi-appimage*.zip`）。

基本手動流程：

1. 下載最新的 CI 成品 zip
2. 解壓縮
3. 執行包含的 `Lumi*.AppImage` 檔案

以下腳本是自動執行這些步驟的可選輔助工具。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 可選輔助腳本

- `lumi-appimage-unpack-zip.sh`
  - 在 `~/Downloads` 中尋找最新的 `lumi-appimage*.zip`
  - 將 AppImage 安裝到 `~/AppImage/Lumi/Lumi_CI.AppImage`
  - 將桌面資源安裝到 `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - 在終端機中啟動 AppImage
  - 啟用執行階段輸出（`APPIMAGE_DEBUG=1`）

## 常見注意事項

- 如果手動執行 AppImage（不使用輔助腳本），請先設為可執行：

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` 會自動套用執行權限。

- 如果 Lumi 已在其他建置中執行，請在啟動 AppImage 之前將其關閉。
