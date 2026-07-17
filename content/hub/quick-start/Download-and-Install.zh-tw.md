---
title: "下載並安裝"
type: docs
url: "hub/quick-start/Download-and-Install"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5f17d7e9009aeeacf256152bef94386ccc5a8eea87cf0feebef073488fb59283
---
如果您已經使用 Linux 並且想要快速運行 Lumi，請使用 GitLab 工件中最新的 **開發 AppImage**：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. 下載最新的開發版 AppImage 構件 zip。
2. 解壓 zip 檔案。
3. 雙擊`Lumi*.AppImage` 檔案運行它。

AppImage 應該已經可以運作。如果不是，請在檔案的權限中啟用**允許將檔案作為程式執行**，或使用下面的終端方法。

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```