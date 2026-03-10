---
title: "快速入門"
type: docs
---
Lumi 尚未發布，它作為開發版本提供。

如果您已經使用 Linux 並且想要快速運行 Lumi，請使用 GitLab 工件中最新的 **開發 AppImage**：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1.下載最新開發AppImage神器zip。
2. 拉開拉鍊。
3. 雙擊`Lumi*.AppImage` 檔案運行它。

AppImage 應該已經可以運作。如果不是，請在檔案的權限中啟用**允許將檔案作為程式執行**，或使用下面的終端方法。

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Linux 上的 Wacom 設置

對於 Lumi 中的數位繪畫，簡單的**線性壓力設定**通常是最好的：

- 保持平板電腦驅動器壓力曲線線性。
- 保持 Lumi 中的壓力/輸入曲線呈線性。
- 使用畫筆本身塑造感覺，因為畫筆動力學可能是非線性的。

您可以使用以下命令檢查並重設 Linux 驅動程式曲線：

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

實用技巧：

- Lumi 目前會阻止有問題的 Wacom 板/觸控環輸入，以避免 X11 故障。將平板電腦按鈕對應到**相對**畫筆大小向上/向下。
- 如果使用 `Alt` 進行畫筆大小拖曳不起作用，則您的桌面可能正在使用 `Alt` 來移動視窗。將該視窗管理器捷徑變更為 `Super` 或停用它。

如果您想使用原始程式碼，請前往[Technical Guides](/hub/technical-guides/) 和[Installation](/hub/technical-guides/Installation/)。