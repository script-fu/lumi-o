---
title: "適用於 Linux 的開源數位繪畫"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o 專為分層光柵繪畫而設計，刻意保持輕量、穩定。Wacom 數位板支援提供流暢、反應靈敏的輸入，滿足專業插畫創作的需求。

Lumi 的調色盤驅動色彩系統會以光譜方式混合真實世界的顏料設定檔。圍繞可視化的色彩關係建立調色盤，控制明度，並將顏色整理為具名項目。調色盤圖和調色板混合器有助於在整幅作品或專案中保持顏色選擇的一致性。

Lumi-o 基於 GIMP 的核心架構建構，由藝術家設計和完善。其工具易於上手，同時提供進階工作流程所需的自訂能力。快速載入與儲存，以及完全整合的自動儲存系統，在不打斷創作流程的情況下保護您的作品。

筆觸可以帶上顆粒與紙齒，書法筆觸也一樣。新的一筆，或筆途中的一次轉彎，可以挪開那層顆粒，所以來回鋪明暗時，每折返一次就從下一段重新疊起。刷毛筆刷可以細細塑形，刷毛也能沿著筆觸偏移、換新。OpenGL 在影像視窗裡畫出圖層疊，也可以按個別工具預設來畫筆觸。大型噴筆和大型紋理圖章快上許多，畫布移動也保持順暢。

延遲載入讓 Lumi 以更少的 RAM 開啟大型、複雜的專案。當圖層群組處於摺疊狀態時，Lumi 會載入其儲存的合成影像，而其中的圖層、遮色片和巢狀群組在群組再次展開之前仍維持為輕量的預留位置。

Lumi-o 的開發環境是 Debian 13，桌面為 X11 上的 Cinnamon。Wayland 上的 GNOME 則另做過測試，範圍是應用程式的整體外觀，以及 OpenGL 渲染。如果您是從 Windows 轉換而來，安裝 Debian 是第一步。請參閱[如何安裝 Linux](/hub/install-linux/)。

{{< cards >}}
  {{< card link="hub/about/" title="關於" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="快速入門" icon="play" >}}
  {{< card link="hub/features/" title="功能" icon="book-open" >}}
{{< /cards >}}
