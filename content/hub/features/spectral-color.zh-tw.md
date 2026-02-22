---
title: "光譜混色"
type: docs
---
Lumi 的調色板系統使用光譜顏色模型來模擬真實顏料的混合方式。目標是讓從數位調色板中建立和選擇顏色的體驗就像混合實體油漆一樣。一旦將顏色應用到畫布上，它就是標準 RGB。

## 光譜混合意味著什麼

傳統的 RGB 混合是相加的：混合兩個 RGB 值將它們平均到中點。顏料混合是減色的：每種顏料吸收特定的波長，它們的組合效果較暗，色調經常會改變。

Lumi 使用調色板顏色的 10 波段光譜反射率表示（而不是 RGB）對此進行建模。

這會產生類似油漆的效果：混合藍色和黃色會產生綠色，而不是灰色。混合兩種飽和顏色會產生一種像物理顏料一樣轉變為中性方向的顏色。

光譜計算在調色板構建期間運行 - 當生成二級和三級調色板條目時以及當調色板混合器混合兩種父顏色時。產生的顏色將轉換為線性 RGB 以供顯示和繪畫。

## 顏料配置文件

調色板條目可以基於使用**顏色索引 (CI) 代碼**的真實顏料資料。每個 CI 顏料系列都具有影響其混合方式的特徵光譜偏差。

|顏料作用|混合行為|範例|
| :--- | :--- | :--- |
| **小學** |高色度、乾淨的二次色 | PY3（檸檬黃）、PR122（洋紅色）|
| **身體** |不透明、強烈的主色調，在綠色混合中轉向橄欖色 | PY35（鎘黃）、PR108（鎘紅）|
| **中和劑** |快速降低飽和度並靜音 | PBk11（火星黑）、PBr7（西耶娜）|
| **色度錨** |高著色力，在混合中占主導地位 | PB29（群青藍）、PG7（酞綠）|

將帶有 CI 程式碼的原色添加到調色板可為混合引擎提供這些顏色的準確光譜偏差，因此生成的二級和三級混合反映了真實世界的混合行為。

## 夜光顏料

主調色板附帶以下顏料。樣本顯示了每種顏料的典型主色（全強度，未稀釋）外觀。

### 橙色和黃色|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1pxsolid rgba(0,0,0,0025) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.525) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25) | PY35：蒼白|黃色（淺鎘）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.525) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1pxPYid rgba(0,0,0,0.align:middle;border:1pxPYid rgba(0,0,0,0.

### 大地色

|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) | PBr7：燒焦|地球（紅棕色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1pxsolid rgba(0,35);vertical-align:middle;border:1pxsolid rgba(0,35);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1pxsolid rgba(0,0,0,000,025) |

### 綠黨

|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.35) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;borderorder:1pxsolid rgba(0,0,0,0.125) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1pxsolid rgba(0,0,00.
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0. 25) | |綠色（酞菁藍）|### 藍色和青色

|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.55) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |

### 紫羅蘭、洋紅色和紅色

|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1pxsolid rgba(0,0,0,0025) | |紫羅蘭（二噁嗪）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;borderorder:1pxsolid rgba(0,0,0,0.25) | PV19：玫瑰|洋紅色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,00.35)> 紅色調子類|洋紅色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1pxsolid rgba(0,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25) | PV19：深紅色|洋紅色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) | |洋紅色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1pxsolid rgba(0,45);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1pxsolid rgba(0,60);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1pxsolid rgba(0,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.55) |

### 黑人和白人|斯沃琪 |名稱 | CI 代碼 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0. 火星) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1pxsolid rgba(0,12);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.55) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0.55) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1pxsolid rgba(0,18);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.align:middle;border:1pxsolid rgba(0,0,0,0. 25) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1pxsolid rgba(0,0,000-align) | PW6：溫暖|白色（鈦暖色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1pxsolid rg鈦ba(0,0,000-align:middle;border:1pxsolid rg鈦ba(0,0,00062) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1pxsolid rgba(025),00. |白色（鋅冷）|

### 控制灰色

控制灰色是標準化的中和劑，用於可預測地降低混合物的飽和度。

|斯沃琪 |名稱 | CI 代碼 |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1pxsolid rgba(0,0,000-align:middle;border:1pxsolid rgba(0,0,00.
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1pxsolid rgba(0,0,000-align:middle;border:1pxsolid rgba(0,0,000.
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1pxsolid rgba(0,0,000-align:middle;border:1pxsolid rgba(0,0,000.

## 調色盤圖

調色盤圖將活動調色盤視覺化為色相輪：36 個色相扇區（10° 步長）× 15 個亮度單元。添加主色後，系統會產生二級和三級混合並將它們放置在適當的地圖位置。

按一下儲存格會選擇一種顏色作為前景。按住 Shift 鍵並按一下將其指定為調色板混合器中的父端點。

## 調色盤混合器

調色板混合器使用固定的三階段管道從兩個父條目派生新顏色：

1. **混合**：母體 A (CCW) 和母體 B (CW) 之間的光譜 WGM。
2. **色度**：向調色盤的中性光譜混合，降低飽和度。
3. **色調**：混合以混合白色或混合黑色，調整亮度。

最後應用色調，因此亮度調整不會因色度變化而減弱。值鎖定和波段鉗控制將結果限製到特定的亮度等級或值帶。

混合顏色可以作為**自訂**條目保存到調色板中，儲存完整的配方（父 UID、混合因子、色調、色度值）以供以後恢復。

## 畫布像素為 RGB光譜系統完全在調色板構建和顏色選擇中運行。當應用畫筆描邊時，前景色（已轉換為線性 RGB）就是所繪製的顏色。畫布儲存標準 RGB 像素資料。

光譜混合改善了建立調色板和以與物理顏料行為一致的方式選擇顏色的體驗，而無需改變影像資料的儲存或合成方式。