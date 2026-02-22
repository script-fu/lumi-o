---
title: "光谱混色"
type: docs
---
Lumi 的调色板系统使用光谱颜色模型来模拟真实颜料的混合方式。目标是让从数字调色板中构建和选择颜色的体验就像混合物理油漆一样。一旦将颜色应用到画布上，它就是标准 RGB。

## 光谱混合意味着什么

传统的 RGB 混合是相加的：混合两个 RGB 值将它们平均到中点。颜料混合是减色的：每种颜料吸收特定的波长，它们的组合效果更暗，并且色调经常发生变化。

Lumi 使用调色板颜色的 10 波段光谱反射率表示（而不是 RGB）对此进行建模。

这会产生类似油漆的效果：混合蓝色和黄色会产生绿色，而不是灰色。混合两种饱和颜色会产生一种像物理颜料一样向中性方向转变的颜色。

光谱计算在调色板构建期间运行 - 当生成二级和三级调色板条目时以及当调色板混合器混合两种父颜色时。生成的颜色将转换为线性 RGB 以供显示和绘画。

## 颜料配置文件

调色板条目可以基于使用**颜色索引 (CI) 代码**的真实颜料数据。每个 CI 颜料系列都具有影响其混合方式的特征光谱偏差。

|颜料作用|混合行为|示例|
| :--- | :--- | :--- |
| **小学** |高色度、干净的二次色 | PY3（柠檬黄）、PR122（洋红色）|
| **身体** |不透明、强烈的主色调，在绿色混合中转向橄榄色 | PY35（镉黄）、PR108（镉红）|
| **中和剂** |快速降低饱和度并静音 | PBk11（火星黑）、PBr7（西耶娜）|
| **色度锚** |高着色力，在混合物中占主导地位 | PB29（群青蓝）、PG7（酞绿）|

将带有 CI 代码的原色添加到调色板可为混合引擎提供这些颜色的准确光谱偏差，因此生成的二级和三级混合反映了真实世界的混合行为。

## 夜光颜料

主调色板附带以下颜料。样本显示了每种颜料的典型主色（全强度，未稀释）外观。

### 橙色和黄色|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |吡咯橙| PO73 |红色（猩红色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |镉橙| PO20 |黄色（身体）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |镉黄| PY35 |黄色（身体）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |浅镉黄 | PY35：苍白|黄色（浅镉）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |柠檬黄| PY3 |黄色（柠檬色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |镍偶氮黄| PY150 |黄色（中）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |绿金| PY129 |黄绿色（金色）|

### 大地色

|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |烧焦的赭石| PBr7：烧焦|地球（红棕色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |烧焦的琥珀色| PBr7：琥珀色 |地球（中性）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |原始西耶娜 | PBr7：原料 |地球（黄棕色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |黄赭石 | PY42 |地球（黄色）|

### 绿党

|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |酞菁绿 (YS) | PG36 |绿色（酞黄色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |常青 | PG18 |绿色（常青）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |绿地 | PG23 |绿色（地球冷）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |温莎·格林（BS）| PG7 |绿色（酞菁蓝）|### 蓝色和青色

|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |钴绿松石灯| PG50 |青色（矿物）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |天蓝色| PB35 |青色（矿物）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |酞菁绿松石 | PB16 |蓝色（酞菁）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |钴蓝| PB28 |蓝色（紫罗兰色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |温莎蓝| PB15 |蓝色（酞菁）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |群青 | PB29 |蓝色（紫罗兰色）|

### 紫罗兰色、洋红色和红色

|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |绚丽紫罗兰 | PV23 |紫罗兰（二恶嗪）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |永久玫瑰 | PV19：玫瑰|洋红色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |喹吖啶酮洋红色| PV19：洋红色 |洋红色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |永久茜素深红 | PV19：深红|洋红色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |苝紫| PV29 |洋红色（喹吖啶酮）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |苝栗色 | PR179 |红色（深红色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |吡咯红| PR254 |红色（猩红色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |吡咯红光| PR255 |红色（吡咯光）|

### 黑人和白人|斯沃琪 |名称 | CI 代码 |家庭|
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |火星黑（暖色）| PBk11 |黑色（火星）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |苝绿| PBk31 |黑色（苝绿）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |象牙黑（冷色）| PBk9 |黑色（象牙色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |灯黑（中性）| PBk7 |黑色（灯）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |钛白（暖）| PW6：温暖|白色（钛暖色）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |钛白（中性）| PW6 |白色（钛中性）|
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |锌白（冷色）| PW4 |白色（锌冷）|

### 控制灰色

控制灰色是标准化的中和剂，用于可预测地降低混合物的饱和度。

|斯沃琪 |名称 | CI 代码 |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |暖灰色| N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |中性灰| N_中性 |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1pxsolid rgba(0,0,0,0.25)"></span> |冷灰色| N_COOL |

## 调色板图

调色板图将活动调色板可视化为色相轮：36 个色相扇区（10° 步长）× 15 个亮度单元。添加主色后，系统会生成二级和三级混合并将它们放置在适当的地图位置。

单击单元格会选择一种颜色作为前景。按住 Shift 键并单击将其指定为调色板混合器中的父端点。

## 调色板混合器

调色板混合器使用固定的三阶段管道从两个父条目派生新颜色：

1. **混合**：母体 A (CCW) 和母体 B (CW) 之间的光谱 WGM。
2. **色度**：向调色板的中性光谱混合，降低饱和度。
3. **色调**：混合以混合白色或混合黑色，调整亮度。

最后应用色调，因此亮度调整不会因色度变化而减弱。值锁定和波段钳控件将结果限制到特定的亮度级别或值带。

混合颜色可以作为**自定义**条目保存到调色板中，存储完整的配方（父 UID、混合因子、色调、色度值）以供以后恢复。

## 画布像素为 RGB光谱系统完全在调色板构建和颜色选择中运行。当应用画笔描边时，前景色（已转换为线性 RGB）就是所绘制的颜色。画布存储标准 RGB 像素数据。

光谱混合改善了构建调色板和以与物理颜料行为一致的方式选择颜色的体验，而无需改变图像数据的存储或合成方式。