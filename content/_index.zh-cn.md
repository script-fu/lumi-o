---
title: "适用于 Linux 的开源数字绘画"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o 专为分层光栅绘画而设计，刻意保持轻量、稳定。Wacom 数位板支持提供流畅、响应灵敏的输入，满足专业插画创作的需求。

Lumi 基于调色板的色彩系统以光谱方式混合真实世界的颜料特性。围绕可视化的颜色关系构建调色板，控制明度，并将颜色组织为带名称的条目。调色板图和调色板混合器有助于在整幅作品或项目中保持颜色选择的一致性。

Lumi-o 基于 GIMP 的核心架构构建，由艺术家设计和完善。其工具易于上手，同时提供高级工作流所需的自定义能力。快速加载与保存，以及完全集成的自动保存系统，在不打断创作流程的情况下保护您的作品。

笔触可以带上颗粒与齿纹那样的表面纹理，书法笔触也包括在内。新的一笔，或笔触中的一次转向，可以挪动那层颗粒，因此来回铺明暗时，每次折返都从一段新的位置叠起。刷毛笔刷可以精细塑形，刷毛也能沿笔触偏移并更新。OpenGL 在图像窗口中绘制图层堆叠，并可以按各个工具预设绘制笔触。大型喷笔和大型纹理图章快得多，画布浏览也保持顺滑。

延迟加载让 Lumi 以更少的 RAM 打开大型、复杂的项目。当图层组处于折叠状态时，Lumi 会加载其保存的合成图像，而其中的图层、蒙版和嵌套组在组再次展开之前仍保持为轻量级占位符。

Lumi-o 在 Debian 13 上、于 X11 的 Cinnamon 中开发。Wayland 上的 GNOME 已就应用程序的整体外观和 OpenGL 渲染做过测试。如果您是从 Windows 转来的，安装 Debian 是第一步。请参阅[如何安装 Linux](/hub/install-linux/)。

{{< cards >}}
  {{< card link="hub/about/" title="关于" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="快速入门" icon="play" >}}
  {{< card link="hub/features/" title="功能" icon="book-open" >}}
{{< /cards >}}
