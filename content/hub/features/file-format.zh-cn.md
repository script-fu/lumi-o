---
title: "文件格式 (.lum)"
type: docs
---
Lumi 使用开放的、基于目录的文件格式 (`.lum`)，专为性能、可靠性和长期可访问性而设计。

## 概述

`.lum` 文件实际上是一个包含以下内容的目录：
- **元数据**（图层、混合模式、属性）。
- **层缓冲区**（每层的单独像素数据）。
- **蒙版**（图层蒙版的灰度数据）。
- **恢复历史记录**（增量快照）。

这种结构可以实现快速保存、延迟加载大文件以及即使在崩溃后也能恢复工作。

## 关键属性

### 打开且可读

`.lum` 格式使用 XML 元数据和压缩的二进制缓冲区。您可以以纯文本形式检查图层结构、属性和混合模式。无专有编解码器；像素数据以标准 GEGL 缓冲区格式存储。

### 增量储蓄

必须在“另存为”对话框中为每个项目启用增量保存（“增量保存”复选框和“最大保存”旋转按钮）。启用后，Ctrl+S 仅写入修改的图层，而不是重写整个项目，从而大大减少保存时间。该设置与项目一起存储并在各个会话中持续存在。

### 延迟加载

大型项目开工速度快。仅当出现以下情况时才会从磁盘加载图层像素：
- 该层变得可见。
- 你在图层上绘画。
- 图层被导出或合成。

非常大的项目（500+层，数GB数据）仍然保持响应。延迟加载默认启用，可以在 **编辑 → 首选项 → 性能 → 内存资源** 中切换。

### 自动保存

Lumi 定期自动将更改保存到**单独的缓存位置** (`~/.cache/lumi/autosave/`)。自动保存独立于工作文件并且不会修改它。间隔和缓存位置可在 **编辑 → 首选项 → 性能** 中配置。

## 访问

### 保存并另存为

- **文件** → **保存** (Ctrl+S)：保存到当前`.lum` 目录。
- **文件** → **另存为** (Shift+Ctrl+S)：保存到新的 `.lum` 文件。 “另存为”对话框包括压缩类型选项和**增量保存**开关（具有**最大保存**限制），用于启用或禁用该项目的增量保存。

未保存的更改在窗口标题中用星号 (*) 表示。

### 导出

- **文件** → **导出为** (Shift+Ctrl+E)：导出为 PNG、JPEG、TIFF 或其他格式。
- **文件** → **覆盖** (Ctrl+E)：重新导出到上次导出的文件。

导出会展平可见图层并从光谱转换为 sRGB 色彩空间。

### 导入

- **文件** → **打开** (Ctrl+O)：加载 `.lum` 项目。
- **文件** → **作为图层打开** (Shift+Ctrl+O)：导入 `.lum`、XCF 或 PSD 文件作为新图层。
- **文件** → **最近文件**：快速访问最近打开的项目。

PSD 和 XCF 文件在导入时会转换为 Lumi 的本机格式。

## 导入和导出兼容性

### 支持的导入格式
- **.lum**：Lumi 原生格式。
- **.xcf**：GIMP 本机格式（保留图层和基本属性）。
- **.psd**：Photoshop 格式（保留图层和混合模式）。
- **PNG、JPEG、TIFF 等**：扁平图像导入。

### 支持的导出格式
- **PNG**：无损，具有 Alpha 透明度。
- **JPEG**：有损、扁平化。
- **TIFF**：无损或 LZW 压缩。
- **XCF**：GIMP 兼容格式。仅供出口；保留图层和基本属性。

## 项目恢复Lumi 维护自动后台保存和手动增量检查点，均可从 **文件** → **恢复图像** 访问。有关完整详细信息，请参阅[File Recovery](../recovery) 页面。

## 组织

`.lum` 文件是具有固定结构的目录：

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

层缓冲区以层 (`layer-Background.geglbuf`) 命名，而不是按顺序编号。图层名称中的空格存储为下划线；图层组有一个 `-GROUP` 后缀。蒙版共享图层名称 (`mask-Background.geglbuf`)。

每个`recovery/primary-NN.lum/` 都是完整的基线保存。随后按 Ctrl+S 按下附加 `delta-NNNN.lum/` 子目录，其中仅包含自上一个基线以来修改的缓冲区，无论项目大小如何，都能保持检查点保存快速。

自动保存遵循相同的结构，但单独存储在 `~/.cache/lumi/autosave/` 中，使工作文件保持不变。
- **非常大的项目**：具有 1000+ 层和 TB 数据的项目将从延迟加载中受益最多；但是，最终导出为平面图像格式可能需要一些时间。
- **网络驱动器**：支持保存到网络安装目录，但由于 I/O 延迟，速度比本地存储慢。