---
title: "应用程序图像"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
AppImage 是一个单文件 Linux 应用程序包。您下载一个文件，将其标记为可执行文件，然后运行它，而无需在系统范围内安装软件。

官方AppImage网站：https://appimage.org/

AppImage 提供了 Lumi 的便携式版本，无需安装或修改系统即可运行。对于想要立即使用该软件而无需管理依赖项、编译源代码或配置开发环境的艺术家来说，它是理想的选择。

作为一个独立的可执行文件，AppImage 可以存储在系统上的任何位置。这使得测试新版本、保留多个版本或在计算机之间移动软件变得容易。

对于 Lumi 的开发过程，AppImage 充当便携式测试构建，与持续集成输出紧密匹配。这允许在一致的环境中进行可靠的测试，同时保持本地源构建专注于开发工作。

## 发布与开发 AppImage

- **发布AppImage**：尚不可用（Lumi 尚未发布）。
- **开发 AppImage（CI 工件）**：从正在进行的开发提交自动生成以进行测试。

本指南主要涵盖**开发 AppImage** 工作流程。

当前工件页面：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 下载基础知识

CI 生成工件 zip 文件（例如 `lumi-appimage*.zip`）。

基本手动流程：

1. 下载最新的 CI 工件 zip。
2. 提取它。
3. 运行包含的`Lumi*.AppImage` 文件。

下面的脚本是可选的帮助程序，可自动执行这些步骤。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 可选的帮助脚本

- `lumi-appimage-unpack-zip.sh`
  - 在`~/Downloads`中找到最新的`lumi-appimage*.zip`
  - 将 AppImage 安装到`~/AppImage/Lumi/Lumi_CI.AppImage`
  - 将桌面资源安装到`~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - 在终端中启动 AppImage
  - 启用运行时输出 (`APPIMAGE_DEBUG=1`)

## 常用注意事项

- 如果您手动运行 AppImage（没有帮助程序脚本），请首先使其可执行：

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` 已自动应用可执行权限。

- 如果 Lumi 已经从另一个版本运行，请在启动 AppImage 之前将其关闭。