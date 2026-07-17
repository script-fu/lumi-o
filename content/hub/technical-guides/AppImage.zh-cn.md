---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage 是单文件 Linux 应用程序包。下载一个文件，将其设为可执行，即可运行，无需在系统范围内安装软件。

官方 AppImage 网站：https://appimage.org/

AppImage 提供无需安装或修改系统即可运行的 Lumi 便携版。对于希望立即使用软件、而不想管理依赖项、编译源代码或配置开发环境的艺术家来说，它是理想选择。

作为自包含的可执行文件，AppImage 可以保存在系统任意位置。这使测试新版本、保留多个版本或在计算机之间移动软件变得容易。

在 Lumi 的开发流程中，AppImage 充当与持续集成输出紧密匹配的便携测试构建。这能在保持一致环境进行可靠测试的同时，让本地源代码构建专注于开发工作。

注意：CI 使用 Lumi 的仓库内集成依赖源（BABL/GEGL/GTK3）构建 AppImage，因此依赖栈与本地 `lumi-build-script.sh` 工作流一致。

## 发布版与开发版 AppImage

- **发布 AppImage**：尚不可用（Lumi 尚未发布）。
- **开发 AppImage（CI 工件）**：从进行中的开发提交自动生成，供测试使用。

本指南主要介绍 **开发 AppImage** 工作流。

当前工件页面：

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage 下载基础

CI 会生成工件 zip 文件（例如 `lumi-appimage*.zip`）。

基本手动流程：

1. 下载最新的 CI 工件 zip
2. 解压
3. 运行包含的 `Lumi*.AppImage` 文件

以下脚本是自动执行这些步骤的可选辅助工具。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## 可选辅助脚本

- `lumi-appimage-unpack-zip.sh`
  - 在 `~/Downloads` 中查找最新的 `lumi-appimage*.zip`
  - 将 AppImage 安装到 `~/AppImage/Lumi/Lumi_CI.AppImage`
  - 将桌面资源安装到 `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - 在终端中启动 AppImage
  - 启用运行时输出（`APPIMAGE_DEBUG=1`）

## 常见注意事项

- 如果手动运行 AppImage（不使用辅助脚本），请先设为可执行：

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` 会自动应用可执行权限。

- 如果 Lumi 已在其他构建中运行，请在启动 AppImage 之前将其关闭。
