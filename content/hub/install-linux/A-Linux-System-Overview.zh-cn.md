---
title: "Linux 系统概述"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux 是功能强大、用途广泛的操作系统，拥有庞大的开发者社区。Linux 系统的核心由若干关键组件协同工作，以提供流畅的用户体验。本概述介绍 Linux 系统的基本组成部分，包括内核、发行版、包管理器、显示管理器、桌面环境和显示服务器（X11 或 Wayland）。

Lumi 在 Debian 上、于 Cinnamon（X11）中开发并测试。那仍然是开发环境。Wayland 上的 GNOME 已与该环境一并测试，范围是应用程序的整体外观和 OpenGL 渲染。

**常见 Linux 发行版的默认配置**

| **发行版** | **包管理器** | **显示管理器** | **桌面环境** | **显示服务器** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | 用户自选              | 用户自选                 | 用户自选            |

### 关键术语

#### 内核

直接与硬件交互的操作系统核心，通常为 Linux。

#### 发行版

将内核与用户空间工具、库和软件打包在一起的 Linux 发行版。例如 Debian、Arch Linux、Fedora。

#### 包管理器

用于从仓库安装、更新和卸载软件的工具。例如 Debian 系的 APT、Fedora 的 DNF、Arch Linux 的 Pacman。

#### 显示管理器

管理图形登录界面和会话启动。例如 GDM（GNOME Display Manager）、LightDM、SDDM（Simple Desktop Display Manager）。

#### 桌面环境

提供图形用户界面（GUI），并管理整体外观与用户体验。例如 GNOME、Cinnamon、KDE Plasma。

#### 显示服务器

管理显示输出和输入事件。例如 X11（X Window System）和 Wayland。X11 是传统的显示服务器，Wayland 是较新、更安全的替代方案。
