---
title: "Linux 系統概述"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux 是功能強大且用途廣泛的作業系統，擁有龐大的開發者社群。Linux 系統的核心由若干關鍵元件協同運作，以提供流暢的使用者體驗。本概述介紹 Linux 系統的基本組成部分，包括核心、發行版、套件管理器、顯示管理器、桌面環境與顯示伺服器（X11 或 Wayland）。

Lumi 在 Debian 上、以 Cinnamon（X11）開發並測試。那仍然是開發環境。Wayland 上的 GNOME 則在旁邊一併測過，看的是應用程式的整體外觀，以及 OpenGL 渲染。

**常見 Linux 發行版的預設配置**

| **發行版** | **套件管理器** | **顯示管理器** | **桌面環境** | **顯示伺服器** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | 使用者自選            | 使用者自選               | 使用者自選          |

### 關鍵術語

#### 核心

直接與硬體互動的作業系統核心，通常為 Linux。

#### 發行版

將核心與使用者空間工具、函式庫和軟體打包在一起的 Linux 發行版。例如 Debian、Arch Linux、Fedora。

#### 套件管理器

用於從儲存庫安裝、更新和移除軟體的工具。例如 Debian 系的 APT、Fedora 的 DNF、Arch Linux 的 Pacman。

#### 顯示管理器

管理圖形登入畫面與工作階段啟動。例如 GDM（GNOME Display Manager）、LightDM、SDDM（Simple Desktop Display Manager）。

#### 桌面環境

提供圖形使用者介面（GUI），並管理整體外觀與使用者體驗。例如 GNOME、Cinnamon、KDE Plasma。

#### 顯示伺服器

管理顯示輸出與輸入事件。例如 X11（X Window System）與 Wayland。X11 是傳統的顯示伺服器，Wayland 是較新、更安全的替代方案。
