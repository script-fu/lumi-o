---
title: "Linux システムの概要"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux は強力で汎用性の高いオペレーティングシステムで、大規模な開発者コミュニティがあります。Linux システムの中核には、スムーズなユーザー体験のために連携する主要コンポーネントがあります。本概要では、カーネル、ディストリビューション、パッケージマネージャー、ディスプレイマネージャー、デスクトップ環境、ディスプレイサーバー（X11 または Wayland）など、Linux システムの基本要素を説明します。

Lumi は Debian と Cinnamon（X11）で最も快適に動作し、その環境で開発・テストされています。

**主要 Linux ディストリビューションの一般的な既定値**

| **ディストリビューション** | **パッケージマネージャー** | **ディスプレイマネージャー** | **デスクトップ環境** | **ディスプレイサーバー** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | ユーザー選択          | ユーザー選択             | ユーザー選択        |

### 主要用語

#### カーネル

ハードウェア（通常は Linux）と直接やり取りする、オペレーティングシステムの中核です。

#### ディストリビューション

カーネルに加え、ユーザースペースのツール、ライブラリ、ソフトウェアをまとめた Linux ディストリビューションです。例: Debian、Arch Linux、Fedora。

#### パッケージマネージャー

リポジトリからソフトウェアをインストール、更新、削除するツールです。例: Debian 系の APT、Fedora の DNF、Arch Linux の Pacman。

#### ディスプレイマネージャー

グラフィカルなログイン画面とセッション開始を管理します。例: GDM（GNOME Display Manager）、LightDM、SDDM（Simple Desktop Display Manager）。

#### デスクトップ環境

グラフィカルユーザーインターフェース（GUI）を提供し、見た目とユーザー体験全体を管理します。例: GNOME、Cinnamon、KDE Plasma。

#### ディスプレイサーバー

表示出力と入力イベントを管理します。例: X11（X Window System）と Wayland。X11 は従来型のディスプレイサーバー、Wayland はより新しく安全な代替です。
