---
title: "安装 Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

本文档概述将 Debian Stable 安装为 Lumi-o 开发操作系统的流程，也可供搭建类似环境的读者参考。

选择 Debian Stable，是因为 Lumi 需要在可预期的长期平台上稳定构建。GIMP 开发面向 Debian Testing，因此 Debian Stable 是与之高度一致的基础系统。

Lumi-o 在 Debian 与 Cinnamon（X11）上表现最佳，并在该环境中开发与测试。Cinnamon 提供熟悉的类 Windows 桌面工作流，X11 则是 Lumi 开发中最稳定的环境。

若您来自 Windows，主要概念差异在于：大多数软件的安装与配置通过包管理器和简单终端命令完成，而非下载安装程序。

## 本指南适合谁

本指南记录 Lumi 开发所用的 Debian Stable 配置，不是通用的 Linux 安装教程。

最适合：

- 从 Windows 迁移、希望获得可预期 Linux 环境的艺术家
- 从源码构建 Lumi 的开发者
- 更倾向于复现已知可用环境，而非自行设计系统配置的用户

假定您已熟悉磁盘分区和基本命令行操作。

## 备份数据

安装 Debian 前，请将整个主目录完整备份到外置驱动器，并包含需要保留的其他数据文件夹。

注意：在 Linux 中，`~` 表示您的主目录。

若使用 Git 仓库，请将重要更改推送到远程，以便安装后轻松恢复。此步骤仅适用于已在使用 Git 的读者。

## 创建分区

在主驱动器上为 Debian 腾出空间。此步骤有许多指南和工具（包括 GParted）。根据您的环境，可以：

- 缩小现有 Windows 分区以实现双启动
- 复用现有 Linux 分区
- 准备新的 Linux 分区和 swap 分区

分区步骤因硬件差异很大；若不确定，请在更改前查阅针对您硬件的指南。

## 创建 Debian 安装 U 盘

假定目标分区和 swap 空间已就绪：

1. 从官网下载 Debian ISO：https://www.debian.org/
2. 在 Windows 上，用 BalenaEtcher 将 ISO 写入 U 盘。
3. 在 Linux 上，用 `dd` 等命令行工具创建可启动 U 盘。

## 安装 Debian

1. 插入 U 盘。
2. 重启，并在启动时按引导菜单键（常见为 `F2`、`F12`、`Esc` 或 `Del`）。
3. 选择 U 盘设备。
4. 选择非图形安装程序。
5. 将 root 密码留空，安装程序会为您的用户账户授予 sudo 权限。
6. 手动分区：

   - 文件系统：ext4（日志）
   - Swap：现有 swap 分区
   - 挂载点：`/`
   - 标签：`linux`
   - 主机名：以 `user@hostname` 形式显示的系统名
   - 用户账户：您的全名
   - 用户名：终端登录名

7. 安装程序在此阶段可选择桌面环境；请选择 Lumi 推荐的 **Cinnamon**。
8. 完成安装并重启进入 Debian Stable。

## 系统设置

### 显示缩放

Debian Stable 目前对分数缩放的处理不一致，尤其在 4K 显示器上。建议不要降低分辨率，而是直接调整界面元素。

推荐调整：

- 避免分数显示缩放
- 菜单 → 字体选择 → 字体设置 → 文本缩放系数：`2.5`
- 桌面字体：`14`
- 面板 → 自定义 → 面板高度：`60`
- 面板外观 → 右侧区域符号图标大小：`48px`
- 鼠标和触控板 → 指针大小调整
- 桌面（右键）→ 自定义 → 更大的图标尺寸

Firefox 调整：

- 地址栏 → `about:config`
- 将 `layout.css.devPixelsPerPx` 设为 `1`

### 终端

配置终端首选项：

1. 菜单 → 终端 → 编辑 → 首选项
2. 文本 → 初始大小：`140 columns`、`40 rows`
3. 文本 → 自定义字体：`Monospace 10`
4. 颜色 → 内置方案 → Solarized Dark

## 恢复数据

按需将备份文件恢复到主目录，例如：

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注意：以 `.` 开头的文件夹是 Linux 中的隐藏配置目录。

## 可选：Git 设置

仅在计划构建 Lumi 或恢复仓库时需要。

### 安装 Git

```bash
sudo apt install git
```

配置身份信息：

```bash
git config --global --edit
```

#### GitLab 访问

恢复对 GitLab 或 GitHub 的仓库访问：

1. 修改 SSH 密钥文件权限：`chmod 600 ~/.ssh/id_rsa`
2. 将密钥加入 SSH 代理：`ssh-add ~/.ssh/id_rsa`
3. 测试连接：`ssh -T git@ssh.gitlab.gnome.org` 或 `ssh -T git@github.com`

对每个仓库，拉取远程并重置本地分支以匹配：

```bash
git reset --hard remote-name/branch-name
git clean -df
```

运行 `git status` 确认仓库干净。

至此，新操作系统已就绪，数据与仓库也已恢复。此配置反映 Lumi 开发使用的已知可用环境，可按个人工作流调整。

## 系统设置后构建 Lumi

Lumi 构建脚本位于：

`~/code/lumi-dev/build/lumi/scripts`。

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```
