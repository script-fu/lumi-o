---
title: "安装Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
本文档概述了将 Debian Stable 安装为 Lumi·o 开发操作系统的过程。对于其他设置类似环境的人来说可能很有用。

选择 Debian Stable 是因为 Lumi 的目标是在可预测的长期平台上可靠地构建。 GIMP 开发目标是 Debian 测试，使 Debian 稳定版成为一个紧密结合的基础系统。

如果您来自 Windows，主要的概念变化是大多数软件安装和配置是通过包管理器和简单的终端命令而不是可下载的安装程序进行的。

## 本指南适合谁

本指南记录了用于 Lumi 开发的 Debian Stable 设置。这不是一般的Linux安装教程。

它最适用于：

- 从 Windows 迁移到需要可预测的 Linux 设置的艺术家
- 开发人员从源代码构建 Lumi
- 更喜欢重现已知工作环境而不是设计自己的系统配置的用户

假设您基本熟悉磁盘分区和简单的命令行用法。

## 备份您的数据

在安装 Debian 之前，请在外部驱动器上创建主目录的完整备份。包括您想要保留的任何其他数据文件夹。

注意：在 Linux 中，`~` 代表您的主目录。

如果您使用 Git 存储库，请将任何重要更改推送到其来源，以便在安装后可以轻松恢复它们。仅当您已经使用 Git 时，此步骤才有意义。

## 创建分区

在主驱动器上为 Debian 创建空间。此步骤有许多指南和工具，包括 GParted。根据您的设置，您可以：

- 缩小现有的 Windows 分区以实现双启动
- 重用现有的 Linux 分区
- 准备新的Linux和交换分区

如果您不确定，请在进行更改之前查阅特定于硬件的指南，因为系统之间的分区步骤差异很大。


## 创建 Debian 安装 USB

假设目标分区和交换空间已经存在：

1.从官网下载Debian ISO：https://www.debian.org/
2. 在 Windows 上，使用 BalenaEtcher 将 ISO 写入 USB 驱动器。
3. 在 Linux 上，使用 `dd` 等命令行工具创建可启动 USB。

## 安装 Debian

1. 插入 USB 驱动器。
2. 重新启动并在启动过程中按启动菜单键（通常为`F2`、`F12`、`Esc` 或`Del`）。
3. 选择USB 设备。
4. 选择非图形安装程序。
5. 出现提示时将 root 密码留空，以便安装程序授予对您的用户帐户的 sudo 访问权限。
6.手动分区：

   - 文件系统：ext4（日志）
   - 交换：现有交换分区
   - 挂载点：`/`
   - 标签：`linux`
   - 主机名：系统名称显示为`user@hostname`
   - 用户帐户：您的全名
   - 用户名：终端登录名

7. 选择 **Cinnamon** 作为桌面环境。
8. 完成安装并重新启动进入 Debian Stable。

## 系统设置

### 显示缩放

Debian Stable 目前处理分数缩放的方式不一致，尤其是在 4K 显示器上。不降低显示分辨率，而是直接调整界面元素。

建议调整：- 避免分数显示缩放。
- 菜单 → 字体选择 → 字体设置 → 文本缩放系数：`2.5`
- 桌面字体：`14`
- 面板→自定义→面板高度：`60`
- 面板外观 → 右区符号图标大小：`48px`
- 鼠标和触摸板→指针大小调整
- 桌面（右键单击）→ 自定义→ 更大的图标尺寸

火狐浏览器调整：

- 地址栏 → `about:config`
- 将`layout.css.devPixelsPerPx`设置为`1`

### 终端

配置终端首选项：

1. 菜单 → 终端 → 编辑 → 首选项
2. 文本→初始大小：`140 columns`、`40 rows`
3.文本→自定义字体：`Monospace 10`
4. 颜色 → 内置方案 → Solarized Dark

## 恢复数据

根据需要将备份文件恢复到主目录中，例如：

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注意：以`.` 开头的文件夹是Linux 中的隐藏配置目录。

## 可选：Git 设置

仅当您计划构建 Lumi 或恢复存储库时才需要。

### 安装 Git

```bash
sudo apt install git
```

配置您的身份：

```bash
git config --global --edit
```

#### GitLab 访问

恢复对 GitLab 或 GitHub 的存储库访问：

1. 更改 SSH 密钥文件的权限：`chmod 600 ~/.ssh/id_rsa`
2. 将用户添加到新的 Git 安装中：`ssh-add ~/.ssh/id_rsa`
3. 测试连接：`ssh -T git@ssh.gitlab.gnome.org` 或 `ssh -T git@github.com`

对于每个存储库，获取源并重置本地分支以匹配：

```bash
git reset --hard remote-name/branch-name
git clean -df
```

运行 `git status` 以确认存储库是干净的。

我们现在有了一个新的操作系统，所有数据和存储库都已恢复。此设置反映了用于 Lumi 开发的已知工作环境，并且可以根据需要适应单独的工作流程。

## 操作系统设置后构建 Lumi

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