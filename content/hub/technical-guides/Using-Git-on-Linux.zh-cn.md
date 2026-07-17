---
title: "在 Linux 上使用 Git"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

欢迎阅读这份 Linux 上使用 Git 的入门指南。本指南帮助你开始使用 Git 和 GitLab，并了解这些工具的基本用法。

## Git 概述

用于开发应用程序的代码保存在系统上的文件夹和文件集合中。Git 是一个可以备份、共享和复制该集合的应用程序。Git 是版本控制系统，可跟踪代码更改并与其他人协作。它是开源社区中广泛使用的强大工具。GitLab 是基于 Web 的平台，可在线托管和管理 Git 仓库，便于协作和跟踪更改。

## 什么是仓库？

_repo_（repository 的缩写）是带有在线副本的 Git 管理本地文件夹。GitLab 仓库是由构成项目的文件和文件夹组成的集合。它可以有 _分支_，即同一项目的独立副本。分支是项目的单独版本，可让你在不影响主版本的情况下进行修改。这有助于在不中断主项目的情况下测试新功能或修复错误。本地仓库存储在硬盘上，远程仓库通过 Git 和 GitLab 在线存储。

## 使用 Git

你需要在系统上安装 Git。在基于 Debian 的系统上，可以使用 apt 命令安装软件包。这里安装的是提供 Git 版本控制系统的 Git 包。sudo 命令授予安装程序在系统上安装的权限。

```bash
 sudo apt install git
```

## 访问 GitLab

在使用 [GitLab](https://gitlab.com/users/sign_up) 之前，你需要访问 GitLab 网站并完成注册以创建账户。

GitLab 在执行 _clone_、_push_ 和 _fetch_ 等 Git 操作时需要 _SSH_，以便在客户端（你）和 GitLab 服务器之间进行安全且经过身份验证的通信。clone 是创建仓库的本地副本，fetch 是将远程更改取回本地，push 是将更改发送到服务器仓库。SSH（Secure Shell）是允许安全远程访问的网络协议，使用 _密钥对_ 进行认证并建立安全连接。可以使用终端中的 ssh-keygen 命令生成 SSH 密钥对。

```bash
 ssh-keygen
```

指定文件名，或按 Enter 使用默认值，也可选择设置密码。如果使用默认名称，主目录中名为 `.ssh` 的隐藏文件夹里会有两个 id_rsa 文件。`.pub` 文件是公钥，可用文本编辑器查看其内容。

登录 GitLab 账户并进入用户设置。点击左侧导航菜单中的“SSH Keys”。将公钥复制并粘贴到 Key 字段，并为密钥设置一个易识别的标题，例如 PC@Home。点击“Add Key”保存。SSH 公钥现已添加到你的 GitLab 账户，可用于 GitLab 仓库认证。使用 `ssh -T` 命令测试密钥和连接是否正常，并查看 GitLab 的欢迎消息。

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## 基本 Git 命令

安装 Git 并在 GitLab 中设置 SSH 密钥后，下面介绍管理仓库所需的基本 Git 命令。这些命令可帮助你处理现有项目、保持本地副本最新并安全地进行更改。

### 1. **克隆仓库**

克隆是创建远程仓库本地副本的过程。当你要处理 GitLab 上已有的项目时很有用。要克隆仓库，请使用 `git clone` 命令，后跟仓库 URL：

```sh
git clone https://gitlab.com/username/repository.git
```

将 `https://gitlab.com/username/repository.git` 替换为要克隆的仓库 URL。此命令会在新目录中创建仓库的本地副本。

### 2. **检查仓库状态**

要查看本地仓库是否有更改或查看当前状态，请使用：

```sh
git status
```

此命令显示本地副本中已修改、添加或删除的文件。

### 3. **远程仓库**

远程仓库是托管在线的项目版本，例如在 GitLab 上。它们是存储代码并可被他人访问的中心位置。克隆项目时 Git 创建的默认远程仓库称为 `origin`。可以使用以下命令添加、删除或列出远程仓库：

- **列出远程仓库：**

  要查看哪些远程仓库链接到本地项目，请使用：

  ```sh
  git remote -v
  ```

  此命令列出所有远程仓库及其 URL。通常这里会显示 `origin`。

- **添加远程仓库：**

  如需添加新的远程仓库，可以使用：

  ```sh
  git remote add <name> <url>
  ```

  将 `<name>` 替换为远程名称，将 `<url>` 替换为仓库 URL。

- **删除远程仓库：**

  要删除远程仓库，请使用：

  ```sh
  git remote remove <name>
  ```

  将 `<name>` 替换为要删除的远程名称。

### 4. **从远程仓库获取更改**

如果要在不应用到本地副本的情况下查看远程仓库的更改，请使用：

```sh
git fetch origin
```

此命令从远程仓库获取最新更改，但不会合并到本地分支。这是在决定是否合并更新之前检查更新的方法。

### 5. **重置本地仓库**

如果想让本地仓库与远程仓库完全一致，可以使用硬重置。**警告：** 这将覆盖所有本地更改。

```sh
git reset --hard origin/branch-name
```

将 `branch-name` 替换为要重置的分支名称。此命令会丢弃所有本地更改，使本地仓库与远程仓库相同。

### 6. **查看提交历史**

要查看仓库随时间变化的提交列表，请使用：

```sh
git log
```

此命令显示提交历史，包括每次更改的作者、日期和消息。有助于了解进行了哪些更改以及何时进行。

### 总结

这些基本 Git 命令可帮助你操作仓库、保持本地副本最新并安全管理远程仓库。克隆仓库、检查本地状态和远程仓库管理是使用 Git 管理项目的核心技能。
