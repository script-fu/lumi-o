---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## 什么是 Cursor？

[Cursor](https://cursor.com/) 是在同一窗口里提供 AI 协助的代码编辑器。它基于 Visual Studio Code，因此语法高亮、扩展、工作区和 Git 都保留下来。这里用 Cursor 编写 Lumi 脚本，因为这种 AI 协助经济得多。

- **语法高亮**：高亮给代码上色。错误更容易看见，结构更清楚，复杂的代码也更好读。
- **扩展**：Cursor 运行 Visual Studio Code 的扩展。语言、主题或工具，都能找到合适的扩展。
- **版本控制集成**：Git 是内置的。借助 [GitHub](https://github.com/) 或 [GitLab](https://gitlab.com/) 这类服务，跟踪改动和管理分支都很方便。
- **编辑器里的 AI**：可以在已经打开的文件旁边请求一次搜索、一次重构或一处小改动。这项协助的费用低到可以当作日常写脚本的一部分。

### 适合初学者的主要功能

- **工作区**：Cursor 把多个项目组织成“工作区”，让相关的文件和文件夹待在一起。浏览和管理项目因此更简单。

- **配置文件**：配置文件可以为不同的项目或任务准备不同的设置。一套用于网页开发，另一套用于 Scheme，各自带上自己的扩展、主题和设置。

### 开始使用 Cursor

开始使用 Cursor：

- **下载并安装**：打开 [Cursor 网站](https://cursor.com/)，下载 Linux 版本。

- **熟悉界面**：打开 Cursor 后会看到几个面板。左侧是**活动栏**，可以从这里在文件、搜索、Git 和扩展之间切换。

- **打开文件或文件夹**：加入项目所在的文件夹来建立工作区，并把当前配置保存为工作区文件。

工作区只是一份保存下来的文件夹视图，再加上该项目的编辑器设置。

### 扩展

- **安装扩展**：点击活动栏上的**扩展**图标，浏览并安装对 Scheme 有用的工具。主要安装 Allen Huang 的 **“vscode-scheme”**。它提供语法高亮，以及对 Scheme 的基本支持。Lumi 插件就是用 Scheme 写的。

也可以编辑语言文件来调整这个扩展，加入更多 Scheme 关键字，或与 Lumi 脚本有关的词。

如果自己维护一套辅助库，把函数名加进语法高亮，浏览和重构都会更快。

### 小结

用一个便于搜索、重构和快速修改的编辑器，并让 AI 就在手边。Cursor 加上 Scheme 扩展，是这里使用的基础。
