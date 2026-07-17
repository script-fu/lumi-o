---
title: "安装"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

下面的初始克隆步骤需要 Git。如果尚未安装 Git，请先安装（Debian/Ubuntu：`sudo apt install git`），或参阅：[在 Linux 上使用 Git](/hub/technical-guides/Using-Git-on-Linux/)

## 1) 克隆 Lumi（首次设置）

创建 Lumi 目录，并用 Git 克隆源代码。

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 安装依赖项（首次设置）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) 构建 Lumi（首次设置）

首次或重大更改后的完整设置构建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) 启动 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 可选：重新构建 / 编译

代码更改后的常规重新构建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

仅编译的快速路径：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

构建单个集成组件（将 `babl` 替换为 `gegl` 或 `gtk3`）：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## 可选：构建类型

需要时使用 `--type`：

- `debug` – 调试工作流
- `debugoptimized` – 开发用的平衡默认选项
- `release` – 最快的运行速度

示例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
