---
title: "安装"
type: docs
---
您需要 Git 来执行下面的初始克隆步骤。如果尚未安装 Git，请先安装它（Debian/Ubuntu：`sudo apt install git`）或按照：[Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) 克隆 Lumi（首次设置）

为Lumi创建目录并使用Git克隆源代码。

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

第一次完整设置构建（第一次或重大更改后）：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) 启动 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 可选：重建/编译

代码更改后正常重建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

快速仅编译路径：

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

需要时使用`--type`：

- `debug` – 调试工作流程
- `debugoptimized` – 开发的平衡默认值
- `release` – 最快的运行时间

示例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```