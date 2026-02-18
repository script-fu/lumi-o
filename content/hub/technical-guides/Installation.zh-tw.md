---
title: "安裝"
type: docs
---
您需要 Git 來執行下面的初始克隆步驟。如果尚未安裝 Git，請先安裝它（Debian/Ubuntu：`sudo apt install git`）或依照：[Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) 克隆 Lumi（首次設定）

為Lumi建立目錄並使用Git克隆原始碼。

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) 安裝依賴項（首次設定）

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) 建造 Lumi（首次設定）

第一次完整設定建置（第一次或重大變更後）：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) 啟動 Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## 可選：重建/編譯

程式碼更改後正常重建：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

快速僅編譯路徑：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

建立單一整合元件（將 `babl` 替換為 `gegl` 或 `gtk3`）：

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## 可選：建置類型

需要時使用`--type`：

- `debug` – 偵錯工作流程
- `debugoptimized` – 開發的平衡預設值
- `release` – 最快的運行時間

範例：

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```