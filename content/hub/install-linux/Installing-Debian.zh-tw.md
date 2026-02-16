---
title: "安裝Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
本文檔概述了將 Debian Stable 安裝為 Lumi·o 開發作業系統的過程。對於其他設置類似環境的人來說可能很有用。

選擇 Debian Stable 是因為 Lumi 的目標是在可預測的長期平台上可靠地建造。 GIMP 開發目標是 Debian 測試，使 Debian 穩定版成為一個緊密結合的基礎系統。

如果您來自 Windows，主要的概念變更是大多數軟體安裝和設定是透過套件管理器和簡單的終端命令而不是可下載的安裝程式進行的。

## 本指南適合誰

本指南記錄了用於 Lumi 開發的 Debian Stable 設定。這不是一般的Linux安裝教學。

它最適用於：

- 從 Windows 遷移到需要可預測的 Linux 設定的藝術家
- 開發人員從原始碼建構 Lumi
- 喜歡重現已知工作環境而不是設計自己的系統配置的用戶

假設您基本上熟悉磁碟分割區和簡單的命令列用法。

## 備份您的數據

在安裝 Debian 之前，請在外部磁碟機上建立主目錄的完整備份。包括您想要保留的任何其他資料資料夾。

注意：在 Linux 中，`~` 代表您的主目錄。

如果您使用 Git 儲存庫，請將任何重要變更推送到其來源，以便在安裝後可以輕鬆恢復它們。只有當您已經使用 Git 時，此步驟才有意義。

## 建立分割區

在主磁碟機上為 Debian 建立空間。此步驟有許多指南和工具，包括 GParted。根據您的設置，您可以：

- 縮小現有的 Windows 分割區以實現雙啟動
- 重複使用現有的 Linux 分割區
- 準備新的Linux和交換分區

如果您不確定，請在進行更改之前查閱特定於硬體的指南，因為系統之間的分區步驟差異很大。


## 建立 Debian 安裝 USB

假設目標分區和交換空間已經存在：

1.從官網下載Debian ISO：https://www.debian.org/
2. 在 Windows 上，使用 BalenaEtcher 將 ISO 寫入 USB 隨身碟。
3. 在 Linux 上，使用 `dd` 等命令列工具建立可啟動 USB。

## 安裝 Debian

1. 插入 USB 隨身碟。
2. 重新啟動並在啟動過程中按下啟動選單鍵（通常為`F2`、`F12`、`Esc` 或`Del`）。
3. 選擇USB 設備。
4. 選擇非圖形安裝程式。
5. 出現提示時將 root 密碼留空，以便安裝程式授予對您的使用者帳號的 sudo 存取權。
6.手動分區：

   - 檔案系統：ext4（日誌）
   - 交換：現有交換分區
   - 掛載點：`/`
   - 標籤：`linux`
   - 主機名稱：系統名稱顯示為`user@hostname`
   - 使用者帳戶：您的全名
   - 使用者名稱：終端登入名

7. 選擇 **Cinnamon** 作為桌面環境。
8. 完成安裝並重新啟動進入 Debian Stable。

## 系統設定

### 顯示縮放

Debian Stable 目前處理分數縮放的方式不一致，尤其是在 4K 顯示器上。不降低顯示分辨率，而是直接調整介面元素。

建議調整：- 避免分數顯示縮放。
- 選單 → 字型選擇 → 字型設定 → 文字縮放係數：`2.5`
- 桌面字體：`14`
- 面板→自訂→面板高度：`60`
- 面板外觀 → 右區符號圖示大小：`48px`
- 滑鼠和觸控板→指標大小調整
- 桌面（右鍵）→ 自訂→ 更大的圖示尺寸

火狐瀏覽器調整：

- 網址列 → `about:config`
- 將`layout.css.devPixelsPerPx`設定為`1`

### 終端

配置終端首選項：

1. 選單 → 終端 → 編輯 → 首選項
2. 文字→初始大小：`140 columns`、`40 rows`
3.文字→自訂字體：`Monospace 10`
4. 顏色 → 內建方案 → Solarized Dark

## 復原數據

根據需要將備份檔案還原到主目錄中，例如：

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注意：以`.` 開頭的資料夾是Linux 中的隱藏配置目錄。

## 可選：Git 設定

僅當您計劃建立 Lumi 或恢復存儲庫時才需要。

### 安裝 Git

```bash
sudo apt install git
```

配置您的身分：

```bash
git config --global --edit
```

#### GitLab 訪問

恢復對 GitLab 或 GitHub 的儲存庫存取：

1. 變更 SSH 金鑰檔案的權限：`chmod 600 ~/.ssh/id_rsa`
2. 將使用者加入新的 Git 安裝：`ssh-add ~/.ssh/id_rsa`
3. 測試連線：`ssh -T git@ssh.gitlab.gnome.org` 或 `ssh -T git@github.com`

對於每個儲存庫，獲取來源並重置本機分支以匹配：

```bash
git reset --hard remote-name/branch-name
git clean -df
```

運行 `git status` 以確認儲存庫是乾淨的。

我們現在有了一個新的作業系統，所有資料和儲存庫都已恢復。此設定反映了用於 Lumi 開發的已知工作環境，並且可以根據需要適應單獨的工作流程。

## 作業系統設定後建置 Lumi

Lumi 建置腳本位於：

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