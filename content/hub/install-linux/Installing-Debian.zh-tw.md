---
title: "安裝 Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

本文說明將 Debian Stable 安裝為 Lumi-o 開發作業系統的流程，也可供搭建類似環境的讀者參考。

選擇 Debian Stable，是因為 Lumi 需要在可預期的長期平台上穩定建置。GIMP 開發面向 Debian Testing，因此 Debian Stable 是與之高度一致的基礎系統。

Lumi-o 在 Debian 與 Cinnamon（X11）上表現最佳，並在該環境中開發與測試。Cinnamon 提供熟悉的類 Windows 桌面工作流程，X11 則是 Lumi 開發中最穩定的環境。

若您來自 Windows，主要概念差異在於：大多數軟體的安裝與設定透過套件管理器與簡單的終端機命令完成，而非下載安裝程式。

## 本指南適合誰

本指南記錄 Lumi 開發所用的 Debian Stable 設定，不是通用的 Linux 安裝教學。

最適合：

- 從 Windows 遷移、希望獲得可預期 Linux 環境的藝術家
- 從原始碼建置 Lumi 的開發者
- 較傾向重現已知可用環境，而非自行設計系統配置的使用者

假定您已熟悉磁碟分割與基本命令列操作。

## 備份資料

安裝 Debian 前，請將整個主目錄完整備份到外接磁碟，並包含需要保留的其他資料夾。

注意：在 Linux 中，`~` 代表您的主目錄。

若使用 Git 儲存庫，請將重要變更推送到遠端，以便安裝後輕鬆還原。此步驟僅適用於已在使用 Git 的讀者。

## 建立分割區

在主磁碟上為 Debian 騰出空間。此步驟有許多指南與工具（包括 GParted）。依您的環境，可以：

- 縮小現有 Windows 分割區以雙開機
- 重用現有 Linux 分割區
- 準備新的 Linux 分割區與 swap 分割區

分割步驟因硬體差異很大；若不確定，請在變更前查閱針對您硬體的指南。

## 建立 Debian 安裝隨身碟

假定目標分割區與 swap 空間已就緒：

1. 從官網下載 Debian ISO：https://www.debian.org/
2. 在 Windows 上，用 BalenaEtcher 將 ISO 寫入隨身碟。
3. 在 Linux 上，用 `dd` 等命令列工具建立可開機隨身碟。

## 安裝 Debian

1. 插入隨身碟。
2. 重新啟動，並在開機時按開機選單鍵（常見為 `F2`、`F12`、`Esc` 或 `Del`）。
3. 選擇隨身碟裝置。
4. 選擇非圖形安裝程式。
5. 將 root 密碼留空，安裝程式會為您的使用者帳戶授予 sudo 權限。
6. 手動分割：

   - 檔案系統：ext4（日誌）
   - Swap：現有 swap 分割區
   - 掛載點：`/`
   - 標籤：`linux`
   - 主機名稱：以 `user@hostname` 形式顯示的系統名稱
   - 使用者帳戶：您的全名
   - 使用者名稱：終端機登入名稱

7. 安裝程式在此階段可選桌面環境；請選擇 Lumi 建議的 **Cinnamon**。
8. 完成安裝並重新開機進入 Debian Stable。

## 系統設定

### 顯示縮放

Debian Stable 目前對分數縮放的處理不一致，尤其在 4K 顯示器上。建議不要降低解析度，而是直接調整介面元素。

建議調整：

- 避免分數顯示縮放
- 選單 → 字型選擇 → 字型設定 → 文字縮放係數：`2.5`
- 桌面字型：`14`
- 面板 → 自訂 → 面板高度：`60`
- 面板外觀 → 右側區域符號圖示大小：`48px`
- 滑鼠與觸控板 → 指標大小調整
- 桌面（右鍵）→ 自訂 → 更大的圖示尺寸

Firefox 調整：

- 網址列 → `about:config`
- 將 `layout.css.devPixelsPerPx` 設為 `1`

### 終端機

設定終端機偏好：

1. 選單 → 終端機 → 編輯 → 偏好設定
2. 文字 → 初始大小：`140 columns`、`40 rows`
3. 文字 → 自訂字型：`Monospace 10`
4. 色彩 → 內建配置 → Solarized Dark

## 還原資料

依需要將備份檔還原到主目錄，例如：

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

注意：以 `.` 開頭的資料夾是 Linux 中的隱藏設定目錄。

## 選用：Git 設定

僅在計畫建置 Lumi 或還原儲存庫時需要。

### 安裝 Git

```bash
sudo apt install git
```

設定身分資訊：

```bash
git config --global --edit
```

#### GitLab 存取

還原對 GitLab 或 GitHub 的儲存庫存取：

1. 變更 SSH 金鑰檔權限：`chmod 600 ~/.ssh/id_rsa`
2. 將金鑰加入 SSH 代理：`ssh-add ~/.ssh/id_rsa`
3. 測試連線：`ssh -T git@ssh.gitlab.gnome.org` 或 `ssh -T git@github.com`

對每個儲存庫，拉取遠端並重置本機分支以符合遠端：

```bash
git reset --hard remote-name/branch-name
git clean -df
```

執行 `git status` 確認儲存庫乾淨。

至此，新作業系統已就緒，資料與儲存庫也已還原。此設定反映 Lumi 開發使用的已知可用環境，可依個人工作流程調整。

## 系統設定後建置 Lumi

Lumi 建置指令碼位於：

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
