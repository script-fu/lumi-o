---
title: "在 Linux 上使用 Git"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

歡迎閱讀這份在 Linux 上使用 Git 的入門指南。本指南協助您開始使用 Git 和 GitLab，並了解這些工具的基本用法。

## Git 概述

用於開發應用程式的程式碼保存在系統上的資料夾和檔案集合中。Git 是一個可以備份、分享和複製該集合的應用程式。Git 是版本控制系統，可追蹤程式碼變更並與他人協作。它是開源社群中廣泛使用的強大工具。GitLab 是基於 Web 的平台，可線上託管和管理 Git 儲存庫，便於協作和追蹤變更。

## 什麼是儲存庫？

_repo_（repository 的縮寫）是帶有線上副本的 Git 管理本機資料夾。GitLab 儲存庫是由構成專案的檔案和資料夾組成的集合。它可以有 _分支_，即同一專案的獨立副本。分支是專案的單獨版本，可讓您在不影響主版本的情況下進行修改。這有助於在不中斷主專案的情況下測試新功能或修復錯誤。本機儲存庫保存在硬碟上，遠端儲存庫透過 Git 和 GitLab 線上保存。

## 使用 Git

您需要在系統上安裝 Git。在基於 Debian 的系統上，可以使用 apt 命令安裝軟體套件。這裡安裝的是提供 Git 版本控制系統的 Git 套件。sudo 指令會授予安裝程式在系統上安裝的權限。

```bash
 sudo apt install git
```

## 存取 GitLab

在使用 [GitLab](https://gitlab.com/users/sign_up) 之前，您需要造訪 GitLab 網站並完成註冊以建立帳戶。

GitLab 在執行 _clone_、_push_ 和 _fetch_ 等 Git 操作時需要 _SSH_，以便在用戶端（您）和 GitLab 伺服器之間進行安全且經過驗證的通訊。clone 是建立儲存庫的本機副本，fetch 是將遠端變更取回本機，push 是將變更傳送到伺服器儲存庫。SSH（Secure Shell）是允許安全遠端存取的網路協定，使用 _金鑰對_ 進行驗證並建立安全連線。可以在終端機中使用 ssh-keygen 命令產生 SSH 金鑰對。

```bash
 ssh-keygen
```

指定檔名，或按 Enter 使用預設值，也可選擇設定密碼。如果使用預設名稱，主目錄中名為 `.ssh` 的隱藏資料夾裡會有兩個 id_rsa 檔案。`.pub` 檔案是公鑰，可用文字編輯器查看其內容。

登入 GitLab 帳戶並進入使用者設定。點選左側導覽功能表中的「SSH Keys」。將公鑰複製並貼到 Key 欄位，並為金鑰設定一個易識別的標題，例如 PC@Home。點選「Add Key」儲存。SSH 公鑰現已新增至您的 GitLab 帳戶，可用於 GitLab 儲存庫驗證。使用 `ssh -T` 命令測試金鑰和連線是否正常，並查看 GitLab 的歡迎訊息。

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## 基本 Git 指令

安裝 Git 並在 GitLab 中設定 SSH 金鑰後，下面介紹管理儲存庫所需的基本 Git 指令。這些指令可協助您處理現有專案、保持本機副本最新並安全地進行變更。

### 1. **複製儲存庫**

複製是建立遠端儲存庫本機副本的過程。當您要處理 GitLab 上已有的專案時很有用。若要複製儲存庫，請使用 `git clone` 指令，後接儲存庫 URL：

```sh
git clone https://gitlab.com/username/repository.git
```

將 `https://gitlab.com/username/repository.git` 替換為要複製的儲存庫 URL。此指令會在新目錄中建立儲存庫的本機副本。

### 2. **檢查儲存庫狀態**

若要查看本機儲存庫是否有變更或查看目前狀態，請使用：

```sh
git status
```

此指令會顯示本機副本中已修改、新增或刪除的檔案。

### 3. **遠端儲存庫**

遠端儲存庫是託管在線上的專案版本，例如在 GitLab 上。它們是儲存程式碼並可供他人存取的中心位置。複製專案時 Git 建立的預設遠端儲存庫稱為 `origin`。可以使用以下指令新增、刪除或列出遠端儲存庫：

- **列出遠端儲存庫：**

  若要查看哪些遠端儲存庫連結到本機專案，請使用：

  ```sh
  git remote -v
  ```

  此指令會列出所有遠端儲存庫及其 URL。通常這裡會顯示 `origin`。

- **新增遠端儲存庫：**

  如需新增遠端儲存庫，可以使用：

  ```sh
  git remote add <name> <url>
  ```

  將 `<name>` 替換為遠端名稱，將 `<url>` 替換為儲存庫 URL。

- **移除遠端儲存庫：**

  若要刪除遠端儲存庫，請使用：

  ```sh
  git remote remove <name>
  ```

  將 `<name>` 替換為要刪除的遠端名稱。

### 4. **從遠端儲存庫取得變更**

若要在不套用到本機副本的情況下查看遠端儲存庫的變更，請使用：

```sh
git fetch origin
```

此指令會從遠端儲存庫取得最新變更，但不會合併到本機分支。這是在決定是否合併更新之前檢查更新的方法。

### 5. **重設本機儲存庫**

若想讓本機儲存庫與遠端儲存庫完全一致，可以使用硬重設。**警告：** 這將覆寫所有本機變更。

```sh
git reset --hard origin/branch-name
```

將 `branch-name` 替換為要重設的分支名稱。此指令會捨棄所有本機變更，使本機儲存庫與遠端儲存庫相同。

### 6. **查看提交歷史**

若要查看儲存庫隨時間變化的提交列表，請使用：

```sh
git log
```

此指令會顯示提交歷史，包括每次變更的作者、日期和訊息。有助於了解進行了哪些變更以及何時進行。

### 總結

這些基本 Git 指令可協助您操作儲存庫、保持本機副本最新並安全管理遠端儲存庫。複製儲存庫、檢查本機狀態和遠端儲存庫管理是使用 Git 管理專案的核心技能。
