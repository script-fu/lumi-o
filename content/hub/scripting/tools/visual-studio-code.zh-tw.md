---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## 什麼是 Cursor？

[Cursor](https://cursor.com/) 是在同一個視窗裡提供 AI 協助的程式碼編輯器。它基於 Visual Studio Code，因此語法醒目提示、擴充功能、工作區和 Git 都保留下來。這裡用 Cursor 編寫 Lumi 指令碼，因為這種 AI 協助經濟得多。

- **語法醒目提示**：醒目提示為程式碼上色。錯誤更容易看見，結構更清楚，複雜的程式碼也更好讀。
- **擴充功能**：Cursor 執行 Visual Studio Code 的擴充功能。語言、主題或工具，都能找到合適的擴充功能。
- **版本控制整合**：Git 是內建的。藉助 [GitHub](https://github.com/) 或 [GitLab](https://gitlab.com/) 這類服務，追蹤改動和管理分支都很方便。
- **編輯器裡的 AI**：可以在已經開啟的檔案旁邊請求一次搜尋、一次重構或一處小改動。這項協助的費用低到可以當作日常撰寫指令碼的一部分。

### 適合初學者的主要功能

- **工作區**：Cursor 把多個專案組織成「工作區」，讓相關的檔案和資料夾待在一起。瀏覽和管理專案因此更簡單。

- **設定檔**：設定檔可以為不同的專案或工作準備不同的設定。一套用於網頁開發，另一套用於 Scheme，各自帶上自己的擴充功能、主題和設定。

### 開始使用 Cursor

開始使用 Cursor：

- **下載並安裝**：開啟 [Cursor 網站](https://cursor.com/)，下載 Linux 版本。

- **熟悉介面**：開啟 Cursor 後會看到幾個面板。左側是**活動列**，可以從這裡在檔案、搜尋、Git 和擴充功能之間切換。

- **開啟檔案或資料夾**：加入專案所在的資料夾來建立工作區，並把目前設定儲存為工作區檔案。

工作區只是一份儲存下來的資料夾檢視，再加上該專案的編輯器設定。

### 擴充功能

- **安裝擴充功能**：點選活動列上的**擴充功能**圖示，瀏覽並安裝對 Scheme 有用的工具。主要安裝 Allen Huang 的 **「vscode-scheme」**。它提供語法醒目提示，以及對 Scheme 的基本支援。Lumi 外掛就是用 Scheme 寫的。

也可以編輯語言檔來調整這個擴充功能，加入更多 Scheme 關鍵字，或與 Lumi 指令碼有關的詞。

如果自己維護一套輔助庫，把函式名稱加進語法醒目提示，瀏覽和重構都會更快。

### 小結

用一個便於搜尋、重構和快速修改的編輯器，並讓 AI 就在手邊。Cursor 加上 Scheme 擴充功能，是這裡使用的基礎。
