---
title: "使用 Clonezilla 進行系統備份"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

備份重要檔案以便回到較早版本或替換損壞資料很常見。但另一類同樣重要的備份是 **磁碟克隆**——對系統狀態的完整備份。

系統設定完成並穩定運作後，建立完整備份至關重要，以便在發生災難時還原環境。這與定期保存工作資料互為補充。

[Clonezilla](https://clonezilla.org/) 是免費的開源磁碟映像與克隆軟體。它可建立並還原電腦硬碟的完整備份，廣受 IT 專業人員與家庭使用者歡迎。

有備份而用不上，總比需要備份卻沒有好。

## Clonezilla 的主要特性

- **磁碟映像**：Clonezilla 建立硬碟的精確副本，包括作業系統、應用程式與資料。
- **備份與還原**：可建立硬碟備份映像，並在故障或遷移到新硬碟時還原。
- **免費開源**：Clonezilla 完全免費，原始碼可修改與自訂。

## 使用 Clonezilla 備份

### 準備工作

您需要一支用於 Clonezilla 的隨身碟，以及一顆比待克隆內建硬碟更大的外接硬碟。

以下步驟依[官方指南](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc)簡化。建議一併閱讀含螢幕截圖的完整指南。

1. **建立 Clonezilla Live 隨身碟或 CD/DVD**：依 Clonezilla [網站](https://clonezilla.org/liveusb.php) 的說明建立可開機隨身碟或 CD/DVD。

2. **連接外接備份硬碟**：插入外接硬碟並確認系統已識別。這將作為備份目的地。

3. **確認分割區配置**：在終端機執行 `lsblk`，查看主硬碟的分割區配置，並記下主要裝置名稱。

4. **從 Clonezilla Live 隨身碟開機**：重新啟動電腦並從建立的 Clonezilla 媒體開機。可能需要在 BIOS/UEFI 設定中（通常在開機時按 F2、F12、Esc 或 Del）將隨身碟設為優先開機。

### Clonezilla 備份步驟

1. **選擇備份模式**：Clonezilla 開機後，選擇 "device-device" 模式。此模式可將內建硬碟直接克隆到外接裝置。

2. **選擇來源裝置**：選擇主要內建硬碟。

3. **選擇目標裝置**：將外接備份硬碟選為目標裝置。選擇時務必小心，避免覆寫重要資料。目標硬碟容量應大於或等於來源硬碟。

4. **開始備份**：Clonezilla 將開始備份。依分割區大小與硬碟速度，可能需要數分鐘到數小時。

5. **為備份做標記**：完成後，在隨身碟與外接硬碟上標註日期及所備份的系統，並存放在安全處。

---

### 從備份還原

若需從備份還原 Debian 系統，請依下列步驟操作：

1. **從 Clonezilla 媒體開機**：插入 Clonezilla 隨身碟並依備份時的相同步驟開機。

2. **選擇還原模式**：再次選擇 "device-device" 模式，但這次從備份映像還原。外接硬碟的資料將複製回內建硬碟。

3. **選擇來源裝置**：選擇存放備份的外接硬碟。

4. **選擇目標裝置**：選擇要還原到的內建硬碟。

5. **開始還原**：Clonezilla 將開始還原。所需時間與備份類似，取決於硬碟大小與硬體速度。

---

## 補充說明

使用 Clonezilla 進行磁碟備份可保留整個系統——作業系統、設定與應用程式。以最小努力即可防範災難性故障，並在當機時減少停機時間。

請記住，**備份至關重要**。請定期更新備份，並週期性測試還原流程，確保需要時能成功還原。

開機後，可連接外接備份硬碟，用 Linux 中的 Disks 工具查看其分割區結構。備份硬碟應鏡像內建硬碟的分割區結構；若外接硬碟較大，會留有未使用空間。
