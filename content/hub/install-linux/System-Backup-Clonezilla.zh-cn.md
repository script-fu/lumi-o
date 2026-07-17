---
title: "使用 Clonezilla 进行系统备份"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

备份重要文件以便回到较早版本或替换损坏数据很常见。但另一类同样重要的备份是 **磁盘克隆**——对系统状态的完整备份。

系统配置完成并稳定运行后，创建完整备份至关重要，以便在发生灾难时恢复环境。这与定期保存工作数据互为补充。

[Clonezilla](https://clonezilla.org/) 是免费的开源磁盘映像与克隆软件。它可创建并恢复计算机硬盘的完整备份，深受 IT 专业人员与家庭用户欢迎。

有备份而用不上，总比需要备份却没有好。

## Clonezilla 的主要特性

- **磁盘映像**：Clonezilla 创建硬盘的精确副本，包括操作系统、应用程序和数据。
- **备份与恢复**：可创建硬盘备份映像，并在故障或迁移到新硬盘时恢复。
- **免费开源**：Clonezilla 完全免费，源代码可修改和定制。

## 使用 Clonezilla 备份

### 准备工作

您需要一个用于 Clonezilla 的 U 盘，以及一块比待克隆内置硬盘更大的外置硬盘。

以下步骤基于[官方指南](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc)做了简化。建议同时阅读含截图的完整指南。

1. **创建 Clonezilla Live U 盘或 CD/DVD**：按 Clonezilla [网站](https://clonezilla.org/liveusb.php) 的说明创建可启动 U 盘或 CD/DVD。

2. **连接外置备份硬盘**：插入外置硬盘并确认系统已识别。这将作为备份目标。

3. **确认分区布局**：在终端运行 `lsblk`，查看主硬盘的分区布局，并记下主设备名称。

4. **从 Clonezilla Live U 盘启动**：重启计算机并从创建的 Clonezilla 介质启动。可能需要在 BIOS/UEFI 设置中（通常在启动时按 F2、F12、Esc 或 Del）将 U 盘设为优先启动。

### Clonezilla 备份步骤

1. **选择备份模式**：Clonezilla 启动后，选择 "device-device" 模式。该模式可将内置硬盘直接克隆到外置设备。

2. **选择源设备**：选择主内置硬盘。

3. **选择目标设备**：将外置备份硬盘选为目标设备。选择时务必小心，避免覆盖重要数据。目标硬盘容量应大于或等于源硬盘。

4. **开始备份**：Clonezilla 将开始备份。根据分区大小和硬盘速度，可能需要数分钟到数小时。

5. **为备份做标记**：完成后，在 U 盘和外置硬盘上标注日期及所备份的系统，并存放在安全处。

---

### 从备份恢复

若需从备份恢复 Debian 系统，请按以下步骤操作：

1. **从 Clonezilla 介质启动**：插入 Clonezilla U 盘并按备份时的相同步骤启动。

2. **选择恢复模式**：再次选择 "device-device" 模式，但这次从备份映像恢复。外置硬盘的数据将复制回内置硬盘。

3. **选择源设备**：选择存放备份的外置硬盘。

4. **选择目标设备**：选择要恢复到的内置硬盘。

5. **开始恢复**：Clonezilla 将开始恢复。所需时间与备份类似，取决于硬盘大小和硬件速度。

---

## 补充说明

使用 Clonezilla 进行磁盘备份可保留整个系统——操作系统、设置和应用程序。以最小努力即可防范灾难性故障，并在崩溃时减少停机时间。

请记住，**备份至关重要**。请定期更新备份，并周期性测试恢复流程，确保需要时能成功还原。

启动后，可连接外置备份硬盘，用 Linux 中的 Disks 工具查看其分区结构。备份硬盘应镜像内置硬盘的分区结构；若外置硬盘更大，会留有未使用空间。
