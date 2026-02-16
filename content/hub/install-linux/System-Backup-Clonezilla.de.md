---
title: "Systemsicherung mit Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Es ist üblich, Ihre wichtigen Dateien zu sichern, um zu früheren Versionen zurückzukehren oder beschädigte Daten zu ersetzen. Eine weitere wichtige Art der Sicherung ist jedoch ein **Festplattenklon**, eine vollständige Sicherung des Zustands Ihres Systems.

Sobald Ihr System eingerichtet ist und ordnungsgemäß funktioniert, ist die Erstellung eines vollständigen Backups von entscheidender Bedeutung für die Wiederherstellung Ihrer Umgebung im Katastrophenfall. Dieses Backup ergänzt die regelmäßige Sicherung Ihrer Arbeitsdaten.

[Clonezilla](https://clonezilla.org/) ist eine kostenlose Open-Source-Software zum Erstellen und Klonen von Datenträgern. Benutzer können damit vollständige Backups der Festplatte ihres Computers erstellen und wiederherstellen, was es zu einem beliebten Tool für IT-Experten und Heimanwender gleichermaßen macht.

Es ist immer besser, ein Backup zu haben und es nicht zu brauchen, als ein Backup zu brauchen und es nicht zu haben.


## Hauptfunktionen von Clonezilla

- **Festplatten-Imaging**: Clonezilla erstellt eine exakte Kopie einer Festplatte, einschließlich des Betriebssystems, der Anwendungen und der Daten.
- **Sichern und Wiederherstellen**: Damit können Sie ein Backup-Image einer Festplatte erstellen und es im Falle eines Fehlers oder einer Migration auf ein neues Laufwerk wiederherstellen.
- **Kostenlos und Open Source**: Die Nutzung von Clonezilla ist völlig kostenlos und der Quellcode steht zur Änderung und Anpassung zur Verfügung.


## Verwenden von Clonezilla zum Sichern

### Vorbereitungsschritte

Sie benötigen ein USB-Laufwerk für Clonezilla und eine externe Festplatte, die größer ist als die interne Festplatte, die Sie klonen möchten.

Diese Schritte vereinfachen den Prozess basierend auf dem [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Es empfiehlt sich, die vollständige Anleitung durchzulesen, die zur besseren Verdeutlichung auch Screenshots enthält.

1. **Erstellen Sie einen Clonezilla Live-USB oder eine CD/DVD**: Befolgen Sie die detaillierten Anweisungen auf dem Clonezilla [website](https://clonezilla.org/liveusb.php), um einen bootfähigen USB-Stick oder eine bootfähige CD/DVD zu erstellen.

2. **Schließen Sie Ihr externes Backup-Laufwerk an**: Schließen Sie Ihr externes Laufwerk an und stellen Sie sicher, dass es von Ihrem System erkannt wird. Dies ist das Ziel für Ihr Backup.

3. **Überprüfen Sie Ihr Partitionslayout**: Verwenden Sie den Befehl `lsblk` in einem Terminal, um das Partitionslayout Ihrer primären Festplatte zu überprüfen. Notieren Sie sich den primären Gerätenamen.

4. **Booten Sie vom Clonezilla Live-USB-Laufwerk**: Starten Sie Ihren Computer neu und booten Sie von dem von Ihnen erstellten Clonezilla-Medium. Möglicherweise müssen Sie auf Ihre BIOS-/UEFI-Einstellungen zugreifen (normalerweise durch Drücken von F2, F12, ESC oder DEL während des Startvorgangs) und die Startreihenfolge anpassen, um dem USB-Laufwerk Priorität einzuräumen.



### Backup mit Clonezilla

1. **Sicherungsmodus auswählen**: Sobald Clonezilla hochfährt, wählen Sie den „Gerät-Gerät“-Modus. In diesem Modus können Sie Ihr internes Laufwerk direkt auf ein externes Gerät klonen.

2. **Wählen Sie das Quellgerät aus**: Wählen Sie das primäre interne Laufwerk.

3. **Wählen Sie das Zielgerät aus**: Wählen Sie Ihr externes Sicherungslaufwerk als Zielgerät. Seien Sie bei der Auswahl des Geräts vorsichtig, um ein Überschreiben wichtiger Daten zu vermeiden. Stellen Sie sicher, dass das Ziellaufwerk genauso groß oder größer als das Quelllaufwerk ist.

4. **Sicherungsvorgang starten**: Clonezilla startet den Sicherungsvorgang. Abhängig von der Größe Ihrer Partition und der Geschwindigkeit Ihrer Laufwerke kann dies einige Minuten bis einige Stunden dauern.

5. **Beschriften Sie Ihr Backup**: Nachdem das Backup abgeschlossen ist, beschriften Sie das USB-Laufwerk und die externe Festplatte mit dem Datum und dem System, das Sie gesichert haben. Bewahren Sie sie an einem sicheren Ort auf.

---

### Wiederherstellung aus dem Backup

Wenn Sie Ihr Debian-System aus dem Backup wiederherstellen müssen, gehen Sie folgendermaßen vor:

1. **Booten von Clonezilla-Medien**: Stecken Sie den Clonezilla-USB-Stick ein und booten Sie von dort. Befolgen Sie dabei die gleichen Schritte wie während des Sicherungsvorgangs.2. **Wiederherstellungsmodus auswählen**: Wählen Sie erneut den Modus „Gerät-Gerät“, aber dieses Mal erfolgt die Wiederherstellung vom Backup-Image. Dadurch werden alle Daten von Ihrem externen Laufwerk zurück auf Ihr internes Laufwerk kopiert.

3. **Wählen Sie das Quellgerät aus**: Wählen Sie Ihr externes Laufwerk, auf dem das Backup gespeichert ist.

4. **Wählen Sie das Zielgerät aus**: Wählen Sie das interne Laufwerk aus, auf dem Sie die Sicherung wiederherstellen möchten.

5. **Starten Sie den Wiederherstellungsprozess**: Clonezilla beginnt mit dem Wiederherstellungsprozess. Wie beim Backup hängt die benötigte Zeit von der Größe des Laufwerks und der Geschwindigkeit Ihrer Hardware ab.

---

## Schlussbemerkungen

Festplattensicherungen mit Clonezilla stellen sicher, dass Ihr gesamtes System – Betriebssystem, Einstellungen und Anwendungen – erhalten bleibt. Mit minimalem Aufwand können Sie Ihr System vor einem katastrophalen Ausfall schützen und Ausfallzeiten im Falle eines Absturzes minimieren.

Denken Sie daran: **Backups sind unerlässlich**. Aktualisieren Sie Ihre Backups regelmäßig und testen Sie sie regelmäßig, um sicherzustellen, dass Sie Ihr System bei Bedarf wiederherstellen können.

Nach dem Booten können Sie Ihr externes Sicherungslaufwerk anschließen und seine Partitionsstruktur mit dem Festplatten-Dienstprogramm in Linux überprüfen. Das Sicherungslaufwerk sollte die Struktur des internen Laufwerks widerspiegeln, mit denselben Partitionen und etwas ungenutztem Speicherplatz, wenn das externe Laufwerk größer ist.