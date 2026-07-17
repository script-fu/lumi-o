---
title: "Systemsicherung mit Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Es ist üblich, wichtige Dateien zu sichern, um zu früheren Versionen zurückzukehren oder beschädigte Daten zu ersetzen. Eine weitere wichtige Sicherungsform ist jedoch der **Festplattenklon** — eine vollständige Sicherung des Systemzustands.

Sobald Ihr System eingerichtet ist und zuverlässig läuft, ist ein vollständiges Backup entscheidend, um Ihre Umgebung im Notfall wiederherzustellen. Es ergänzt die regelmäßige Sicherung Ihrer Arbeitsdaten.

[Clonezilla](https://clonezilla.org/) ist eine freie Open-Source-Software für Datenträger-Imaging und -Klonen. Damit lassen sich vollständige Backups der Festplatte erstellen und wiederherstellen — ein beliebtes Werkzeug sowohl für IT-Profis als auch für Heimanwender.

Es ist immer besser, ein Backup zu haben und es nicht zu brauchen, als eines zu brauchen und keines zu haben.


## Hauptfunktionen von Clonezilla

- **Festplatten-Imaging**: Clonezilla erstellt eine exakte Kopie einer Festplatte, einschließlich Betriebssystem, Anwendungen und Daten.
- **Sichern und Wiederherstellen**: Sie können ein Backup-Image einer Festplatte erstellen und es bei Ausfall oder Migration auf ein neues Laufwerk wiederherstellen.
- **Kostenlos und Open Source**: Clonezilla ist vollständig kostenlos, und der Quellcode steht zur Anpassung bereit.


## Mit Clonezilla sichern

### Vorbereitungsschritte

Sie benötigen einen USB-Stick für Clonezilla und eine externe Festplatte, die größer ist als das interne Laufwerk, das Sie klonen möchten.

Diese Schritte vereinfachen den Ablauf auf Basis des [offiziellen Leitfadens](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Lesen Sie die vollständige Anleitung mit Screenshots für zusätzliche Klarheit.

1. **Clonezilla-Live-USB oder CD/DVD erstellen**: Befolgen Sie die detaillierten Anweisungen auf der Clonezilla-[Website](https://clonezilla.org/liveusb.php), um ein bootfähiges Medium zu erstellen.

2. **Externes Backup-Laufwerk anschließen**: Schließen Sie Ihr externes Laufwerk an und stellen Sie sicher, dass es vom System erkannt wird. Es dient als Ziel für das Backup.

3. **Partitionslayout prüfen**: Verwenden Sie den Befehl `lsblk` in einem Terminal, um das Partitionslayout Ihrer primären Festplatte zu prüfen. Notieren Sie den Namen des primären Geräts.

4. **Vom Clonezilla-Live-USB booten**: Starten Sie den Computer neu und booten Sie vom erstellten Clonezilla-Medium. Möglicherweise müssen Sie die BIOS-/UEFI-Einstellungen aufrufen (meist mit F2, F12, ESC oder DEL beim Start) und die Boot-Reihenfolge so anpassen, dass der USB-Stick Vorrang hat.



### Backup mit Clonezilla

1. **Backup-Modus wählen**: Wählen Sie nach dem Start von Clonezilla den Modus „device-device“. In diesem Modus klonen Sie das interne Laufwerk direkt auf ein externes Gerät.

2. **Quellgerät wählen**: Wählen Sie das primäre interne Laufwerk.

3. **Zielgerät wählen**: Wählen Sie das externe Backup-Laufwerk als Ziel. Seien Sie bei der Geräteauswahl vorsichtig, um wichtige Daten nicht zu überschreiben. Stellen Sie sicher, dass das Ziellaufwerk mindestens so groß ist wie das Quelllaufwerk.

4. **Backup starten**: Clonezilla startet den Sicherungsvorgang. Je nach Partitionsgröße und Laufwerksgeschwindigkeit kann das von wenigen Minuten bis zu mehreren Stunden dauern.

5. **Backup beschriften**: Beschriften Sie nach Abschluss den USB-Stick und die externe Festplatte mit Datum und gesichertem System. Bewahren Sie beides sicher auf.

---

### Wiederherstellung aus dem Backup

Wenn Sie Ihr Debian-System aus dem Backup wiederherstellen müssen, gehen Sie so vor:

1. **Vom Clonezilla-Medium booten**: Stecken Sie den Clonezilla-USB-Stick ein und booten Sie wie beim Backup.

2. **Wiederherstellungsmodus wählen**: Wählen Sie erneut den Modus „device-device“, diesmal jedoch zur Wiederherstellung aus dem Backup-Image. Alle Daten werden vom externen Laufwerk zurück auf das interne Laufwerk kopiert.

3. **Quellgerät wählen**: Wählen Sie das externe Laufwerk, auf dem das Backup gespeichert ist.

4. **Zielgerät wählen**: Wählen Sie das interne Laufwerk, auf das Sie das Backup wiederherstellen möchten.

5. **Wiederherstellung starten**: Clonezilla beginnt den Wiederherstellungsprozess. Wie beim Backup hängt die Dauer von Laufwerksgröße und Hardwaregeschwindigkeit ab.

---

## Schlussbemerkungen

Festplatten-Backups mit Clonezilla sichern das gesamte System — Betriebssystem, Einstellungen und Anwendungen. Mit minimalem Aufwand schützen Sie sich vor einem katastrophalen Ausfall und verkürzen Ausfallzeiten nach einem Crash.

Denken Sie daran: **Backups sind unerlässlich**. Aktualisieren Sie sie regelmäßig und testen Sie sie in regelmäßigen Abständen, damit Sie Ihr System bei Bedarf wiederherstellen können.

Nach dem Booten können Sie das externe Backup-Laufwerk anschließen und seine Partitionsstruktur mit dem Disks-Dienstprogramm unter Linux prüfen. Das Backup-Laufwerk sollte die Struktur des internen Laufwerks widerspiegeln — mit denselben Partitionen und etwas ungenutztem Speicherplatz, wenn das externe Laufwerk größer ist.
