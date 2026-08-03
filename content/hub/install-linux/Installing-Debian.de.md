---
title: "Debian installieren"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e79ae25c72fd6b2a9d31e1efe3019289f4b44d9230990f6874c0332de6c5f19
---

Dieses Dokument beschreibt den Prozess zur Installation von Debian Stable als Lumi-o-Entwicklungsbetriebssystem. Es kann auch für andere nützlich sein, die eine ähnliche Umgebung einrichten.

Debian Stable wurde gewählt, weil Lumi zuverlässig auf einer vorhersehbaren, langfristigen Plattform aufgebaut werden soll. Die GIMP-Entwicklung zielt auf Debian Testing ab, wodurch Debian Stable eine eng anliegende Basissystemwahl ist.

Lumi läuft unter Debian mit Cinnamon (X11) am besten und wird in dieser Umgebung entwickelt und getestet. Cinnamon bietet einen vertrauten, Windows-ähnlichen Desktop-Workflow, während X11 die stabilste Umgebung für die Lumi-Entwicklung geliefert hat.

Wenn Sie von Windows kommen, besteht der wichtigste konzeptionelle Unterschied darin, dass Softwareinstallation und -konfiguration meist über Paketmanager und einfache Terminalbefehle laufen — statt über herunterladbare Installationsprogramme.

## Für wen dieser Leitfaden gedacht ist

Dieser Leitfaden dokumentiert ein funktionierendes Debian-Stable-Setup für die Lumi-Entwicklung. Er ist kein allgemeines Linux-Installationstutorial.

Er ist besonders nützlich für:

- Künstler, die von Windows wechseln und ein vorhersehbares Linux-Setup wünschen
- Entwickler, die Lumi aus dem Quellcode bauen
- Nutzer, die lieber eine bekannte Arbeitsumgebung reproduzieren als eine eigene Systemkonfiguration entwerfen

Grundlegende Kenntnisse in Festplattenpartitionierung und einfacher Befehlszeilennutzung werden vorausgesetzt.

## Sichern Sie Ihre Daten

Erstellen Sie vor der Debian-Installation eine vollständige Sicherung Ihres Home-Verzeichnisses auf einem externen Laufwerk. Nehmen Sie alle zusätzlichen Datenordner mit, die Sie behalten möchten.

Hinweis: Unter Linux steht `~` für Ihr Home-Verzeichnis.

Wenn Sie Git-Repositorys nutzen, pushen Sie wichtige Änderungen zu deren Remotes, damit Sie sie nach der Installation leicht wiederherstellen können. Dieser Schritt ist nur relevant, wenn Sie Git bereits verwenden.

## Partition anlegen

Schaffen Sie Platz auf Ihrem primären Laufwerk für Debian. Für diesen Schritt gibt es viele Anleitungen und Tools, darunter GParted. Je nach Setup können Sie:

- eine vorhandene Windows-Partition für Dual-Boot verkleinern
- eine vorhandene Linux-Partition wiederverwenden
- neue Linux- und Swap-Partitionen vorbereiten

Wenn Sie unsicher sind, lesen Sie hardwarespezifische Anleitungen, bevor Sie Änderungen vornehmen — die Partitionierungsschritte unterscheiden sich je nach System erheblich.


## Debian-Installations-USB erstellen

Vorausgesetzt, Zielpartition und Swap-Bereich existieren bereits:

1. Laden Sie die Debian-ISO von der offiziellen Website herunter: https://www.debian.org/
2. Unter Windows schreiben Sie die ISO mit BalenaEtcher auf einen USB-Stick.
3. Unter Linux erstellen Sie einen bootfähigen USB-Stick mit einem Befehlszeilentool wie `dd`.

## Debian installieren

1. Stecken Sie den USB-Stick ein.
2. Starten Sie neu und drücken Sie während des Startvorgangs die Boot-Menü-Taste (üblicherweise `F2`, `F12`, `Esc` oder `Del`).
3. Wählen Sie das USB-Gerät aus.
4. Wählen Sie einen nicht-grafischen Installer.
5. Lassen Sie das Root-Passwort leer, damit der Installer Ihrem Benutzerkonto sudo-Zugriff gewährt.
6. Partitionieren Sie manuell:

   - Dateisystem: ext4 (Journaling)
   - Swap: vorhandene Swap-Partition
   - Einhängepunkt: `/`
   - Beschriftung: `linux`
   - Hostname: Systemname, angezeigt als `user@hostname`
   - Benutzerkonto: Ihr vollständiger Name
   - Benutzername: Terminal-Anmeldename

7. Der Debian-Installer bietet an dieser Stelle eine Desktop-Umgebungsauswahl; wählen Sie **Cinnamon** für das von Lumi empfohlene Setup.
8. Schließen Sie die Installation ab und starten Sie in Debian Stable neu.

## System-Setup

### Anzeigeskalierung

Debian Stable behandelt fraktionale Skalierung derzeit uneinheitlich, besonders auf 4K-Displays. Statt die Bildschirmauflösung zu verringern, passen Sie die Elemente der Benutzeroberfläche direkt an.

Empfohlene Anpassungen:

- Vermeiden Sie fraktionale Anzeigeskalierung.
- Menü → Schriftartenauswahl → Schriftarteneinstellungen → Textskalierungsfaktor: `2.5`
- Desktop-Schriftart: `14`
- Panel → Anpassen → Panelhöhe: `60`
- Paneldarstellung → Größe der symbolischen Symbole in der rechten Zone: `48px`
- Maus und Touchpad → Zeigergröße anpassen
- Desktop (Rechtsklick) → Anpassen → Größere Symbolgröße

Firefox-Anpassung:

- Adressleiste → `about:config`
- Setzen Sie `layout.css.devPixelsPerPx` auf `1`

### Terminal

Konfigurieren Sie die Terminaleinstellungen:

1. Menü → Terminal → Bearbeiten → Einstellungen
2. Text → Anfangsgröße: `140 columns`, `40 rows`
3. Text → Benutzerdefinierte Schriftart: `Monospace 10`
4. Farben → Integrierte Schemata → Solarized Dark

### Alt-Taste für Werkzeuggrößenänderung

Wenn `Alt` + Rechtsklick und Ziehen in Lumi die Pinselgröße nicht ändert, verwendet der Desktop Alt für die Fensterverwaltung.

1. Suchen Sie im Systemmenü nach **Fenster**.
2. Fenster → Verhalten → Sondertaste zum Verschieben und Vergrößern von Fenstern → **Deaktiviert**

Danach sollte `Alt` + Rechtsklick und Ziehen in Lumi zum Ändern der Werkzeuggröße funktionieren.

## Daten wiederherstellen

Stellen Sie gesicherte Dateien nach Bedarf im Home-Verzeichnis wieder her, zum Beispiel:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Hinweis: Ordner, die mit `.` beginnen, sind versteckte Konfigurationsverzeichnisse unter Linux.

## Optional: Git-Setup

Nur erforderlich, wenn Sie Lumi bauen oder Repositorys wiederherstellen möchten.

### Git installieren

```bash
sudo apt install git
```

Konfigurieren Sie Ihre Identität:

```bash
git config --global --edit
```

#### GitLab-Zugriff

Stellen Sie den Repository-Zugriff auf GitLab oder GitHub wieder her:

1. Setzen Sie die Berechtigungen der SSH-Schlüsseldatei: `chmod 600 ~/.ssh/id_rsa`
2. Fügen Sie den Schlüssel zur neuen Git-Installation hinzu: `ssh-add ~/.ssh/id_rsa`
3. Testen Sie die Verbindung: `ssh -T git@ssh.gitlab.gnome.org` oder `ssh -T git@github.com`

Rufen Sie für jedes Repository die Remotes ab und setzen Sie den lokalen Branch zurück, damit er übereinstimmt:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Führen Sie `git status` aus, um zu bestätigen, dass die Repositorys sauber sind.

Sie haben jetzt ein neues Betriebssystem mit wiederhergestellten Daten und Repositorys. Dieses Setup entspricht einer bekannten Arbeitsumgebung für die Lumi-Entwicklung und lässt sich bei Bedarf an individuelle Workflows anpassen.

## Lumi nach dem Betriebssystem-Setup bauen

Die Lumi-Build-Skripte befinden sich in:

`~/code/lumi-dev/build/lumi/scripts`.

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
