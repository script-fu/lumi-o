---
title: "Debian installieren"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Dieses Dokument beschreibt den Prozess zur Installation von Debian Stable als Lumi·o-Entwicklungsbetriebssystem. Es kann für andere nützlich sein, die eine ähnliche Umgebung einrichten.

Debian Stable wurde ausgewählt, weil Lumi darauf abzielt, zuverlässig auf einer vorhersehbaren langfristigen Plattform aufzubauen. Die GIMP-Entwicklung zielt auf Debian-Tests ab und macht Debian Stable zu einem eng abgestimmten Basissystem.

Wenn Sie mit Windows arbeiten, besteht die wichtigste konzeptionelle Änderung darin, dass die meisten Softwareinstallationen und -konfigurationen über Paketmanager und einfache Terminalbefehle statt über herunterladbare Installationsprogramme erfolgen.

## Für wen dieser Leitfaden gedacht ist

Dieses Handbuch dokumentiert ein funktionierendes Debian Stable-Setup, das für die Lumi-Entwicklung verwendet wird. Es handelt sich nicht um ein allgemeines Tutorial zur Linux-Installation.

Es ist am nützlichsten für:

- Künstler, die von Windows wechseln und ein vorhersehbares Linux-Setup wünschen
- Entwickler, die Lumi aus dem Quellcode erstellen
- Benutzer, die es vorziehen, eine bekannte Arbeitsumgebung zu reproduzieren, anstatt ihre eigene Systemkonfiguration zu entwerfen

Grundlegende Kenntnisse der Festplattenpartitionierung und der einfachen Verwendung der Befehlszeile werden vorausgesetzt.

## Sichern Sie Ihre Daten

Erstellen Sie vor der Installation von Debian eine vollständige Sicherung Ihres Home-Verzeichnisses auf einem externen Laufwerk. Fügen Sie alle zusätzlichen Datenordner hinzu, die Sie beibehalten möchten.

Hinweis: Unter Linux stellt `~` Ihr Home-Verzeichnis dar.

Wenn Sie Git-Repositorys verwenden, übertragen Sie alle wichtigen Änderungen an ihre Ursprünge, damit sie nach der Installation problemlos wiederhergestellt werden können. Dieser Schritt ist nur relevant, wenn Sie Git bereits verwenden.

## Erstellen Sie eine Partition

Schaffen Sie Platz auf Ihrem primären Laufwerk für Debian. Für diesen Schritt gibt es viele Anleitungen und Tools, darunter auch GParted. Abhängig von Ihrem Setup können Sie:

- Verkleinern Sie eine vorhandene Windows-Partition für Dual-Boot
- Wiederverwendung einer vorhandenen Linux-Partition
- Neues Linux vorbereiten und Partitionen austauschen

Wenn Sie sich nicht sicher sind, konsultieren Sie die hardwarespezifischen Handbücher, bevor Sie Änderungen vornehmen, da die Partitionierungsschritte je nach System erheblich variieren.


## Erstellen Sie einen Debian-Installations-USB

Vorausgesetzt, eine Zielpartition und ein Auslagerungsbereich sind bereits vorhanden:

1. Laden Sie die Debian-ISO von der offiziellen Website herunter: https://www.debian.org/
2. Unter Windows verwenden Sie BalenaEtcher, um die ISO auf ein USB-Laufwerk zu schreiben.
3. Verwenden Sie unter Linux ein Befehlszeilentool wie `dd`, um einen bootfähigen USB-Stick zu erstellen.

## Debian installieren

1. Stecken Sie das USB-Laufwerk ein.
2. Starten Sie neu und drücken Sie während des Startvorgangs die Startmenütaste (üblicherweise `F2`, `F12`, `Esc` oder `Del`).
3. Wählen Sie das USB-Gerät aus.
4. Wählen Sie ein nicht-grafisches Installationsprogramm.
5. Lassen Sie das Root-Passwort leer, wenn Sie dazu aufgefordert werden, damit das Installationsprogramm Sudo-Zugriff auf Ihr Benutzerkonto gewährt.
6. Manuelle Partitionierung:

   - Dateisystem: ext4 (Journaling)
   - Swap: vorhandene Swap-Partition
   - Einhängepunkt: `/`
   - Beschriftung: `linux`
   - Hostname: Systemname, angezeigt als `user@hostname`
   - Benutzerkonto: Ihr vollständiger Name
   - Benutzername: Terminal-Anmeldename

7. Wählen Sie **Cinnamon** als Desktop-Umgebung.
8. Schließen Sie die Installation ab und starten Sie Debian Stable neu.

## System-Setup

### Skalierung anzeigen

Debian Stable verarbeitet die fraktionierte Skalierung derzeit inkonsistent, insbesondere auf 4K-Displays. Anstatt die Bildschirmauflösung zu reduzieren, passen Sie die Elemente der Benutzeroberfläche direkt an.

Empfohlene Anpassungen:- Vermeiden Sie eine fraktionierte Anzeigeskalierung.
- Menü → Schriftartenauswahl → Schriftarteneinstellungen → Textskalierungsfaktor: `2.5`
- Desktop-Schriftart: `14`
- Panel → Anpassen → Panelhöhe: `60`
- Bedienfelddarstellung → Symbolische Symbolgröße der rechten Zone: `48px`
- Maus und Touchpad → Anpassung der Zeigergröße
- Desktop (Rechtsklick) → Anpassen → Größere Symbolgröße

Firefox-Anpassung:

- Adressleiste → `about:config`
- Setzen Sie `layout.css.devPixelsPerPx` auf `1`

### Terminal

Konfigurieren Sie die Terminaleinstellungen:

1. Menü → Terminal → Bearbeiten → Einstellungen
2. Text → Anfangsgröße: `140 columns`, `40 rows`
3. Text → Benutzerdefinierte Schriftart: `Monospace 10`
4. Farben → Integrierte Schemata → Solarisierte Dunkelheit

## Daten wiederherstellen

Stellen Sie gesicherte Dateien nach Bedarf im Home-Verzeichnis wieder her, zum Beispiel:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Hinweis: Ordner, die mit `.` beginnen, sind versteckte Konfigurationsverzeichnisse in Linux.

## Optional: Git-Setup

Nur erforderlich, wenn Sie Lumi erstellen oder Repositorys wiederherstellen möchten.

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

1. Ändern Sie die Berechtigungen für die SSH-Schlüsseldatei: `chmod 600 ~/.ssh/id_rsa`
2. Fügen Sie den Benutzer zur neuen Git-Installation hinzu: `ssh-add ~/.ssh/id_rsa`
3. Testen Sie die Verbindung: `ssh -T git@ssh.gitlab.gnome.org` oder `ssh -T git@github.com`

Rufen Sie für jedes Repository die Ursprünge ab und setzen Sie den lokalen Zweig zurück, damit er übereinstimmt:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Führen Sie `git status` aus, um zu bestätigen, dass die Repositorys sauber sind.

Wir haben jetzt ein neues Betriebssystem, in dem alle Daten und Repositorys wiederhergestellt sind. Dieses Setup spiegelt eine bekannte Arbeitsumgebung für die Lumi-Entwicklung wider und kann bei Bedarf an individuelle Arbeitsabläufe angepasst werden.

## Erstellen Sie Lumi nach dem Betriebssystem-Setup

Lumi-Build-Skripte befinden sich in:

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