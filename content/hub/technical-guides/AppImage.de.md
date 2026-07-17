---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Ein AppImage ist ein Linux-Anwendungspaket in einer einzigen Datei. Sie laden eine Datei herunter, markieren sie als ausführbar und führen sie aus, ohne systemweit Software zu installieren.

Offizielle AppImage-Website: https://appimage.org/

Das AppImage stellt eine portable Version von Lumi bereit, die ohne Installation oder Systemänderung läuft. Es ist ideal für Künstler, die die Software sofort nutzen möchten, ohne Abhängigkeiten zu verwalten, Quellcode zu kompilieren oder eine Entwicklungsumgebung zu konfigurieren.

Als eigenständige ausführbare Datei kann das AppImage überall im System gespeichert werden. Das erleichtert das Testen neuer Versionen, das Beibehalten mehrerer Versionen oder das Verschieben der Software zwischen Computern.

Für den Entwicklungsprozess von Lumi fungiert das AppImage als portabler Testbuild, der der Ausgabe der Continuous Integration sehr nahe kommt. Das ermöglicht zuverlässige Tests in einer konsistenten Umgebung, während sich lokale Quellcode-Builds auf die Entwicklungsarbeit konzentrieren.

Hinweis: CI erstellt das AppImage mithilfe der im Repo integrierten Abhängigkeitsquellen von Lumi (BABL/GEGL/GTK3), sodass der Abhängigkeitsstack mit dem lokalen `lumi-build-script.sh`-Workflow konsistent ist.

## Release AppImage vs. Development AppImage

- **Release AppImage**: noch nicht verfügbar (Lumi wurde noch nicht veröffentlicht).
- **Development AppImage (CI-Artefakt)**: wird automatisch aus laufenden Entwicklungs-Commits zum Testen generiert.

Dieser Leitfaden behandelt hauptsächlich den **Development-AppImage**-Workflow.

Aktuelle Artefaktseite:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Grundlagen zum CI-AppImage-Download

CI erzeugt Artefakt-ZIP-Dateien (zum Beispiel `lumi-appimage*.zip`).

Grundlegender manueller Ablauf:

1. Laden Sie die neueste CI-Artefakt-ZIP-Datei herunter.
2. Extrahieren Sie sie.
3. Führen Sie die enthaltene Datei `Lumi*.AppImage` aus.

Die folgenden Skripte sind optionale Helfer, die diese Schritte automatisieren.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Neuestes heruntergeladenes CI-ZIP aus ~/Downloads entpacken
bash lumi-appimage-unpack-zip.sh

# AppImage mit Terminalausgabe starten
bash lumi-appimage-launch.sh
```

## Optionale Hilfsskripte

- `lumi-appimage-unpack-zip.sh`
  - findet das neueste `lumi-appimage*.zip` in `~/Downloads`
  - installiert das AppImage nach `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installiert Desktop-Ressourcen nach `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - startet das AppImage in einem Terminal
  - aktiviert die Laufzeitausgabe (`APPIMAGE_DEBUG=1`)

## Allgemeine Hinweise

- Wenn Sie das AppImage manuell ausführen (ohne Hilfsskripte), machen Sie es zuerst ausführbar:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` wendet ausführbare Berechtigungen bereits automatisch an.

- Wenn Lumi bereits von einem anderen Build ausgeführt wird, schließen Sie es, bevor Sie das AppImage starten.
