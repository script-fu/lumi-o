---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
---
Ein AppImage ist ein einzelnes Linux-Anwendungspaket. Sie laden eine Datei herunter, markieren sie als ausführbar und führen sie aus, ohne systemweit Software zu installieren.

Offizielle AppImage-Website: https://appimage.org/

Das AppImage stellt eine portable Version von Lumi bereit, die ohne Installation oder Systemänderung läuft. Es ist ideal für Künstler, die die Software sofort nutzen möchten, ohne Abhängigkeiten zu verwalten, Quellcode zu kompilieren oder eine Entwicklungsumgebung zu konfigurieren.

Als eigenständige ausführbare Datei kann das AppImage überall im System gespeichert werden. Dies erleichtert das Testen neuer Versionen, das Beibehalten mehrerer Versionen oder das Verschieben der Software zwischen Computern.

Für den Entwicklungsprozess von Lumi fungiert AppImage als tragbarer Testbuild, der der Ausgabe der kontinuierlichen Integration sehr nahe kommt. Dies ermöglicht zuverlässige Tests in einer konsistenten Umgebung, während sich lokale Quellcode-Builds weiterhin auf die Entwicklungsarbeit konzentrieren.

## Release vs. Entwicklung AppImage

- **AppImage veröffentlichen**: noch nicht verfügbar (Lumi wurde nicht veröffentlicht).
- **Entwicklungs-AppImage (CI-Artefakt)**: wird automatisch aus laufenden Entwicklungs-Commits zum Testen generiert.

Dieses Handbuch behandelt hauptsächlich den **Entwicklungs-AppImage**-Workflow.

Aktuelle Artefaktseite:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage-Download-Grundlagen

CI erzeugt Artefakt-ZIP-Dateien (zum Beispiel `lumi-appimage*.zip`).

Grundlegender manueller Ablauf:

1. Laden Sie die neueste CI-Artefakt-ZIP-Datei herunter.
2. Extrahieren Sie es.
3. Führen Sie die enthaltene Datei `Lumi*.AppImage` aus.

Die folgenden Skripte sind optionale Helfer, die diese Schritte automatisieren.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Optionale Hilfsskripte

- `lumi-appimage-unpack-zip.sh`
  - findet das neueste `lumi-appimage*.zip` in `~/Downloads`
  - Installiert AppImage auf `~/AppImage/Lumi/Lumi_CI.AppImage`
  - Installiert Desktop-Ressourcen auf `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  – startet das AppImage in einem Terminal
  - ermöglicht die Laufzeitausgabe (`APPIMAGE_DEBUG=1`)

## Allgemeine Hinweise

- Wenn Sie AppImage manuell ausführen (ohne Hilfsskripte), machen Sie es zuerst ausführbar:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` wendet ausführbare Berechtigungen bereits automatisch an.

- Wenn Lumi bereits von einem anderen Build ausgeführt wird, schließen Sie es, bevor Sie AppImage starten.