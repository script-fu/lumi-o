---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Sie benötigen Git für den ersten Klonschritt unten. Wenn Git noch nicht installiert ist, installieren Sie es zuerst (Debian/Ubuntu: `sudo apt install git`) oder folgen Sie: [Git unter Linux verwenden](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi klonen (Ersteinrichtung)

Erstellen Sie das Verzeichnis für Lumi und verwenden Sie Git, um den Quellcode zu klonen.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Per SSH klonen (entspricht dem Git-Leitfaden oben)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Oder per HTTPS klonen (ohne SSH-Schlüssel-Einrichtung)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Abhängigkeiten installieren (Ersteinrichtung)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi erstellen (Ersteinrichtung)

Erster vollständiger Setup-Build (beim ersten Mal oder nach größeren Änderungen):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lumi starten

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Optional: Neu erstellen / kompilieren

Normaler Neuaufbau nach Codeänderungen:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Schneller Kompilierungsweg:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Einzelne integrierte Komponente erstellen (ersetzen Sie `babl` durch `gegl` oder `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Optional: Build-Typen

Verwenden Sie bei Bedarf `--type`:

- `debug` – Debugging-Workflows
- `debugoptimized` – ausgewogener Standard für die Entwicklung
- `release` – schnellste Laufzeit

Beispiel:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
