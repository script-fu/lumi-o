---
title: "Installation"
type: docs
---
Sie benötigen Git für den ersten Klonschritt unten. Wenn Git noch nicht installiert ist, installieren Sie es zuerst (Debian/Ubuntu: `sudo apt install git`) oder folgen Sie: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi klonen (Ersteinrichtung)

Erstellen Sie das Verzeichnis für Lumi und verwenden Sie Git, um den Quellcode zu klonen.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Abhängigkeiten installieren (Ersteinrichtung)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi erstellen (Ersteinrichtung)

Erster vollständiger Setup-Build (zum ersten Mal oder nach größeren Änderungen):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Starten Sie Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Optional: Neu erstellen/kompilieren

Normaler Neuaufbau nach Codeänderungen:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Schneller Pfad nur zum Kompilieren:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Erstellen Sie eine einzelne integrierte Komponente (ersetzen Sie `babl` durch `gegl` oder `gtk3`):

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