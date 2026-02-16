---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
---
In dieser Anleitung werden die aktuellen Lumi-Build-Skripte verwendet in:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Abhängigkeiten installieren (Ersteinrichtung)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Lumi bauen

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Starten Sie Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Build-Typen

Verwenden Sie bei Bedarf `--type`:

- `debug` – Debugging-Workflows
- `debugoptimized` – ausgewogener Standard für die Entwicklung
- `release` – schnellste Laufzeit

Beispiel:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```