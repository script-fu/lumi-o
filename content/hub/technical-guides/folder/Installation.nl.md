---
title: "Installatie"
type: docs
url: "hub/technical-guides/folder/Installation"
---
Deze handleiding gebruikt de huidige Lumi-buildscripts in:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Afhankelijkheden installeren (eerste installatie)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Bouw Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Start Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Bouwtypen

Gebruik `--type` indien nodig:

- `debug` – foutopsporing in workflows
- `debugoptimized` – evenwichtige standaard voor ontwikkeling
- `release` – snelste looptijd

Voorbeeld:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```