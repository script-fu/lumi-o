---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
---
Den här guiden använder de nuvarande Lumi-byggskripten i:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Installera beroenden (förstagångsinstallation)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Bygg Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Starta Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Byggtyper

Använd `--type` vid behov:

- `debug` – felsökning av arbetsflöden
- `debugoptimized` – balanserad standard för utveckling
- `release` – snabbaste körtiden

Exempel:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```