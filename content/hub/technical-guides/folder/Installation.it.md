---
title: "Installazione"
type: docs
url: "hub/technical-guides/folder/Installation"
---
Questa guida utilizza gli attuali script di build Lumi in:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Installa le dipendenze (prima configurazione)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Costruisci Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Avvia Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Tipi di costruzione

Usa `--type` quando necessario:

- `debug` – debug dei flussi di lavoro
- `debugoptimized` – impostazione predefinita bilanciata per lo sviluppo
- `release` – autonomia più veloce

Esempio:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```