---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
---
Ce guide utilise les scripts de build Lumi actuels dans :

`~/code/lumi-dev/build/lumi/scripts`

## 1) Installer les dépendances (première configuration)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Construire Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Lancez Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Types de construction

Utilisez `--type` si nécessaire :

- `debug` – workflows de débogage
- `debugoptimized` – valeur par défaut équilibrée pour le développement
- `release` – durée d'exécution la plus rapide

Exemple :

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```