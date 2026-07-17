---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Vous avez besoin de Git pour l'étape de clonage initiale ci-dessous. Si Git n'est pas encore installé, installez-le d'abord (Debian/Ubuntu : `sudo apt install git`) ou suivez : [Utiliser Git sous Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Cloner Lumi (première configuration)

Créez le répertoire pour Lumi et utilisez Git pour cloner le code source.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Cloner via SSH (comme dans le guide Git ci-dessus)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Ou cloner via HTTPS (sans configuration de clé SSH)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Installer les dépendances (première configuration)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Construire Lumi (première configuration)

Première compilation complète (première fois ou après des changements majeurs) :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lancer Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Facultatif : reconstruire / compiler

Reconstruction normale après modification du code :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Compilation rapide uniquement :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Construire un seul composant intégré (remplacez `babl` par `gegl` ou `gtk3`) :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Facultatif : types de build

Utilisez `--type` si nécessaire :

- `debug` – workflows de débogage
- `debugoptimized` – valeur par défaut équilibrée pour le développement
- `release` – exécution la plus rapide

Exemple :

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
