---
title: "Installation"
type: docs
---
Vous avez besoin de Git pour l'étape de clonage initiale ci-dessous. Si Git n'est pas encore installé, installez-le d'abord (Debian/Ubuntu : `sudo apt install git`) ou suivez : [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Cloner Lumi (première configuration)

Créez le répertoire pour Lumi et utilisez Git pour cloner le code source.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Installer les dépendances (première configuration)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Construire Lumi (première configuration)

Première version d'installation complète (première fois ou après des changements majeurs) :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lancez Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Facultatif : Reconstruire / Compiler

Reconstruction normale après modification du code :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Chemin rapide de compilation uniquement :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Créez un seul composant intégré (remplacez `babl` par `gegl` ou `gtk3`) :

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Facultatif : types de build

Utilisez `--type` si nécessaire :

- `debug` – workflows de débogage
- `debugoptimized` – valeur par défaut équilibrée pour le développement
- `release` – durée d'exécution la plus rapide

Exemple :

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```