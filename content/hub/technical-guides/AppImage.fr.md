---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
---
Une AppImage est un package d’application Linux à fichier unique. Vous téléchargez un fichier, le marquez comme exécutable et l'exécutez sans installer de logiciel à l'échelle du système.

Site officiel AppImage : https://appimage.org/

L'AppImage fournit une version portable de Lumi qui fonctionne sans installation ni modification du système. Il est idéal pour les artistes qui souhaitent utiliser le logiciel immédiatement sans gérer les dépendances, compiler le code source ou configurer un environnement de développement.

En tant qu'exécutable autonome, AppImage peut être stocké n'importe où sur le système. Cela facilite le test des nouvelles versions, la conservation de plusieurs versions ou le déplacement du logiciel entre les machines.

Pour le processus de développement de Lumi, AppImage fonctionne comme une version de test portable qui correspond étroitement à la sortie d'intégration continue. Cela permet des tests fiables dans un environnement cohérent tout en gardant les versions sources locales concentrées sur le travail de développement.

## Version vs développement AppImage

- **Release AppImage** : pas encore disponible (Lumi n'est pas encore sorti).
- **Development AppImage (artefact CI)** : généré automatiquement à partir des validations de développement en cours pour les tests.

Ce guide couvre principalement le workflow de **développement AppImage**.

Page actuelle de l'artefact :

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Bases du téléchargement de CI AppImage

CI produit des fichiers zip d'artefacts (par exemple `lumi-appimage*.zip`).

Flux manuel de base :

1. Téléchargez le dernier zip d'artefact CI.
2. Extrayez-le.
3. Exécutez le fichier `Lumi*.AppImage` inclus.

Les scripts ci-dessous sont des assistants facultatifs qui automatisent ces étapes.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Scripts d'aide facultatifs

- `lumi-appimage-unpack-zip.sh`
  - trouve le dernier `lumi-appimage*.zip` dans `~/Downloads`
  - installe AppImage sur `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installe les ressources du bureau sur `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - lance l'AppImage dans un terminal
  - active la sortie d'exécution (`APPIMAGE_DEBUG=1`)

## Notes communes

- Si vous exécutez AppImage manuellement (sans scripts d'assistance), rendez-le d'abord exécutable :

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` applique déjà automatiquement les autorisations exécutables.

- Si Lumi s'exécute déjà à partir d'une autre version, fermez-la avant de lancer AppImage.