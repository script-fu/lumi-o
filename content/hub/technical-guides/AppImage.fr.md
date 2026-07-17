---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Une AppImage est un paquet d'application Linux en un seul fichier. Vous téléchargez un fichier, le rendez exécutable et le lancez sans installer de logiciel à l'échelle du système.

Site officiel AppImage : https://appimage.org/

L'AppImage fournit une version portable de Lumi qui fonctionne sans installation ni modification du système. Il est idéal pour les artistes qui souhaitent utiliser le logiciel immédiatement, sans gérer les dépendances, compiler le code source ou configurer un environnement de développement.

En tant qu'exécutable autonome, l'AppImage peut être stocké n'importe où sur le système. Cela facilite le test de nouvelles versions, la conservation de plusieurs versions ou le déplacement du logiciel entre machines.

Pour le processus de développement de Lumi, l'AppImage sert de build de test portable qui correspond étroitement à la sortie de l'intégration continue. Cela permet des tests fiables dans un environnement cohérent, tout en gardant les builds source locaux concentrés sur le travail de développement.

Remarque : le CI construit l'AppImage à l'aide des sources de dépendances intégrées dans le dépôt de Lumi (BABL/GEGL/GTK3), de sorte que la pile de dépendances est cohérente avec le workflow local `lumi-build-script.sh`.

## AppImage Release vs AppImage de développement

- **Release AppImage** : pas encore disponible (Lumi n'a pas encore été publié).
- **AppImage de développement (artefact CI)** : généré automatiquement à partir des commits de développement en cours, pour les tests.

Ce guide couvre principalement le workflow de **l'AppImage de développement**.

Page actuelle des artefacts :

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Bases du téléchargement de l'AppImage CI

Le CI produit des fichiers zip d'artefacts (par exemple `lumi-appimage*.zip`).

Flux manuel de base :

1. Téléchargez le dernier zip d'artefact CI.
2. Extrayez-le.
3. Exécutez le fichier `Lumi*.AppImage` inclus.

Les scripts ci-dessous sont des assistants facultatifs qui automatisent ces étapes.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Décompresser le dernier zip CI téléchargé depuis ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Lancer l'AppImage avec sortie terminal
bash lumi-appimage-launch.sh
```

## Scripts d'aide facultatifs

- `lumi-appimage-unpack-zip.sh`
  - trouve le dernier `lumi-appimage*.zip` dans `~/Downloads`
  - installe l'AppImage dans `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installe les ressources de bureau dans `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - lance l'AppImage dans un terminal
  - active la sortie d'exécution (`APPIMAGE_DEBUG=1`)

## Notes générales

- Si vous exécutez l'AppImage manuellement (sans scripts d'aide), rendez-le d'abord exécutable :

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` applique déjà automatiquement les permissions exécutables.

- Si Lumi est déjà en cours d'exécution depuis un autre build, fermez-le avant de lancer l'AppImage.
