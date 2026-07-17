---
title: "Format de fichier (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

Le format de fichier natif de Lumi est conçu pour les projets de peinture en calques qui doivent rester fiables, inspectables et récupérables dans la durée. Il tient compte des réalités du travail d'illustration : de nombreux calques, de grandes toiles, des informations couleur intégrées, des masques, des effets et des données de récupération.

Plutôt que de traiter un projet comme une masse opaque, le format laisse la structure de l'illustration visible pour l'application. Lumi peut ainsi enregistrer, charger et récupérer de grandes images de façon plus intelligente tout en préservant l'organisation dont les artistes dépendent.

## Structure de projet ouverte

Un projet Lumi sépare les composants de l'illustration : structure d'image, contenu des calques, masques, données couleur, métadonnées et informations de récupération ont chacun un rôle clair. Le format est ainsi plus facile à comprendre et mieux adapté à un accès à long terme qu'un conteneur fermé et monolithique.

L'objectif n'est pas seulement de stocker des pixels, mais l'état de travail d'une illustration. Les calques restent des calques, les masques restent des masques, et le fichier continue de refléter la façon dont l'illustration a été construite.

## Conçu pour les grandes illustrations

Les grandes images en calques deviennent vite lourdes. Le format de Lumi prend en charge des flux de travail où toutes les données d'image n'ont pas besoin d'être chargées en mémoire d'un coup. Les projets restent réactifs en chargeant uniquement les parties nécessaires à l'affichage, à l'édition, à la composition ou à l'export.

Cette approche rend les fichiers complexes plus gérables, surtout lorsqu'une illustration contient de nombreux calques cachés, archivés, expérimentaux ou groupés.

## Enregistrer sans interrompre le flux

Le format prend en charge l'enregistrement normal du projet ainsi que des instantanés légers de type récupération. Les artistes peuvent ainsi protéger fréquemment leur travail sans transformer chaque point de contrôle en copie complète de l'image entière.

Comme les informations de récupération font partie de la structure du projet, Lumi peut conserver un historique utile à proximité de l'illustration tout en laissant les sauvegardes automatiques vivre séparément du fichier de travail.

## Échange et export

Le format natif sert au travail courant dans Lumi, tandis que les formats d'export servent à partager des résultats aplatis ou orientés compatibilité. L'import permet d'intégrer des illustrations existantes dans l'environnement en calques de Lumi, et l'export permet aux pièces finies de quitter le format projet lorsqu'elles sont prêtes pour la publication, la livraison ou un traitement ultérieur.

Le fichier de travail reste riche et modifiable, tandis que les images finales peuvent être produites dans des formats externes courants.

## Fiabilité à long terme

En bref, le format `.lum` est un conteneur pratique pour un travail de peinture sérieux : assez ouvert pour être inspecté, assez structuré pour être récupéré, et assez souple pour gérer efficacement des images complexes en calques.
