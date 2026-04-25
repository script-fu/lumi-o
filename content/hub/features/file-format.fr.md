---
title: "Format de fichier (.lum)"
type: docs
---
Le format de fichier natif de Lumi est conçu pour les projets de peinture en couches qui doivent rester fiables, inspectables et récupérables au fil du temps. Il est conçu autour des réalités du travail d’illustration : de nombreux calques, de grandes toiles, des informations de couleur intégrées, des masques, des effets et des données de récupération.

Plutôt que de traiter un projet comme une seule goutte opaque, le format maintient la structure de l'œuvre d'art visible pour l'application. Cela permet à Lumi d'enregistrer, de charger et de récupérer des images volumineuses de manière plus intelligente tout en préservant l'organisation dont dépendent les artistes.

## Ouvrir la structure du projet

Un projet Lumi sépare les parties de l'œuvre d'art : la structure de l'image, le contenu des calques, les masques, les données de couleur, les métadonnées et les informations de récupération ont chacun un rôle clair. Cela rend le format plus facile à raisonner et mieux adapté à un accès à long terme qu’un conteneur fermé et monolithique.

Le but n’est pas seulement de stocker des pixels, mais aussi de stocker l’état de fonctionnement d’une illustration. Les calques restent des calques, les masques restent des masques et le fichier continue de refléter la façon dont l'illustration a été construite.

## Conçu pour les grands tableaux

Les grandes images superposées peuvent rapidement devenir lourdes. Le format de Lumi prend en charge les flux de travail dans lesquels il n'est pas nécessaire d'extraire toutes les données d'image en même temps. Les projets peuvent rester réactifs en chargeant les parties de l'image qui sont réellement nécessaires à l'affichage, à l'édition, à la composition ou à l'exportation.

Cette approche permet de gérer les fichiers complexes, en particulier lorsqu'une illustration contient de nombreux calques cachés, archivés, expérimentaux ou regroupés.

## Économiser sans interrompre le flux

Le format de fichier prend en charge à la fois l'enregistrement normal du projet et les instantanés légers de style récupération. Cela donne aux artistes un moyen de protéger fréquemment leurs œuvres sans transformer chaque point de contrôle en une copie complète de l’image entière.

Étant donné que les informations de récupération appartiennent à la structure du projet, Lumi peut conserver un historique utile proche de l'illustration tout en permettant aux sauvegardes de sécurité automatiques de vivre séparément du fichier de travail.

## Échange et exportation

Le format natif est destiné au travail Lumi en cours, tandis que les formats d'exportation sont utilisés pour partager des résultats aplatis ou axés sur la compatibilité. La prise en charge de l'importation permet d'intégrer les illustrations existantes dans l'environnement en couches de Lumi, et la prise en charge de l'exportation permet aux pièces finies de quitter le format du projet lorsqu'elles sont prêtes à être publiées, livrées ou traitées ultérieurement.

Cette distinction permet de conserver le fichier de travail riche et modifiable tout en permettant aux images finales d'être produites dans des formats externes courants.

## Fiabilité à long terme

En bref, le format `.lum` est un conteneur pratique pour les travaux de peinture sérieux : suffisamment ouvert pour inspecter, suffisamment structuré pour récupérer et suffisamment flexible pour gérer des images complexes en couches de manière économique.