---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
---
Lumi est une application rapide et efficace, exclusivement pour Linux, dédiée à la création d'images matricielles. Elle est développée ouvertement : ses choix de conception, sa documentation d'architecture et son historique de développement sont publics, afin que les utilisateurs puissent comprendre comment elle évolue.

Lumi privilégie la peinture et l'illustration numériques, tout en restant capable de retouches structurées et de corrections photographiques. Ajustements correctifs, effets de lentille stylisés, étalonnage des couleurs par plages tonales et calques de filtre non destructifs couvrent un large éventail de flux de retouche.

Lumi repose sur l'idée qu'un logiciel de peinture numérique doit se comporter comme un outil d'atelier fiable : prévisible, transparent et centré sur l'acte de créer des images.

## Objectif

Lumi prend en charge une méthode structurée et non destructive de création d'images, qu'elles soient peintes, dessinées ou photographiques. C'est une alternative ciblée et affirmée aux éditeurs d'images généralistes comme aux logiciels de peinture dédiés : libre et open source, sans abonnement, enfermement propriétaire, dépendance au cloud ni génération d'images par IA.

La fiabilité et l'accès à long terme sont des atouts essentiels. Le format de fichier ouvert, basé sur des répertoires, reste lisible sans logiciel propriétaire, avec import et export XCF et PSD.

## Fondements artistiques

Lumi est développé par un artiste indépendant avec une expérience en pixel art, dessin et peinture traditionnels, développement de jeux, art technique, illustration et animation 3D. Ce parcours façonne son approche de la couleur, du trait, des calques, des performances, de la récupération de données, du scripting et de l'expérience utilisateur.

## Philosophie

Lumi combine un système de couleurs à base de pigments avec un flux de travail réactif, non destructif et fondé sur les calques. Son système chromatique s'écarte délibérément des curseurs HSV conventionnels et des sélecteurs RVB arbitraires.

- **Couleur centrée sur les pigments** : les profils de pigments réels (codes Colour Index) sont mélangés spectralement, de sorte que les palettes se comportent davantage comme de la vraie peinture.
- **Flux de travail piloté par les palettes** : des palettes enregistrées et interchangeables organisent pigments, mélanges, plages de valeurs et dégradés, pour garder les décisions chromatiques cohérentes au sein d'une illustration ou d'un projet.
- **Outils tactiles et ciblés** : les pinceaux intègrent pression, inclinaison et vitesse du stylet pour un contrôle direct et nuancé ; les commandes soutiennent des choix réfléchis sans complexité superflue. Une texture de surface peut donner au trait le grain et l'accroche d'un support, y compris sur un trait calligraphique, et un virage peut décaler ce grain. Les pinceaux à poils se façonnent de près et s'animent le long du trait.
- **Canevas réactif** : OpenGL peut dessiner la pile de calques et peindre les traits de pinceau de chaque préréglage d'outil. Les grands aérographes et les grands tampons de texture suivent la main, et se déplacer sur le canevas reste fluide.
- **Fiabilité non destructive** : calques et filtres modifiables prennent en charge des projets complexes tout en restant prévisibles. Sauvegarde automatique, enregistrement rapide, sauvegardes incrémentielles et récupération protègent les longues sessions de peinture et les grands projets.
- **Espaces de travail dynamiques** : des profils nommés conservent docks, outils, presets, palettes et associations de périphériques, et peuvent être activés de façon atomique à l'exécution.
- **Scripts Scheme** : Lumi prolonge la tradition Script-Fu avec un langage de plug-ins basé sur Scheme et des fonctions utilitaires supplémentaires pour créer des plug-ins et automatiser les flux de travail.

Les [filtres](/hub/features/filters/), y compris le flou d'objectif à bokeh façonné, le tilt shift, l'étalonnage tonal, la netteté et la réduction du bruit, peuvent rester modifiables aux côtés du travail au pinceau.

## Limites

- **Ciblé, pas exhaustif** : Lumi ne vise ni la conception web, ni la publication assistée par ordinateur, ni toutes les niches qu'un éditeur généraliste comme GIMP cherche à couvrir.
- **Linux uniquement** : Lumi est développé sur Debian avec Cinnamon (X11). GNOME sur Wayland a été testé pour l'apparence générale de l'application et pour le rendu OpenGL. Windows et macOS ne sont pas pris en charge.

## Remerciements

Lumi s'appuie sur le GNU Image Manipulation Program (GIMP). Lumi reconnaît avec une profonde gratitude les nombreuses années de travail des développeurs, artistes et contributeurs.

![Lumi logo placeholder](/images/lumi.png)
