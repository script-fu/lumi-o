---
title: "Carte des palettes"
type: docs
---
La Palette Map répond à une question pratique pour les peintres : étant donné un ensemble de pigments, quelles couleurs peuvent réellement en être mélangées ? À partir des pigments entrés dans la palette, il explore chaque combinaison de manière procédurale (mélanges à deux pigments, mélanges à trois voies, variations tonales) et mappe les résultats sur une roue chromatique. Le résultat est une image de l’espace colorimétrique accessible pour cet ensemble spécifique de pigments.

La carte est également un outil de navigation basé sur des coordonnées. Il organise chaque mélange généré par teinte et luminosité dans une grille circulaire, de sorte que la palette entière soit lisible d'un coup d'œil et que chaque couleur ait une adresse d'origine stable.

## Structure de la grille

La Carte est divisée en une grille 36×15 :

- **36 secteurs de teintes** : pas de 10° autour de la roue, centrés sur les noms de teintes majeures.
- **15 cellules de luminosité** : 3 cellules par bande de valeur × 5 bandes (High Key, Upper Mid, Middle, Lower Mid, Deep), allant du blanc à l'extérieur au noir au centre.

Chaque cellule est un petit coin sur la roue. On dit qu'une entrée placée dans une cellule a cette cellule comme **origine** : son adresse logique sur la carte.

## Couleurs dans les cellules

Lorsque plusieurs couleurs sont en compétition pour la même cellule, un seul **gagnant** est affiché bien en évidence :

1. Les participants **Primaires** gagnent toujours leur cellule, quels que soient les autres occupants.
2. Si aucun primaire n'est présent, le mix généré (secondaire ou tertiaire) avec la **chroma la plus élevée** l'emporte.

Les participations qui ne gagnent pas sont finalistes et restent accessibles via le cycle de clics (voir ci-dessous).

Les entrées personnalisées (mixages enregistrés) s'affichent sous forme de points carrés ; les mixages générés et les primaires s'affichent sous forme de points ronds.

## Cliquez sur Cyclisme

Cliquer sur une cellule occupée sélectionne le gagnant comme couleur de premier plan. En cliquant à nouveau sur la même cellule, vous passez à l'occupant suivant (les mélanges générés en deuxième position, puis toutes les entrées personnalisées enregistrées à cette adresse de grille). Chaque clic avance d'un pas dans la pile.

**Cliquez avec le bouton gauche** sur les itinéraires vers le premier plan. Lorsque la cible de couleur est définie sur l'arrière-plan (à partir de la boîte à outils), les clics se dirigent vers l'arrière-plan.

## Shift-Select : Chargement des points de terminaison du mixeur

Maintenez **Shift** pour passer en mode de chargement de point de terminaison :

- Le **clic gauche** attribue l'entrée cliquée comme **Parent A (CCW)** dans le mélangeur de palettes.
- **Cliquez avec le bouton droit** pour l'attribuer en tant que **Parent B (CW)**.

Seules les entrées de classe A (primaires et mélanges personnalisés avec provenance intacte) sont sélectionnables dans ce mode. Les tertiaires sont masqués et les points non-Classe-A sont grisés. Une brève superposition confirme que le mode est actif.

## Points forts des parents du mixeur

Lorsque le mélangeur de palette a des points de terminaison Parent A et Parent B actifs, les deux sont marqués sur la carte avec des **anneaux en diamant** (une forme de losange avec une bordure noire). Ces surbrillance restent visibles même lorsque d'autres éléments d'affichage sont basculés, de sorte que les parents de mélange actifs sont toujours identifiables.

## Origine vs position visuelle

Chaque entrée a deux positions sur la carte :

- **Origin (Source Cell)** : L'adresse de grille logique à laquelle appartient l'entrée, fixe pour sa durée de vie.
- **Position visuelle du point** : où la couleur est réellement rendue en fonction de sa teinte et de sa luminosité perceptuelles.

Avec **Best-Match Relocation**, lorsqu'un mélange est enregistré, le système calcule la recette optimale pour la couleur finale et définit l'origine pour qu'elle corresponde à la position visuelle de la couleur. Cela maintient les couleurs enregistrées proches de leur emplacement visuel sur la roue et rend la carte spatialement cohérente.

## Faire glisser les mix enregistrés

Les entrées personnalisées (mixes enregistrés) peuvent être repositionnées en les faisant glisser :1. Cliquez et maintenez sur une entrée personnalisée (point carré) et faites glisser au-delà du seuil de 5 pixels.
2. Le curseur change pour indiquer le mode glisser. Les mises en évidence des parents sont mises à jour en direct à mesure que vous vous déplacez sur la carte pour afficher les nouveaux parents mixtes à chaque poste candidat.
3. Le point déplacé s'aligne sur la position d'échantillon valide la plus proche.
4. Relâchez pour valider. L'entrée adopte la recette de la cellule de destination : ses parents, son mélange, sa tonalité et sa chrominance sont mis à jour pour correspondre, et son origine est mise à jour pour correspondre à la nouvelle position visuelle.

Les mouvements de glisser sont annulables via **Modifier → Annuler**.

## Double-clic : basculer l'espace de travail de la carte

Dans **Éditeur de palette**, double-cliquez sur n'importe quelle entrée de palette pour activer ou désactiver la vue de l'espace de travail Carte de palette. Il s'agit d'un moyen rapide de basculer entre la navigation dans les couleurs enregistrées et le mixage sur la carte sans utiliser de menu. Le comportement du simple clic (restauration de la recette de l'entrée dans le mixeur) n'est pas affecté.

## Superposition de toile

La carte de palette peut être invoquée directement sur le canevas de l'image en superposition plein écran en cliquant sur l'**échantillon de premier plan/arrière-plan** dans la boîte à outils. Cela donne une grande surface de mixage sans dédier un panneau permanent à la Map.

## Échantillon de couleur centrale

Un échantillon circulaire se trouve au centre du trou du beignet et reflète la couleur de la cellule sur laquelle se trouve le curseur :

- **Couleur de survol** : lorsque le curseur repose sur une entrée de la carte, l'échantillon se met immédiatement à jour pour afficher la couleur de cette entrée.
- **Couleur sélectionnée comme solution de secours** : lorsqu'aucune cellule n'est survolée, l'échantillon affiche le résultat calculé par le mélangeur de palettes pour l'entrée actuellement sélectionnée. Si le mixeur n'a pas encore résolu, il utilise la couleur d'affichage de base de l'entrée afin que le spot ne devienne jamais vide.
- Une fine bordure sombre délimite l'échantillon à tout moment.
- Après que le curseur s'attarde brièvement sur l'échantillon central, un anneau extérieur blanc et noir apparaît pour signaler que la zone est interactive.
- **Cliquer sur l'échantillon central** ferme la superposition du canevas et revient à l'affichage normal de l'image (la même chose que cliquer à l'extérieur de l'anneau extérieur).

## Touche Alt : mode de comparaison de canevas

Lorsque la superposition du canevas Palette Map est ouverte, maintenir **Alt** révèle temporairement l'image en dessous :

- L'ensemble de l'interface utilisateur de la palette devient invisible (son opacité tombe à zéro), révélant ainsi le canevas.
- Un échantillon circulaire de 64 pixels suit le curseur, rempli de la couleur échantillonnée actuelle du Palette Mixer, afin que vous restiez conscient du mélange actif tout en inspectant l'image.
- Relâcher Alt restaure la carte de la palette avec une opacité totale.

Une étiquette d'indice, *"Maintenez la touche Alt enfoncée pour voir l'image"*, s'affiche dans la vue de l'espace de travail à titre de rappel.