---
title: "Palette Editor"
type: docs
url: "hub/features/palette-editor"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 21ebf10fe8b43e6b83161bd8734aafa42546b91b13d669356e0395ac4c9c8530
---

Le Palette Editor est l'endroit où une palette Lumi devient un environnement couleur complet. Il rassemble les pigments dont la palette est construite, les mélanges que l'artiste choisit de conserver, les couleurs utilisées pendant la peinture et la structure tonale qui façonne la façon dont la palette est explorée.

Une palette dans Lumi est plus qu'une liste d'échantillons. C'est un système couleur de travail : un ensemble d'ingrédients, de mélanges mémorisés, de relations de valeurs et de dégradés qui peuvent guider toute une illustration ou un corpus d'œuvres.

![palette-editor](/images/screens/palette-editor.jpg)

## Les palettes comme contraintes artistiques

Beaucoup de peintres travaillent mieux avec un ensemble limité de couleurs familières. Un petit jeu de pigments peut créer une unité, établir une ambiance et accélérer les décisions couleur, car chaque mélange appartient à la même famille visuelle.

Le Palette Editor prend en charge ce type de limitation intentionnelle ainsi que des palettes plus larges à spectre complet. Les artistes peuvent conserver des palettes distinctes pour différents sujets, projets, styles ou conditions de lumière, chacune avec son propre caractère et son propre comportement de mélange.


![palette-editor-zorn](/images/screens/palette-editor-zorn.jpg)


## Pigments, mélanges et mémoire

Les pigments de palette constituent la base. Ils définissent les couleurs dont le reste du système se développe et influencent les mélanges générés disponibles ailleurs dans les outils couleur de Lumi.

Les mélanges enregistrés représentent des découvertes délibérées : des couleurs qui méritent d'être conservées, nommées et retrouvées. Les couleurs utilisées forment une mémoire plus discrète, en préservant celles qui sont réellement arrivées sur le canevas même si elles n'avaient pas été enregistrées à l'avance.

Ensemble, ces zones permettent à une palette d'évoluer naturellement. Elle peut commencer comme un jeu de pigments, accumuler des mélanges utiles pendant la peinture, puis devenir progressivement un vocabulaire couleur personnel.

![add-pigment](/images/screens/add-pigment.jpg)

## Pigments utilisateur

Les peintres prémélangent souvent un tas de couleur — un rouge de Mars rabattu avec un neutre, un jaune tempéré, une ombre chaude — et traitent ces tas comme les primaires de travail de la peinture. Les mélanges enregistrés peuvent conserver ces découvertes, mais un mélange n'est pas encore un pigment : il ne génère pas le reste de la palette, et on ne peut pas l'ajouter comme pigment à une autre palette.

Un mélange enregistré peut être promu en pigment utilisateur. Il se comporte alors comme un pigment de palette ordinaire : il génère des mélanges, apparaît sur la carte, et se choisit comme n'importe quel autre pigment, tout en conservant la couleur du mélange promu. Le même pigment peut être ajouté à n'importe quelle autre palette depuis une bibliothèque partagée.

Cette bibliothèque reste liée. Renommer, remplacer ou supprimer un pigment utilisateur atteint chaque palette qui l'utilise, pour qu'une primaire de travail évolue avec le vocabulaire couleur de l'artiste plutôt que de rester coincée dans un fichier.

## Organisation par la valeur

Lumi organise les couleurs de palette en tenant compte de la valeur, car les peintres pensent souvent en clair et en sombre avant de penser en teinte. Regrouper les mélanges par rôle tonal facilite la recherche d'une couleur qui appartient à la bonne partie de l'image, et pas seulement celle qui porte le bon nom.

Les palettes peuvent aussi porter leur propre logique d'espacement tonal. Une illustration en clé haute, un portrait en clé basse et un paysage sourd peuvent chacun bénéficier d'un accent tonal différent, et la palette peut refléter cette structure.

## Dégradés et transitions

Une palette peut inclure des dégradés dérivés de ses propres couleurs. Ils sont utiles pour les transitions douces, les bandes de référence, les études de lumière et les mouvements couleur qui restent dans l'identité de la palette.

Comme ces dégradés appartiennent à la palette, ils renforcent le même langage couleur que les pigments et les mélanges enregistrés, au lieu de ressembler à des ajouts sans lien.

![gradients](/images/screens/gradient-editor.jpg)

## Flux de peinture

Le Palette Editor sert à construire et affiner le système couleur, tandis que des vues palette plus légères permettent un accès rapide pendant la peinture. La conception profonde de palette reste disponible sans être imposée à chaque choix de couleur.

Dans l'ensemble, le Palette Editor transforme la sélection des couleurs en pratique cohérente : choisir les pigments, explorer leurs mélanges, conserver les résultats utiles, et laisser la palette devenir partie de la voix de l'illustration.
