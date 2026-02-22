---
title: "Éditeur de palettes"
type: docs
---
L'éditeur de palette est l'endroit où vous créez et gérez une palette Lumi. Il contient votre ensemble de pigments, stocke les mélanges que vous enregistrez à partir du mélangeur de palette, enregistre les couleurs que vous avez réellement utilisées pendant la peinture et vous permet de configurer la structure des valeurs et les dégradés de la palette.

## Sélection d'une palette

Une palette est plus qu'une collection de pigments : c'est un engagement stylistique. De nombreux artistes travaillent avec un petit ensemble fixe de pigments qu'ils connaissent intimement : la façon dont ils se mélangent, les neutres qu'ils produisent, les changements de température entre eux. Cette familiarité devient une partie de leur voix visuelle. Un peintre peut conserver une palette chaude et à faible chrominance pour le travail des figures et une palette distincte pour les paysages, ou il peut réaliser tout son travail dans un seul ensemble de quatre pigments comme une contrainte délibérée qui unifie un ensemble d'œuvres.

Lumi soutient cette façon de travailler. Chaque palette a ses propres pigments, mélanges, structure de valeurs et dégradés. Changer de palette modifie l'ensemble du système de couleurs : la carte, le mélangeur et les mélanges disponibles sont tous mis à jour pour refléter le nouvel ensemble.

Une liste déroulante en haut de l'éditeur de palette sélectionne la palette active. Lumi est livré avec trois palettes dans le groupe **Standard** :

| Palettes | Caractère |
| :--- | :--- |
| **Par défaut** | Une palette polyvalente à tendance chaleureuse couvrant toute la roue des teintes. Bon point de départ pour la plupart des sujets. |
| **Maître** | Une large palette à spectre complet pour les peintres qui souhaitent une couverture de teinte maximale et un contrôle explicite sur les axes de grisage. |
| **Zorn** | Une palette limitée à quatre pigments basée sur l'approche d'Anders Zorn. Couvre une gamme étonnamment large de tons chair chauds et de neutres à faible saturation à partir d'un ensemble minimal de pigments. |

Les palettes peuvent également être créées, importées ou dupliquées à partir de l'onglet Palettes.

## Palette de pigments

La section **Pigments de palette** en haut de la vue de la palette répertorie vos entrées principales : les pigments de base à partir desquels le reste de la palette est construit. Ce sont les entrées du système de mélange spectral. Les secondaires et les tertiaires sont générés automatiquement à partir d'eux et sont utilisés pour remplir la carte de palette.

## Mixages enregistrés

La section **Mélanges enregistrés** contient les couleurs que vous avez explicitement conservées du mélangeur de palette en utilisant **Ajouter à la palette**. Ce sont vos couleurs dérivées : les résultats du mélange spectral, des ajustements de tonalité et de chrominance enregistrés pour être réutilisés.

Les mixages sauvegardés sont subdivisés en cinq bandes de valeurs :

| Bande | Plage de luminosité par défaut |
| :--- | :--- |
| Clé haute | 80 – 100 % |
| Milieu supérieur | 60 – 80 % |
| Milieu | 40 à 60 % |
| Milieu inférieur | 20 à 40 % |
| Profond | 0 – 20 % |

Lumi place automatiquement chaque mix enregistré dans la bande appropriée en fonction de sa légèreté perceptuelle (CIE L\*). Cela organise vos mélanges par valeur plutôt que de rechercher dans une liste plate et correspond généralement à la façon dont un artiste perçoit la couleur.

Les mixages enregistrés peuvent être renommés via le bouton **Rename Custom** ou le menu contextuel.

## Mélanges utilisés

La section **Mélanges utilisés** est un historique déclenché par la peinture. Chaque fois qu'une couleur de la palette est appliquée à la toile, elle est enregistrée ici. Les mix utilisés sont classés du plus récent au moins récent.

Cette section est utile pour récupérer une couleur avec laquelle vous avez peint mais que vous n'avez pas explicitement enregistrée. Pour conserver un mix utilisé de manière permanente, sélectionnez-le et cliquez sur **Promouvoir** et il se déplace vers les mix enregistrés dans la bande de valeurs appropriée.

Les mix utilisés sont stockés par palette et persistent entre les sessions.

## Bandes de valeurLes bandes de valeur définissent où se situent les limites entre les cinq zones de luminosité. Par défaut, ils répartissent la luminosité uniformément sur la plage de 0 à 100 %, mais vous pouvez les ajuster pour qu'ils correspondent à la structure tonale de votre sujet. Il est utile pour les peintres de définir et de gérer les tranches de valeurs _et_ les écarts entre elles.

### Le curseur de bande de valeur

L'**extenseur de bandes de valeurs** de l'éditeur de palette contient un curseur avec cinq séparateurs déplaçables. Faites glisser n'importe quel séparateur pour déplacer la limite entre les bandes adjacentes. L'étiquette au-dessus du curseur affiche le nom et la plage de pourcentage exacte de la bande active.

**Boutons :**

| Bouton | Effet |
| :--- | :--- |
| **Annuler** | Ramène le curseur au dernier état appliqué |
| **Copie** | Copie la configuration actuelle de la bande dans le presse-papiers |
| **Coller** | Colle une configuration de bande copiée à partir d'une autre palette |
| **Par défaut** | Restaure les paramètres par défaut de division égale d'usine |
| **Postuler** | Valide les modifications et régénère la palette |

**Appliquer** est requis pour rendre les modifications permanentes. Il déclenche une régénération complète de la palette et supprimera tous les mixages enregistrés dont la luminosité ne correspond plus à aucune bande. Lumi affiche une boîte de dialogue de confirmation répertoriant le nombre de mixages qui seront supprimés avant de continuer.

### Bandes de valeurs et carte de palette

La carte de palette affiche la palette sous la forme d'une roue de teintes avec 36 secteurs de teinte (10° chacun) et 15 cellules de luminosité disposées sous forme d'anneaux concentriques. Chaque bande correspond à trois anneaux : les cinq bandes × 3 anneaux = 15 cellules au total.

L'ajustement des bandes de valeurs modifie les valeurs de luminosité qui atterrissent dans chaque niveau d'anneau. Une bande compressée vers l’extrémité sombre fait que ses trois anneaux couvrent une gamme tonale plus étroite ; une large bande donne à ses trois anneaux une plus grande diffusion tonale. C’est ainsi que la même structure Palette Map s’adapte aux palettes adaptées à différentes priorités tonales.

## Dégradés de palette

Chaque palette peut stocker un ou plusieurs **Dégradés** : des progressions douces dérivées des entrées de palette qui peuvent être appliquées au canevas sous forme de remplissages dégradés ou utilisées comme bandes de référence.

Les dégradés sont gérés dans l'**expandeur de dégradés**. La liste déroulante en haut répertorie les dégradés de la palette actuelle. **Ajouter** crée un nouveau dégradé. **Supprimer** supprime celui sélectionné. **Renommer** le renomme.

### Éditeur de dégradé

L'**expandeur de l'éditeur de dégradé** configure le dégradé sélectionné. Chaque dégradé comporte trois points finaux (**A**, **B** et **C**) affichés sous forme d'échantillons de couleur. Cliquez sur un échantillon pour en faire le point de terminaison actif pour la modification.

Chaque point de terminaison peut être défini en cliquant sur **Choisir**, puis en cliquant sur une entrée de palette dans la carte de palette ou dans la vue de palette. Le point de terminaison est lié à cette entrée de palette par UID ; si l'entrée change, le dégradé est mis à jour.

**Contrôles par point de terminaison :**

| Contrôle | Effet |
| :--- | :--- |
| **Force** | Dans quelle mesure la couleur du point final contribue-t-elle à celle de ses voisins |
| **Opacité** | Alpha de la couleur du point final dans le dégradé |
| **Courbe** | Ajustement gamma pour l'atténuation des couleurs à partir de ce point final |

**Curseurs de distribution** (S1, S2, S3) définissent l'endroit où les trois points médians entre les points finaux se situent le long de la bande de dégradé. Les réinitialiser ramène les points médians à un espacement égal.

La bande d'aperçu du dégradé en haut du bloc Éditeur de dégradé affiche le résultat des paramètres de point final et de distribution actuels.

## Palette ancrableLa **Palette** ancrable (**Panneaux > Palette**) est un panneau de lecture plus simple permettant de parcourir et de sélectionner les couleurs de n'importe quelle palette. Il affiche la même vue en trois sections (Pigments de palette, Mélanges enregistrés, Mélanges utilisés) sans les extensions Bandes de valeur et Dégradés.

Une liste déroulante de sélection de palette en haut vous permet de basculer entre toutes les palettes disponibles. Cliquez sur n’importe quelle entrée pour la définir comme couleur de premier plan. Double-cliquez pour ouvrir l'éditeur de nom de couleur. Pour les palettes inscriptibles, les actions Modifier la couleur, Nouvelle couleur à partir de FG et Supprimer la couleur sont disponibles dans la barre de boutons.

La palette ancrable est destinée à un accès rapide aux couleurs pendant la peinture lorsque l'éditeur de palette complet prendrait trop de place.

## Onglet Palettes

L'**onglet Palettes** (disponible sous forme d'onglet ancrable) affiche la palette active en mode compact. Il exclut les pigments pour se concentrer sur les mélanges enregistrés