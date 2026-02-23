---
title: "Espaces de travail"
type: docs
---
Un espace de travail est un instantané enregistré de l'ensemble de votre environnement d'interface utilisateur : quels panneaux sont ouverts et où, les décorations et le remplissage du canevas pour les vues normale et plein écran, le thème et le jeu d'icônes actifs, la disposition de la boîte à outils, la palette active et les paramètres de vos outils. Lumi vous permet d'enregistrer autant d'espaces de travail nommés que vous le souhaitez et de basculer instantanément entre eux : toutes les images ouvertes sont mises à jour en place, aucun redémarrage n'est requis.

## Ce qu'un espace de travail permet d'économiser

Chaque espace de travail nommé stocke les éléments suivants indépendamment :

| Composant | Ce qu'il couvre |
| :--- | :--- |
| **Mise en page** | Position et taille de la fenêtre, disposition des quais (colonnes des panneaux gauche et droit, quels panneaux sont ouverts et dans quel ordre), mode simple ou multi-fenêtre, état maximisé, visibilité et position de la barre d'onglets |
| **Options des outils** | Les paramètres actuels pour chaque outil (taille du pinceau, dureté, comportement de déformation, etc.) |
| **Périphériques d'entrée** | Configuration du périphérique d'entrée : courbes de pression, affectations de boutons, mappages d'axes pour stylet et autres appareils |
| **Décorations en toile** | Paramètres par défaut par espace de travail pour les règles, les barres de défilement, les guides, la grille, la surbrillance de la sélection, la limite du calque et la limite du canevas — définis via **Préférences → Fenêtres d'image → Apparence par défaut** et **Apparence plein écran**, indépendamment pour les vues normale et plein écran |
| **Rembourrage en toile** | Mode de remplissage et couleur par espace de travail pour les vues normale et plein écran — définis via **Préférences → Fenêtres d'image → Apparence par défaut** |
| **Thème et icônes** | Thème actif, variante de couleur sombre/claire, jeu d'icônes, remplacement de la taille des icônes et échelle de police |
| **Boîte à outils** | Position du widget FG/BG (haut/bas/gauche/droite), échelle FG/BG, visibilité de la mascotte Wilber, en-têtes de groupe d'outils |

La **palette** et le **préréglage d'outils** actifs sont également enregistrés par espace de travail et restaurés lorsque vous changez.

> **Les décorations et le rembourrage des toiles** sont contrôlés par
> **Préférences → Fenêtres d'images → Options avancées de la fenêtre → Apparence par défaut** (Vue normale)
> et **Apparence plein écran** (affichage plein écran). Ajustez ces paramètres à votre guise,
> puis enregistrez l'espace de travail. Les éléments du **menu Affichage** (règles, guides, etc.) sont locaux au
> fenêtre d'image actuelle et ne sont pas enregistrés par espace de travail.

### Mises à jour en direct sur le commutateur

Lorsque vous changez d'espace de travail, toutes les fenêtres d'images ouvertes sont immédiatement mises à jour : les règles, les guides, les barres de défilement, la couleur de remplissage et tous les autres paramètres d'affichage changent en place sans qu'il soit nécessaire de fermer et de rouvrir les images.

## Accès

**Modifier → Préférences → Espace de travail**

La section supérieure de la page des préférences de l'espace de travail répertorie tous vos espaces de travail enregistrés et fournit des contrôles pour les gérer.

## Création d'un espace de travail

Configurez vos panneaux, outils et palettes exactement comme vous le souhaitez, puis :

1. Ouvrez **Modifier → Préférences → Espace de travail**.
2. Cliquez sur **Enregistrer la mise en page sous…**.
3. Saisissez un nom et cliquez sur **Enregistrer**.

Le nouvel espace de travail apparaît dans la liste déroulante **Mise en page active** et dans le menu **Windows**.

## Changer d'espace de travail

Il existe deux manières de changer :

- **Menu Windows** : les noms de mise en page apparaissent sous **Windows → Mise en page** pour un accès rapide depuis le canevas.
- **Préférences → Espace de travail** : sélectionnez une mise en page dans la liste déroulante **Mise en page active** et cliquez sur **Recharger la mise en page**.

La commutation est immédiate : Lumi reconstruit la disposition des panneaux, restaure les options des outils, recharge les paramètres de l'appareil, met à jour les décorations du canevas, le remplissage, le thème et la disposition de la boîte à outils, le tout sans redémarrer.

## Gestion des espaces de travail

Depuis **Modifier → Préférences → Espace de travail** :| Actions | Effet |
| :--- | :--- |
| **Enregistrer la mise en page** | Remplace l'espace de travail actuel par vos paramètres actuels. |
| **Enregistrer la mise en page sous…** | Crée un nouvel espace de travail nommé à partir de vos paramètres actuels. |
| **Renommer la mise en page…** | Renomme l'espace de travail sélectionné. |
| **Recharger la mise en page** | Applique immédiatement l’espace de travail sélectionné. |
| **Supprimer la mise en page…** | Supprime définitivement l'espace de travail sélectionné et ses fichiers. |

## Paramètres de persistance

La partie inférieure de la page des préférences de Workspace contrôle ce que Lumi enregistre automatiquement :

- **Enregistrer les positions des fenêtres à la sortie** : Lorsque cette option est activée, les positions du dock et des fenêtres sont écrites sur le disque à chaque fois que vous quittez.
- **Ouvrir les fenêtres sur le même moniteur** : Rouvre chaque fenêtre du moniteur sur lequel elle se trouvait lors de la dernière session.
- **Enregistrer les options de l'outil à la sortie** : Enregistre les paramètres actuels de l'outil lors de la fermeture.
- **Enregistrer les paramètres du périphérique d'entrée à la sortie** : enregistre la configuration du stylet et du périphérique lors de la fermeture.

Ces paramètres s'appliquent à chaque espace de travail : chaque mise en page conserve indépendamment son propre état enregistré.

## Exemples de flux de travail

Voici quelques façons dont les artistes peuvent utiliser plusieurs espaces de travail :

- **Peinture** — grands quais de pinceaux, couleur de remplissage chaude (définie dans Préférences → Fenêtres d'images → Apparence par défaut), votre variante de thème préférée
- **Encrage** — guides et limites du canevas activés, barres de défilement activées (définies dans Préférences → Apparence par défaut), couleur de remplissage neutre
- **Roughs** — quais cachés, pas de règles ni de grille, remplissage sombre, taille d'icône compacte pour maximiser l'espace du canevas
- **Mise au point plein écran** — différents paramètres de couleur de remplissage et de décoration dans l'apparence plein écran par rapport à l'apparence par défaut, donc basculer en plein écran donne un environnement de travail véritablement différent
- **Scripting** — panneau de script ouvert, modification de la taille de la police pour plus de lisibilité, jeu d'icônes différent