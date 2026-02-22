---
title: "Calques et édition non destructive"
type: docs
---
Le système de calques de Lumi permet des flux de travail complexes et non destructifs avec un contrôle total sur le mélange, le masquage et la composition.

## Aperçu

Les calques constituent la base d’une illustration structurée. Chaque calque est indépendant, avec son propre mode de fusion, son opacité et son masque de calque facultatif. Les groupes peuvent imbriquer des calques de manière hiérarchique avec leurs propres propriétés de fusion et de découpage.

## Accès

**Panneaux** → **Couches**, ou le panneau **Couches** par défaut sur la droite.

## Types de calques

### Couches de peinture

Calques raster standard pour le contenu peint. Stockez les données de pixels sous forme de tampons GEGL avec transparence alpha en option.

### Couches de groupe

Conteneurs hiérarchiques pour organiser les couches associées. Les groupes peuvent avoir leur propre mode de fusion, leur propre opacité et leurs propres masques d'écrêtage. Les projections de groupe sont composées à la demande.

### Masques de calque

Masques en niveaux de gris attachés à n’importe quel calque, contrôlant l’opacité par pixel. Peindre sur un masque avec du blanc rend les pixels opaques ; le noir les rend transparents ; le gris fournit une opacité partielle.

## Modes de fusion

Chaque calque a un mode de fusion déterminant comment il se combine avec les calques ci-dessous :

- **Normal** : fusion directe de l'opacité.
- **Multiplier** : Assombrir en multipliant les valeurs de couleur.
- **Écran** : éclaircissez en inversant, en multipliant et en inversant à nouveau.
- **Superposition** : combinaison de multiplication et d'écran.
- **Ajouter** : Mélange additif (somme les valeurs de couleur).
- **Soustraire** : Mélange soustractif.
- **Couleur, Teinte, Saturation, Légèreté** : mélange de composants HSL.

## Découpage et masquage

- **Mode composite : Clip sur toile de fond** : la définition du mode composite d'un calque sur **Clip sur toile de fond** restreint la composition aux zones où les calques **Union** accumulés ci-dessous ont établi leur opacité. Le calque peint uniquement là où ces calques ont du contenu et ne peut pas étendre l'empreinte alpha. Ceci est défini par couche dans la boîte de dialogue Attributs de couche (menu déroulant **Mode composite**). Lorsque le mode composite effectif d'un calque est autre que Union, l'icône en forme d'œil dans le panneau Calques est remplacée par une icône composite pour indiquer le comportement de composition non standard.

  **Exemple : forme alpha partagée :** Dans un groupe, le calque inférieur contient un cercle rempli sur un fond transparent, défini sur le mode composite **Union** par défaut. Chaque calque situé au-dessus dans le même groupe est défini sur **Clip to Toile de fond**. Ces calques ne peuvent peindre que là où le cercle assure l'opacité (une forme, plusieurs calques). Il s'agit d'un motif courant pour colorer, ombrer et détailler une silhouette définie sans se soucier des déversements.
- **Masques de calque** : appliquez un masque en niveaux de gris pour contrôler la visibilité du calque pixel par pixel. La peinture blanche sur le masque révèle ; le noir cache; le gris fournit une opacité partielle.
- **Masques purs enfants** : les masques sont stockés en tant qu'enfants dans la pile dessinable, évitant ainsi la perte de données lors des transformations.

## Sélection des calques (touche Alt)

En appuyant sur **Alt** (Alt gauche) tout en survolant le canevas, vous sélectionnez le calque avec les pixels visibles sous le curseur, sans changer d'outil ni cliquer.

### Comment ça marche

- **Appuyez sur Alt** : Le curseur se transforme en réticule, indiquant que le mode de sélection est actif.
- **Release Alt** : Lumi sélectionne le calque non transparent le plus haut à la position du curseur (opacité > 25 %) et le sélectionne. Le calque est mis en surbrillance dans le panneau Calques et la barre d'état affiche **"Couche sélectionnée : 'nom du calque'"**.
- Une poignée est dessinée au centre du calque sélectionné sur la toile. La poignée diminue et s'efface à mesure que le curseur s'éloigne.

### Parcourir les couchesChaque pression Alt suivante au même endroit sélectionne le **calque suivant** dans la pile à ce stade. Lumi se souvient du dernier calque sélectionné et passe à celui du dessous. Une fois que le bas de la pile est atteint, le prochain tapotement revient à la couche la plus élevée à cette position. Cela permet d'accéder facilement aux calques imbriqués dans des scènes complexes en appuyant plusieurs fois sur Alt.

### Règles d'annulation

La sélection est annulée (ne se déclenche pas lorsque Alt est relâché) si l'un des événements suivants se produit pendant que Alt est maintenu :

- Un bouton de la souris est enfoncé (clic gauche ou droit).
- Une autre touche est enfoncée.

Cela garantit que les gestes de glisser Alt (tels que l'ajustement de la taille du pinceau) et les raccourcis modifiés par Alt fonctionnent sans modifier accidentellement le calque actif.

### Limites

- La sélection de calques ne s'active pas pendant les opérations de l'outil **Transformation** ; Alt a ici une signification différente.
- Le picking n'a pas lieu si une sélection flottante est présente.
- Seul l'Alt gauche déclenche le picking ; Right Alt est traité comme un modificateur standard.

## Opérations

Dans le panneau Calques :

- **Créer un calque** : cliquez avec le bouton droit → **Nouveau calque** ou utilisez le menu **Couche**.
- **Dupliquer** : cliquez avec le bouton droit → **Dupliquer** ou **Couche** → **Dupliquer**.
- **Supprimer** : cliquez avec le bouton droit → **Supprimer**, ou sélectionnez et appuyez sur **Supprimer**.
- **Réorganiser** : faites glisser les calques vers le haut ou vers le bas pour modifier l'ordre d'empilement.
- **Renommer** : double-cliquez sur le nom du calque.
- **Fusionner vers le bas** : cliquez avec le bouton droit → **Fusionner vers le bas** pour combiner avec le calque ci-dessous.
- **Aplatir l'image** : **Image** → **Aplatir l'image** pour fusionner tous les calques visibles.

## Propriétés du calque

- **Opacité** : 0 à 100 %, contrôle la transparence globale du calque.
- **Mode de fusion** : menu déroulant pour sélectionner la façon dont le calque se combine avec les calques ci-dessous.
- **Visible/Caché** : l'icône en forme d'œil bascule la visibilité du calque.

## Verrous de calque

Les icônes de verrouillage sont affichées dans la ligne d'en-tête du panneau Calques. Chaque verrou peut être basculé indépendamment. Un clic droit sur une icône de verrouillage la définit exclusivement (verrouille uniquement ce type, déverrouillant tous les autres sur le même calque).

- **Lock Alpha** : empêche la peinture sur les zones transparentes. Les coups de pinceau n'affectent que les pixels qui ont déjà une opacité ; les pixels entièrement transparents ne sont pas modifiés. Utile pour peindre dans des formes existantes sans renverser à l'extérieur.

- **Verrouiller le masque** : empêche la modification du masque de calque. Le masque reste visible et actif mais ne peut pas être peint ou modifié tant que ce verrou est activé.

- **Verrouiller la couleur** : Verrouille la peinture sur une couleur spécifique : la couleur de premier plan actuelle au moment où le verrouillage est appliqué. Les traits suivants sur ce calque utilisent cette couleur stockée quelle que soit la couleur de premier plan active. Le déverrouillage supprime la couleur stockée.

- **Verrouiller le contenu** (Verrouiller les pixels) : empêche toutes les modifications de pixels du calque. Le calque ne peut pas être peint, rempli, transformé ou autrement modifié. Utile pour protéger les couches finies.

- **Verrouiller la position** : empêche le déplacement ou la transformation du calque. Le calque peut toujours être modifié ; seuls les changements de position (outil Déplacer, outil Transformation) sont bloqués.

- **Verrouiller la visibilité** : empêche l'icône en forme d'œil de modifier la visibilité du calque. Protège les calques qui doivent toujours rester visibles (ou masqués) pendant l'édition.

Tous les verrous sont enregistrés avec le projet et persistent au fil des sessions.

## Effets de calque (effets)

Les filtres GEGL non destructifs appliqués via le menu **Filtres** sont stockés sous forme d'effets validés sur le calque plutôt que de modifier immédiatement les pixels. Lorsqu'un calque a au moins un effet validé, une icône **fx** apparaît dans le panneau Calques à côté de ce calque.### Accéder à la fenêtre contextuelle des effets

Cliquez sur l'icône **fx** sur une ligne de calque dans le panneau Calques pour ouvrir le popover **Effets de calque** pour ce calque.

La fenêtre contextuelle affiche la pile de filtres pour la couche, chaque effet validé étant répertorié par nom avec une bascule de visibilité à côté.

### Contrôles

- **Bascule de l'œil de visibilité** (en haut de la fenêtre contextuelle) : active ou désactive tous les effets simultanément.
- **Bascule de visibilité par filtre** : chaque ligne de filtre possède sa propre icône en forme d'œil pour activer ou désactiver cet effet indépendamment.
- **Modifier** : Ouvre la boîte de dialogue des paramètres du filtre sélectionné, permettant d'ajuster ses paramètres de manière non destructive.
- **Augmenter / Diminuer** : Déplace le filtre sélectionné vers le haut ou vers le bas dans la pile, en modifiant l'ordre dans lequel les effets sont appliqués.
- **Fusionner** : valide tous les effets actuellement visibles sur les pixels du calque, rendant les modifications permanentes. L'icône FX est supprimée si tous les effets sont fusionnés. La fusion n'est pas disponible sur les calques de groupe.
- **Supprimer** : Supprime entièrement le filtre sélectionné. Le popover se ferme automatiquement s'il ne reste aucun effet.

Un double-clic sur un filtre dans la liste ouvre également sa boîte de dialogue d'édition.

**Modifier** et **Supprimer** sont bloqués si Verrouiller les pixels est actif sur le calque. Les filtres ne peuvent pas être réorganisés lorsqu’un filtre est en cours de modification.

### Ajout d'effets

Appliquez un filtre depuis **Filtres** → (n'importe quelle catégorie). Si le calque actif est ciblé et que l'opération s'exécute de manière non destructive, le résultat est stocké sous forme d'effet de calque plutôt que intégré aux données de pixels. L'icône fx apparaît sur le calque lorsqu'au moins un effet est présent.

## Boîte de dialogue Attributs de calque

Double-cliquez sur un calque dans le panneau Calques pour ouvrir la boîte de dialogue Attributs du calque.

### Identité

- **Étiquette de couleur** : étiquette de couleur pour l'organisation visuelle dans le panneau Calques.

### Espace et mode composites

- **Espace composite** : L'espace colorimétrique utilisé lors de la composition de ce calque avec les calques ci-dessous. Options : Auto, Linéaire (RVB), Perceptuel (RVB).
- **Mode composite** : contrôle la façon dont le calque alpha interagit avec la toile de fond. Les options incluent Union (affecte toutes les zones, valeur par défaut pour le mode Normal), Découper sur toile de fond (affecte uniquement les zones avec du contenu existant, valeur par défaut pour la plupart des autres modes de fusion) et Intersection.

### Taille et décalages

Pour un calque existant, **Tailles** affiche les dimensions du calque et les dimensions du masque (si un masque est attaché) sous forme d'étiquettes en lecture seule.

**Décalages de calque** : les doubles flèches X et Y contrôlant la position du calque sur le canevas. Les modifications s’appliquent immédiatement plutôt qu’à la fermeture de la boîte de dialogue.

Si le calque a un masque, les **Décalages du masque** (les doubles flèches X et Y pour la position indépendante du masque) sont indiqués ci-dessous.

Lors de la création d'un nouveau calque, les champs Largeur et Hauteur et une liste déroulante **Remplir avec** (Premier plan, Arrière-plan, Blanc, Transparent) remplacent l'affichage de la taille en lecture seule.

### Attributs de couche (parasites persistants)

La section inférieure de la boîte de dialogue contient un tableau déroulant Nom/Valeur pour les parasites persistants (métadonnées clé-valeur arbitraires attachées à la couche). Ces valeurs sont stockées avec le projet et sont accessibles depuis l'interface de script Scheme.

- Cliquez sur n'importe quelle cellule de la colonne Nom ou Valeur pour la modifier en ligne.
- **Ajouter** : ajoute une nouvelle ligne vide.
- **Supprimer** : Supprime la ligne sélectionnée et son parasite du calque.

Si la couche ne contient pas de parasites persistants, trois lignes de départ vides sont affichées.

### État du contenuUne ligne d'informations en lecture seule en bas affiche l'état actuel du contenu du calque (et du masque, le cas échéant) : **Clair**, **Uniforme** ou **Mixte**. Un préfixe `*` indique que la couche comporte des modifications non enregistrées depuis la dernière sauvegarde.

## Performances

- **Mode rapide** : lors de la peinture sur un seul calque imbriqué dans un groupe, Lumi bascule temporairement les groupes d'ancêtres en rendu direct pendant la durée du trait, ignorant ainsi la recomposition complète de la projection du groupe. Cela élimine le décalage de mise à jour de la projection imbriquée lors de l'encrage et de la peinture. La composition complète reprend à la fin du trait, lorsque le calque actif change ou avant une sauvegarde.

  Le mode rapide est désactivé lorsque l'une des conditions suivantes s'applique à un groupe ancêtre :
  - Le groupe dispose de filtres visibles non destructifs (les filtres nécessitent le tampon de projection).
  - Le mode de fusion du groupe est autre que **Normal** ou **Pass-through**.
  - Le groupe a un enfant direct utilisant le mode composite **Clip to Toile de fond** ou **Intersection** (ceux-ci nécessitent des données de toile de fond du tampon de projection).

  Le mode rapide ne s’active pas non plus pour les calques de niveau supérieur, les sélections flottantes ou lorsque plusieurs calques sont ciblés simultanément.

  La structuration des fichiers pour éviter ces conditions dans les groupes de peinture, en utilisant les modes de fusion normaux sur les calques, garantit que le mode rapide reste actif tout au long d'une session d'encrage ou de peinture.
- **Lazy Loading** : les grands projets se chargent rapidement ; les données de calque sont chargées uniquement lorsque cela est nécessaire (par exemple, lorsqu'elles sont rendues visibles ou peintes).

## Format de fichier

Tous les calques, masques et propriétés sont stockés au format ouvert `.lum` de Lumi. Le fichier est un répertoire contenant des tampons de couches individuels et des métadonnées, garantissant la compatibilité et l'accessibilité à long terme.