---
title: "Outil Pinceau"
type: docs
---
Le pinceau est le principal outil de peinture, conçu pour un travail au pinceau réactif et intelligent avec un contrôle total sur la dynamique de la pression, de la vitesse, de l'inclinaison et de l'espacement.

## Aperçu

L’outil Pinceau prend en charge les types de pinceaux raster, générés de manière procédurale et animés. Les traits peuvent être stabilisés, lissés et post-traités. La dynamique du pinceau répond à la saisie du stylet, offrant un contrôle précis sur l'opacité, la taille, la couleur, l'angle et d'autres propriétés lors d'un trait.

## Types de pinceaux

### Pinceaux raster (.raster)

Images de pinceau bitmap prenant en charge la transparence alpha.

### Pinceaux générés (.param)

Formes rendues de manière procédurale (cercle, carré, diamant, triangle) avec paramètres réglables : dureté, rapport hauteur/largeur, angle, rondeur et rayon des coins. Les pinceaux générés sont légers et évolutifs.

### Pinceaux animés (.anim)

Séquences d'images séquentielles qui avancent pendant les traits. Les images peuvent être cycliques de manière incrémentale (avances d'image par touche), sélectionnées au hasard par touche ou indexées par dynamique (pression, vitesse, inclinaison, angle).

## Curseur de peinture

Le curseur s'adapte à l'état actuel de l'outil pour fournir un retour clair et contextuel :

- **Contour du pinceau** : le curseur suit la forme et la taille exactes du pinceau, donnant un aperçu en direct de l'endroit où la peinture va atterrir.
- **Mode Effacement** : Lorsque l'effacement est actif, le contour passe à un cercle en pointillés pour distinguer visuellement les traits d'effacement des traits de peinture.
- ** Limite de pinceau simple ** : pour les pinceaux complexes ou très grands pour lesquels le rendu du contour précis est coûteux, activez ** Limite de pinceau simple ** (dans les options supplémentaires) pour utiliser un cercle simple à la place.

## Options des outils

### Contrôles de niveau supérieur

Présent à tout moment, en dehors de tout expandeur :
- **Mode** : Mode de fusion de peinture (Normal, Multiplier, Écran, etc.)
- **Opacité** : Opacité globale du trait (0–100).

### Options de pinceau

Dans l'expandeur **Options de pinceau** (développé par défaut) :
- **Taille** : Diamètre du pinceau en pixels.
- **Ratio** : écrasez ou étirez la forme du pinceau (-1,0–1,0). 0 = non modifié ; les valeurs négatives font pivoter la courge de 90°.
- **Angle** : fait pivoter le tampon du pinceau (-180–180°). Indépendant de la dynamique de la direction de la course.
- **Espacement** : Distance entre les touches peintes en pourcentage de la taille du pinceau. Inférieur = traits plus doux ; plus haut = motif dispersé.
- **Dureté** : fondu doux (0,0) à bord net (1,0).
- **Force** : Force d'application du pinceau (0,0–1,0). Masqué pour l'outil Crayon.
- **Jitter** : décale de manière aléatoire chaque position de touche jusqu'à ce nombre de pixels (0 à 1 024).
- **Gomme** : multiplicateur de taille appliqué lorsque ce pinceau est utilisé comme gomme (0,1 à 10,0). Non affiché sur l'outil Gomme lui-même.

### Effets des traits

Dans l'expandeur **Effets de trait** :
- **Post-traitement** : applique la stabilisation, la compression de la vitesse et la correction de la relecture une fois le trait terminé, améliorant ainsi la cohérence sans latence.
  - **Seuil de virage** : seuil d'angle (0–180°) pour la correction de direction dans les virages serrés. 0 = correction de la direction de saut.
  - **Vitesse d'aperçu** : Supprime l'aperçu post-traitement lorsque la vitesse de trait dépasse cette valeur (0 = toujours aperçu).
- **Build-Up** : lorsque cette option est activée, chaque touche accumule de l'opacité plutôt que d'être composée en un seul trait.#### Calligraphique
Lorsqu'il est actif, le tamponnage est remplacé par un couloir géométrique continu :
- **Largeur** et **Hauteur** : Dimensions du couloir calligraphique.
- **Angle** : orientation de la plume (degrés).
- **Opacité dynamique** : module l'opacité du trait en fonction des changements de vitesse et de direction. Fonctionne mieux sur des traits fins et contrôlés ; les résultats sont moins prévisibles sur des gribouillages rapides. Expérimental.
- **Velocity Growth** (0–100 %) : augmentation maximale autorisée de la taille par échantillon en pourcentage de la taille de l'échantillon précédent. Limite la rapidité avec laquelle une dynamique de taille basée sur la vitesse peut croître, empêchant ainsi les sauts soudains lorsque la course accélère.
- **Velocity Shrink** (0–100 %) : diminution de la taille maximale autorisée par échantillon. Limite la rapidité avec laquelle la taille peut diminuer lorsque le trait décélère.

#### Lissage

Permet le lissage des entrées en temps réel appliqué au tracé du trait pendant que vous peignez. S'agrandit pour révéler :
  - **Profondeur** (2–256) : nombre d'échantillons d'entrée précédents pris en compte lors du calcul de la position lissée. Des valeurs plus élevées produisent un décalage plus long et plus engagé.
  - **Position** (0–100) : Intensité du lissage appliqué à la position du pinceau. Des valeurs plus élevées complètent les changements brusques de direction.
  - **Pression** (0–100) : Lissage appliqué au signal de pression du stylet, réduisant les pics de pression et la gigue.
  - **Direction** (0–100) : Lissage appliqué à la direction du trait, stabilisant la dynamique sensible à l'angle.

#### Dynamique

Attribuez une entrée de stylet ou d'autres valeurs en direct aux paramètres de peinture :

- **Pression** (stylet) : contrôle la taille, l'opacité, le taux, la dureté, la couleur et bien plus encore en fonction de la pression du stylet.
- **Vitesse** : mappe la vitesse de trait aux propriétés du pinceau.
- **Inclinaison** : les angles d'inclinaison X et Y du stylet affectent l'angle et d'autres paramètres.
- **Roue** : saisie avec la molette de la souris ou la molette du stylet.
- **Direction** : Angle de direction du trait.
- **Fade** : Estompez l'opacité ou la taille sur un nombre fixe de touches.

Chaque entrée dynamique peut être mappée à plusieurs propriétés indépendamment. Ouvrez **Options de l'outil** → **Dynamique** pour configurer.

#### Fondu et couleur

Dans l'expandeur **Fade and Colour** (imbriqué dans Stroke Effects ; visible uniquement lorsque le **Dynamics System** est activé) :

- **Angle initial relatif** : la valeur de l'**Angle initial** est interprétée par rapport à la direction du trait plutôt que comme un angle absolu du canevas.
- **Fade Initial Angle** : Fondu depuis **l'angle initial** au début du trait vers l'angle dynamique en direct au cours du trait. L'activation de cette option force l'activation de **Angle initial relatif**.
- **Angle initial** (-180–180°) : L'angle du pinceau au tout début d'un trait, avant que la dynamique ne prenne le dessus.
- **Angle Blend Factor** (0,0–1,0) : contrôle la rapidité avec laquelle l'angle du pinceau passe de l'angle initial à l'angle dynamique. 0 = maintient l'angle initial ; 1 = utilise immédiatement l'angle entièrement dynamique.
- **Stabilisation de la direction** (0 à 100 px) : retarde les dynamiques sensibles à la direction en exigeant que le pointeur parcoure autant de pixels avant de mettre à jour la direction du trait. Actif uniquement lorsque **Post Process** est désactivé (Post Process fournit sa propre stabilisation). 0 = désactivé (direction immédiate, peut sauter au début de la course).
- **Longueur du fondu** : Distance en unités de canevas sur laquelle le fondu se déroule.
- **Répéter** : Comment le fondu est répété une fois la longueur du fondu épuisée (Aucun, Boucle, Dents de scie, Triangle).


### Têtes de brosseLes têtes de brosse placent plusieurs têtes de brosse indépendantes sur un **anneau d'orbite** circulaire centré sur la trajectoire du trait. Chaque tête peint une touche complète à sa propre position à chaque fois que le trait avance, produisant simultanément plusieurs traits parallèles ou en éventail.

Le rayon de l'orbite est déterminé par la taille globale de la brosse moins la taille de la tête : les têtes plus grandes sont plus proches du centre ; des têtes plus petites orbitent plus loin. Dirige l'espace uniformément autour de l'anneau. Avec deux têtes, vous en obtenez une de chaque côté du trait, créant ainsi une répartition symétrique qui se comporte comme une plume de calligraphie. Le curseur **Follows Direction** fait pivoter l'ensemble de l'anneau pour rester perpendiculaire au trait, de sorte que la plume suit naturellement la direction pendant que vous peignez. L'ajout de têtes supplémentaires les ventile progressivement autour de l'anneau, jusqu'à un cercle de pulvérisation complet à 16.

Les commandes apparaissent dans l'extenseur **Têtes de brosse** du panneau d'options de l'outil.

- **Count** : Nombre de têtes de brosse simultanées (1 à 16).
- **Taille** : taille rendue de chaque tête par rapport à la taille globale du pinceau (0,1 à 1,0).
- **Angle** (0–360°) : orientation statique de l'anneau de formation, utilisée lorsque **Follows Direction** est inférieur à 1,0.
- **Variation de pression** : variation de taille par tête appliquée en tant que biais de pression indépendant à travers les courbes dynamiques.
- **Variation d'opacité** : variation d'opacité par tête, indépendante de la variation de taille.
- **Rigidité** : la rigidité avec laquelle le rayon de l'orbite suit la taille du pinceau à l'échelle dynamique. 0 = l'orbite suit la taille de la dynamique ; 1 = l'orbite reste fixe à la taille de la base.
- **Suit la direction** (0,0–1,0) : la force avec laquelle l'anneau de formation suit la direction de déplacement de la course. À 1,0, l'anneau est toujours perpendiculaire à la direction du déplacement ; à 0,0, il se verrouille sur la valeur statique **Angle**.
- **Character Seed** (0-255) : graine fixe pour le personnage par tête (taille, position de dispersion, portée). La même graine reproduit la même formation à chaque coup. Désensibilisé lorsque **Random Head Character** est activé.

#### Interpolation

Déplace les têtes le long et autour du tracé du trait à chaque touche, créant des effets de frottis et de pulvérisation.

- **Dépassement** (0–5) : disperse la tête vers l'avant dans le sens du déplacement. À 1,0, les têtes s'étalent jusqu'à un intervalle d'espacement complet en avant ; les valeurs supérieures à 1,0 permettent une plus grande portée avec un fort biais de rareté.
- **Undershoot** (0–5) : identique à Overshoot mais en retard sur la touche actuelle. Combiné avec Overshoot, cela crée un frottis principal ou une queue de comète. Supprimé dès le premier coup pour éviter les artefacts rétrogrades.
- **Angle de pulvérisation** (0–90°) : Ventile chaque tête vers l'extérieur par rapport à la direction du mouvement selon un angle aléatoire par tête jusqu'à cette valeur. Serré à 90° pour qu'aucune tête ne soit jamais tournée vers l'arrière. Par défaut : 10°.
- **Spray Seed** (0–255) : graine fixe pour les angles de pulvérisation par tête, indépendamment de Character Seed. Désensibilisé lorsque **Modèle de pulvérisation aléatoire** est activé.

#### Randomisation

- **Caractère de tête aléatoire** : redessine les valeurs de caractère par tête (taille, position de dispersion, portée) à chaque touche afin que la formation soit complètement chaotique le long du trait. Remplace **Character Seed**.
- **Modèle de pulvérisation aléatoire** : redessine les angles de pulvérisation à chaque touche afin que le ventilateur se déplace continuellement le long de la course (« pulvérisation vivante »). Remplace **Spray Seed**.
- **Images d'animation aléatoires** : Pour les pinceaux animés : chaque tête avance indépendamment son image d'animation.

### Options supplémentaires

Dans l'expandeur **Options supplémentaires** (réduit par défaut) :- **Verrouiller sur la vue** : maintient l'apparence du pinceau fixe par rapport à la vue du canevas : lorsque vous faites pivoter le canevas, le pinceau tourne avec lui.
- **Bordure simple du pinceau** : utilise un cercle simple pour le contour du curseur du pinceau au lieu de restituer la forme complète du pinceau. Utile pour les pinceaux complexes ou volumineux où la limite précise est coûteuse à tracer.
- **Jitter uniforme** : lorsque cette option est activée, les décalages du curseur **Jitter** sont tirés d'une distribution uniforme (chaque décalage est également probable dans la plage). Lorsqu'elle est désactivée, la distribution est gaussienne (décalages du cluster vers le centre).
- **Restaurer les dernières couleurs utilisées** : restaure les couleurs de premier plan et d'arrière-plan de la session précédente au démarrage, au lieu de passer par défaut au noir et blanc.
- **Horizontal aléatoire** : 50 % de chances de refléter chaque tampon de gauche à droite par touche.
- **Vertical aléatoire** : 50 % de chances de retourner chaque tampon à l'envers par touche.
- **Rotation aléatoire** : fait pivoter chaque tampon de manière aléatoire de 0°, 90°, 180° ou 270° par touche.
- **Réinitialiser l'animation** : Pour les pinceaux animés : lorsqu'elle est activée, l'animation redémarre à partir de l'image 0 à chaque nouveau trait ; lorsqu'elle est désactivée, elle continue à partir de l'endroit où le trait précédent s'est terminé.