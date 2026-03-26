---
title: "Outil Pinceau"
type: docs
---
Le pinceau est le principal outil de peinture et de dessin, conçu pour un travail au pinceau réactif et intelligent avec un contrôle total sur la dynamique de la pression, de la vitesse, de l'inclinaison et de l'espacement.

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

- **Pinceau** : Sélectionnez le tampon du pinceau ou modifiez celui actif.
- **Mode** : Mode de fusion de peinture (Normal, Multiplier, Écran, etc.)

### Propriétés du pinceau

Dans l'expandeur **Propriétés du pinceau** (développé par défaut) :

#### Forme

- **Taille** : Diamètre du pinceau en pixels.
- **Aspect Ratio** : écrasez ou étirez la forme du pinceau (-1,0–1,0). 0 = non modifié ; les valeurs négatives font pivoter la courge de 90°.
- **Angle** : fait pivoter le tampon du pinceau (-180–180°). Indépendant de la dynamique de la direction de la course.
- **Dureté** : fondu doux (0,0) à bord net (1,0).

#### Candidature

- **Opacité** : Opacité globale du trait (0–100).
- **Espacement** : Distance entre les touches peintes en pourcentage de la taille du pinceau. Inférieur = traits plus doux ; plus haut = motif dispersé.
- **Texture Bias** : biaise la réponse de la texture du tampon ; 50 est neutre. Les valeurs inférieures favorisent la rupture de la texture et une surface écrémée en tirant vers le bas de la courbe de valeur ; des valeurs plus élevées se resserrent vers les remplissages solides en poussant vers l'épaule. L'effet visible dépend de la gamme tonale de la texture.
- **Jitter** : décale de manière aléatoire chaque position de touche jusqu'à ce nombre de pixels (0 à 1 024).
- **Gomme** : multiplicateur de taille appliqué lorsque ce pinceau est utilisé comme gomme (0,1 à 10,0). Non affiché sur l'outil Gomme lui-même.

### Dynamique

Dans l'expandeur **Dynamics** :
- **Activer la dynamique** : activation principale du préréglage de dynamique actif.
- **La pression se multiplie** : laissez la pression dominer la sortie dynamique.
- **Dynamique** : sélectionne les mappages d'entrée utilisés.### Comportement des traits
Dans l'expandeur **Comportement de trait** :
- **Création d'opacité** : lorsque cette option est activée, chaque touche accumule de l'opacité plutôt que d'être composée en un seul trait.
- **Post-traitement** : applique la stabilisation, la compression de la vitesse et la correction de la relecture une fois le trait terminé, améliorant ainsi la cohérence sans latence.
  - **Seuil de virage** : seuil d'angle (0–180°) pour la correction de direction dans les virages serrés. 0 = correction de la direction de saut.
  - **Seuil d'aperçu** : supprime l'aperçu post-traitement lorsque la vitesse de trait dépasse cette valeur (0 = toujours aperçu).

#### Calligraphique

Lorsqu'il est actif, le tamponnage est remplacé par un couloir géométrique continu :
- **Opacité dynamique** : module l'opacité du trait en fonction des changements de vitesse et de direction. Fonctionne mieux sur des traits fins et contrôlés ; les résultats sont moins prévisibles sur des gribouillages rapides. Expérimental.
- **Velocity Growth** (0–100 %) : augmentation maximale autorisée de la taille par échantillon en pourcentage de la taille de l'échantillon précédent. Limite la rapidité avec laquelle une dynamique de taille basée sur la vitesse peut croître, empêchant ainsi les sauts soudains lorsque la course accélère.
- **Velocity Shrink** (0–100 %) : diminution de la taille maximale autorisée par échantillon. Limite la rapidité avec laquelle la taille peut diminuer lorsque le trait décélère.

#### Mouvement uniquement

Lorsqu'il est activé (par défaut), le pinceau peint uniquement lorsque le pointeur se déplace. Éteignez-le pour laisser le pinceau continuer à tamponner tout en restant immobile.
- **Taux** : contrôle la rapidité avec laquelle les horodatages stationnaires sont émis lorsque **Motion Only** est désactivé.
- **Flow** : contrôle l'opacité par touche de ces horodatages lorsque **Motion Only** est désactivé.

#### Stabilisation et lissage

- **Distance de stabilisation de la direction** (0 à 100 px) : déplacement minimum du pointeur avant le début d'un comportement sensible à la direction, permettant d'éviter les sauts d'angle précoces.

#### Lissage

Permet le lissage des entrées en temps réel appliqué au tracé du trait pendant que vous peignez. S'agrandit pour révéler :
  - **Profondeur** (2–256) : nombre d'échantillons d'entrée précédents pris en compte lors du calcul de la position lissée. Des valeurs plus élevées produisent un décalage plus long et plus engagé.
  - **Position** (0–100) : Intensité du lissage appliqué à la position du pinceau. Des valeurs plus élevées complètent les changements brusques de direction.
  - **Pression** (0–100) : Lissage appliqué au signal de pression du stylet, réduisant les pics de pression et la gigue.
  - **Direction** (0–100) : Lissage appliqué à la direction du trait, stabilisant la dynamique sensible à l'angle.

#### Dynamique

Attribuez une entrée de stylet ou d'autres valeurs en direct aux paramètres de peinture :

- **Pressure** (stylus): Controls size, opacity, rate, hardness, color, and more based on stylus pressure.
- **Vitesse** : mappe la vitesse de trait aux propriétés du pinceau.
- **Inclinaison** : les angles d'inclinaison X et Y du stylet affectent l'angle et d'autres paramètres.
- **Roue** : saisie avec la molette de la souris ou la molette du stylet.
- **Direction** : Angle de direction du trait.
- **Fade** : Estompez l'opacité ou la taille sur un nombre fixe de touches.

Chaque entrée dynamique peut être mappée à plusieurs propriétés indépendamment. Ouvrez **Options de l'outil** → **Dynamique** pour configurer.

### Modulation de course

Dans l'expandeur **Stroke Modulation** (affiché uniquement lorsque **Dynamics** est activé) :- **Angle initial relatif** : la valeur de l'**Angle initial** est interprétée par rapport à la direction du trait plutôt que comme un angle absolu du canevas.
- **Fade Initial Angle** : Fondu depuis **l'angle initial** au début du trait vers l'angle dynamique en direct au cours du trait. L'activation de cette option force l'activation de **Angle initial relatif**.
- **Angle initial du pinceau** (-180–180°) : L'angle du pinceau au tout début d'un trait, avant que la dynamique ne prenne le dessus.
- **Initial Angle Blend** (0,0–1,0) : contrôle la rapidité avec laquelle l'angle du pinceau passe de l'angle initial à l'angle dynamique. 0 = maintient l'angle initial ; 1 = utilise immédiatement l'angle entièrement dynamique.
- **Longueur du fondu** : Distance en unités de canevas sur laquelle le fondu se déroule.
- **Répéter** : Comment le fondu est répété une fois la longueur du fondu épuisée (Aucun, Boucle, Dents de scie, Triangle).

#### Multiplicateurs de fondu

Quatre cases à cocher qui contrôlent ou modifient la manière dont la valeur de fondu est appliquée :
- **Fade Reverse** : Inverse la direction du fondu afin que les traits commencent à s'estomper et deviennent complètement opaques.
- **Fade Multiply** : multiplie la sortie de fondu dans la propriété affectée plutôt que de la remplacer.
- **Fade Multiply Angular** : applique le multiplicateur de fondu uniquement à la dynamique angulaire (angle, inclinaison).
- **Fade Multiply Ratio** : applique le multiplicateur de fondu uniquement aux propriétés de type rapport (rapport hauteur/largeur).

#### Cartographie des couleurs

Disponible pour les outils Pinceau et Smudge lorsque Dynamics est actif :
- **Gradient** : La ressource de dégradé utilisée pour le mappage des couleurs le long du trait.
- **Blend Color Space** : Espace colorimétrique dans lequel les couleurs dégradées sont interpolées pendant la peinture.


### Têtes de brosse

Les têtes de brosse placent plusieurs têtes de brosse indépendantes sur un **anneau d'orbite** circulaire centré sur la trajectoire du trait. Chaque tête peint une touche complète à sa propre position à chaque fois que le trait avance, produisant simultanément plusieurs traits parallèles ou en éventail.

Le rayon de l'orbite est déterminé par la taille globale de la brosse moins la taille de la tête : les têtes plus grandes sont plus proches du centre ; des têtes plus petites orbitent plus loin. Dirige l'espace uniformément autour de l'anneau. Avec deux têtes, vous en obtenez une de chaque côté du trait, créant ainsi une répartition symétrique qui se comporte comme une plume de calligraphie. Le curseur **Suivez la direction** fait pivoter l'ensemble de l'anneau pour rester perpendiculaire au trait, de sorte que la plume suit naturellement la direction pendant que vous peignez. L'ajout de têtes supplémentaires les ventile progressivement autour de l'anneau, jusqu'à un cercle de pulvérisation complet à 16.

Les têtes de pinceau contrôlent l'endroit où chaque tête est placée autour du trait. Si **Pivot X** ou **Pivot Y** est éloigné du centre par défaut, chaque tête suit toujours la même formation, mais chaque touche estampée atterrit désormais en utilisant le point de contact interne choisi au lieu du centre géométrique de l'image du pinceau.Les commandes apparaissent dans l'extenseur **Têtes de brosse** du panneau d'options de l'outil.
- **Activer les têtes de brosse** : activation principale du système de têtes de brosse.
- **Count** : Nombre de têtes de brosse simultanées (1 à 16).
- **Taille de la tête** : taille rendue de chaque tête par rapport à la taille globale du pinceau (0,1 à 1,0).
- **Rapport d'aspect de l'orbite** (0,1–1,0) : façonne l'orbite de la formation du cercle à l'ellipse. 1,0 = orbite circulaire ; des valeurs inférieures écrasent l’axe mineur.
- **Angle de formation** (0–360°) : orientation statique de l'anneau de formation, utilisée lorsque **Suivre la direction** est inférieur à 1,0.
- **Suivez la direction** (0,0–1,0) : la force avec laquelle l'anneau de formation suit la direction de déplacement de la course. À 1,0, l'anneau est toujours perpendiculaire à la direction du déplacement ; à 0,0, il se verrouille sur la valeur statique de l'**Angle de formation**.
- **Variation de pression** : variation de taille par tête appliquée en tant que biais de pression indépendant à travers les courbes dynamiques.
- **Variation d'opacité** : variation d'opacité par tête, indépendante de la variation de taille.

#### Dispersion

Principaux contrôles de dispersion dans l'extenseur **Têtes de brosse** :

- **Angle de diffusion** (0 à 360°, 10° par défaut) : fait pivoter uniquement le composant de diffusion aléatoire (pas l'espacement de remplissage). Les angles par tête/par touche sont biaisés vers l'extérieur avec un croisement contrôlé pour éviter les panaches rigides en miroir. Serré à 360°.
- **Distance de diffusion** (0 à 10 000 px) : déplacement aléatoire vers l'avant à partir de la position d'espacement de remplissage de chaque tête. J'ai relancé chaque touche.
- **Scatter Randomness** (0,0–1,0) : variation supplémentaire par touche superposée à la distance et à l'angle de diffusion de base, produisant une pulvérisation plus lâche et plus organique.
- **Influence de la vitesse** (0,0–1,0) : met à l'échelle la dispersion en fonction de la vitesse de course. À 1,0, les coups rapides dispersent les têtes beaucoup plus loin que les coups lents ; à 0,0, la dispersion est constante quelle que soit la vitesse.
- **Scatter Size Balance** (0,0–1,0) : contrôle l'inclinaison de la suppression pour les têtes au-dessus du seuil. À 1,0, toutes les têtes se dispersent de manière égale ; des valeurs plus faibles suppriment de plus en plus les têtes plus grosses tandis que les têtes au niveau/en dessous du seuil restent à la distance de diffusion complète.

### Configuration de l'outil

Dans l'expandeur **Configuration de l'outil** (réduit par défaut), les contrôles sont regroupés en sections de débordement qui sont modifiées moins souvent. Cela permet aux principaux expandeurs de se concentrer sur les commandes de peinture fréquemment ajustées.#### Propriétés du pinceau (débordement)
- **Verrouiller l'angle sur l'espace de l'écran** : verrouille l'angle du pinceau sur l'espace de l'écran, de sorte que l'angle reste au même niveau pendant que la toile tourne/se retourne. Aucun effet lorsque Dynamics contrôle l’angle.
- **Random Flip Horizontal** : 50 % de chances de refléter chaque tampon de gauche à droite par touche.
- **Renversement vertical aléatoire** : 50 % de chances de retourner chaque tampon à l'envers par touche.
- **Rotation aléatoire** : fait pivoter chaque tampon de manière aléatoire de 0°, 90°, 180° ou 270° par touche.
- **Pivot horizontal** (0,0–1,0) : point de contact horizontal dans l'image du pinceau. 0,0 = bord gauche, 0,5 = centre (par défaut), 1,0 = bord droit. Le point choisi est ce qui s'aligne avec la position de la peinture sur la toile, donc l'éloigner du centre décale chaque touche dans cette direction. Les rotations et les retournements restent visuellement ancrés au pivot car le décalage de placement est appliqué après la transformation.
- **Pivot vertical** (0,0–1,0) : point de contact vertical dans l'image du pinceau. 0,0 = bord supérieur, 0,5 = centre (par défaut), 1,0 = bord inférieur. Fonctionne avec **Pivot horizontal** pour définir la partie exacte de la touche qui se trouve sur le tracé du trait.
- **Jitter uniforme** : lorsque cette option est activée, les décalages du curseur **Jitter** sont tirés d'une distribution uniforme (chaque décalage est également probable dans la plage). Lorsqu'elle est désactivée, la distribution est biaisée vers le centre.
- **Réinitialiser l'animation** : Pour les pinceaux animés : lorsqu'elle est activée, l'animation redémarre à partir de l'image 0 à chaque nouveau trait ; lorsqu'elle est désactivée, elle continue à partir de l'endroit où le trait précédent s'est terminé.

Lorsque l’une ou l’autre des valeurs de pivot diffère du centre, l’aperçu du pinceau affiche une superposition de réticule marquant le point de contact actif du tampon.

#### Comportement du trait (débordement)

- **Restaurer les dernières couleurs utilisées** : restaure les couleurs de premier plan et d'arrière-plan de la session précédente au démarrage, au lieu de passer par défaut au noir et blanc.
- **Limite de pinceau simple** : utilise un cercle simple pour le contour du curseur du pinceau au lieu de restituer la forme complète du pinceau. Utile pour les pinceaux complexes ou volumineux où la limite précise est coûteuse à tracer.

#### Dynamique (débordement)

Commandes qui étendent l'expandeur Dynamics principal, regroupées ici car elles sont rarement ajustées :
- **Aperçu de la taille aléatoire** : affiche la variation de taille aléatoire dans l'aperçu du contour du pinceau lorsque le préréglage de dynamique actif entraîne une taille aléatoire.
- **Aperçu de la rotation aléatoire** : affiche la variation de rotation aléatoire dans l'aperçu du contour du pinceau lorsque le préréglage de dynamique actif entraîne l'angle de manière aléatoire.

#### Têtes de brosse (débordement)

Formation :
- **Rigidité des poils** : la rigidité avec laquelle le rayon de l'orbite suit la taille du pinceau à l'échelle dynamique. 0 = l'orbite se dilate et se contracte sous l'effet de la pression ; 1 = l'orbite reste fixe à la taille de la base.
- **Fill Spacing** (0,0–1,0) : répartit les têtes sur l'espace entre les positions de tamponnage consécutives. La valeur de caractère stable de chaque tête détermine sa direction d'inclinaison ; à 1,0, les têtes remplissent tout l'intervalle d'espacement. Le caractère est stable par graine.

Dispersion :
- **Seuil de taille de diffusion** (0,01 à 100 px) : rayon de seuil pour une distance de diffusion complète. Les têtes égales ou inférieures à ce rayon utilisent la distance de diffusion complète ; les têtes plus grosses sont progressivement rapprochées de la course.Randomisation :
- **Character Seed** (0–255) : graine fixe pour le caractère par tête (taille, position d'espacement de remplissage). La même graine reproduit la même formation à chaque coup. Désensibilisé lorsque **Randomize Head Character** est activé.
- **Randomiser le personnage de tête** : redessine les valeurs de caractère par tête (taille, position de dispersion) pour chaque tampon afin que la formation soit complètement chaotique le long du trait. Remplace **Character Seed**.
- **Animation de tuyaux indépendante** : Pour les pinceaux animés : chaque tête avance indépendamment son image d'animation.