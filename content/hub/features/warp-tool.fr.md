---
title: "Outil de déformation"
type: docs
---
L'outil Warp pousse, tire et fait circuler les pixels librement sur le canevas. Dans Lumi, cela va plus loin que la plupart des implémentations : il peut déformer un groupe de calques entier - quel que soit le nombre de calques et de masques imbriqués qu'il contient - en un seul objet unifié, sans aplatir ni perdre aucune structure.

## Aperçu

Sélectionnez un calque et faites glisser dessus pour déplacer les pixels dans n'importe quelle direction. La déformation est non destructive pendant que vous travaillez : vous pouvez annuler et refaire des traits individuels, modifier la taille ou le comportement du pinceau entre les traits et continuer à affiner jusqu'à ce que vous vous engagez. La validation applique la carte de déplacement accumulée de manière destructive aux données de pixels de la couche.

Lorsqu'un **couche de groupe** est sélectionné, l'outil opère sur le groupe dans son ensemble. Vous voyez et interagissez avec un aperçu en direct de l’ensemble du groupe composé. Lors de la validation, la même déformation est appliquée avec précision et indépendamment à chaque calque enfant et masque à l'intérieur du groupe, préservant ainsi la structure complète des calques.

## Déformation de groupe

La déformation d'un groupe est la principale capacité qui distingue l'outil de déformation de Lumi.

### Le problème qu'il résout

Dans la plupart des programmes de peinture, la déformation d'une illustration multicouche nécessite soit d'aplatir d'abord le groupe (détruisant la structure des calques), soit de déformer chaque calque séparément et d'essayer de les faire correspondre à l'œil nu (fastidieux et imprécis). Aucune des deux approches ne préserve la structure originale pour une édition ultérieure non destructive.

Lumi déforme l'ensemble du groupe en un seul élément, puis distribue exactement la même transformation à chaque couche à l'intérieur.

### Comment ça marche

Lorsque vous sélectionnez un groupe et commencez un trait de déformation, Lumi crée un **calque d'aperçu flottant** à partir de la projection composite du groupe. Si le groupe possède un masque, celui-ci est intégré à l'aperçu afin que l'aperçu représente avec précision l'apparence finale. Vous peignez vos traits de déformation directement sur cet aperçu : ce que vous voyez est précisément ce que vous obtenez.

Au commit, Lumi :

1. Applique le déplacement à chaque calque de base à l'intérieur du groupe (y compris les calques profondément imbriqués dans des sous-groupes), en élargissant le canevas de chaque calque juste assez pour capturer la zone de déformation complète.
2. Applique le même déplacement à chaque masque du groupe au cours du même passage.
3. Reprend le calcul automatique des limites du groupe afin que le groupe se redimensionne pour s'adapter à ses enfants nouvellement déformés.
4. Recadre chaque calque déformé selon son contenu réellement peint pour conserver une taille de fichier compacte.
5. Supprime le calque d'aperçu et régénère la projection de groupe à partir des enfants mis à jour.

Tout cela se produit en une seule étape d’annulation. Après la validation, le groupe apparaît exactement comme dans l'aperçu, avec chaque calque et masque intacts.

### Masques

L'option **Masques de déformation** (activée par défaut) permet aux masques de chaque calque et groupe à l'intérieur de la cible de déformation de recevoir la transformation de déplacement identique. Les masques de calque se déplacent avec leurs calques : un masque qui découpait le contour d'un personnage continue de découper ce même contour après la déformation.

Lorsque **Masques de déformation** est désactivé, seul le contenu du calque est déplacé ; les masques conservent leur position d'origine.

## Options des outils

### Comportement

| Mode | Effet |
| :--- | :--- |
| **Déplacer** | Pousse les pixels dans la direction du trait. Le mode principal pour la plupart des travaux de déformation. |
| **Grandir** | Agrandit les pixels vers l'extérieur à partir du centre du pinceau. |
| **Rétrécir** | Tire les pixels vers l’intérieur vers le centre du pinceau. |
| **Tourbillon dans le sens des aiguilles d'une montre** | Fait pivoter les pixels dans le sens des aiguilles d’une montre autour du centre du pinceau. |
| **Tourbillon dans le sens inverse des aiguilles d'une montre** | Fait pivoter les pixels dans le sens inverse des aiguilles d’une montre autour du centre du pinceau. |
| **Effacer** | Supprime le déplacement de déformation, rétablissant les pixels vers leurs positions d'origine. |
| **Lisse** | Diffuse le déplacement, adoucissant les transitions brusques entre les zones déformées et non déformées. |

### Contrôles du pinceau

- **Taille** : Diamètre du pinceau de déformation en pixels. Les pinceaux plus grands déplacent les zones plus larges avec une atténuation plus douce ; des brosses plus petites donnent un contrôle précis et localisé.
- **Dureté** : Atténuation du centre vers le bord. Une dureté élevée produit un déplacement uniforme sur toute la zone de la brosse ; une faible dureté concentre l'effet au centre.
- **Force** : dans quelle mesure les pixels sont déplacés par trait. Une résistance inférieure permet une mise en forme subtile et progressive ; une résistance plus élevée produit un mouvement spectaculaire et rapide.

### Chronométrage des courses

- **Trait pendant le mouvement** (mode Déplacement uniquement) : applique une déformation en continu lorsque la souris se déplace, plutôt que uniquement selon une impulsion de minuterie. À utiliser pour les traits fluides, semblables à un pinceau, où vous souhaitez que le déplacement suive directement le curseur.
- **Trait périodiquement** : applique une déformation à un intervalle de temps fixe pendant que le bouton de la souris est maintenu enfoncé. À utiliser pour les modes Croissance, Rétrécissement et Tourbillon où l'application circulaire continue est l'intention.
- **Taux** : La fréquence d'application périodique de la course.

### Qualité

- **Interpolation** : La méthode d'échantillonnage utilisée lors de la validation. Linear est rapide et fluide pour la plupart des travaux ; Cubic et Nohalo offrent une fidélité plus élevée pour les détails les plus fins.
- **Aperçu de haute qualité** : utilise l'échantillonneur de qualité de validation lors de l'aperçu interactif. Plus lent, mais l'aperçu correspond exactement au résultat validé.

### Options de groupe

- **Développer la zone de déformation** (déformation de groupe uniquement) : le nombre de pixels ajoutés sous forme de marge transparente autour de l'aperçu du groupe sur tous les côtés. Cela donne au contenu déplacé un espace dans lequel s'installer. La valeur par défaut de 256 px est suffisante pour la plupart des travaux ; réduisez-le pour les grandes images où la mémoire est importante, ou augmentez-le pour les traits de déplacement très importants.
- **Masques de déformation** : s'il faut appliquer la même déformation aux masques de calque et de groupe. Activé par défaut.

## Annuler et refaire

Chaque trait est une étape d'annulation discrète au sein de la session de déformation. **Ctrl+Z** supprime le dernier trait et restaure la carte de déplacement à son état antérieur. **Ctrl+Y** (ou **Ctrl+Shift+Z**) le réapplique. Vous pouvez parcourir tout l’historique de l’AVC avant de vous engager.

Appuyer sur **Échap** ou changer d'outil supprime tous les traits non validés et restaure le(s) calque(s) à leur état d'origine. Aucune modification n'est écrite tant que vous ne vous engagez pas explicitement.

## S'engager

Cliquez sur le bouton **Commit** (ou appuyez sur **Entrée**) pour appliquer la distorsion accumulée de manière destructrice. Pour les déformations de groupe, cela déclenche l'application multicouche complète décrite ci-dessus. L'historique d'annulation de la déformation validée est alors une entrée unique dans la pile d'annulation de l'image, réversible avec la commande standard **Modifier → Annuler**.