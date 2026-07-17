---
title: "Configuration Wacom"
type: docs
url: "hub/quick-start/Wacom-Configuration"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3af66b116d9f361052280ac9636ae4b23bf5fc30f10f7227fb42d2d9e654ea95
---
Pour la peinture numérique dans Lumi, une simple **configuration de pression linéaire** est recommandée.

- Gardez la courbe de pression du pilote de la tablette linéaire.
- Gardez les courbes de pression/entrée dans Lumi principalement linéaires.
- Façonnez la sensation avec le pinceau lui-même, car la dynamique du pinceau peut déjà être non linéaire.

Nous recommandons de conserver la courbe de pression linéaire par défaut au niveau du pilote du système d'exploitation. La combinaison de plusieurs courbes non linéaires conduit souvent à un comportement d'entrée imprévisible ; en gardant le pilote neutre, vous garantissez que tous les ajustements effectués dans Lumi-o restent intuitifs et reproductibles. Un léger ajustement de la courbe globale de Lumi peut toujours être raisonnable en cas de besoin.

## Courbe globale du stylet dans Lumi

Dans Lumi, ouvrez :

Édition → Préférences → Périphériques d'entrée → Configurer la tablette, le stylet et d'autres appareils...

Ici, vous pouvez définir la courbe de pression globale de votre stylet.

## Anneau tactile Wacom

Lumi prend désormais directement en charge la saisie Wacom Touch Ring, y compris les entrées de sonnerie basées sur des modificateurs.

Dans la même boîte de dialogue de configuration de l'appareil, vous pouvez attribuer des actions de sonnerie par entrée, notamment :

- Taille du pinceau
- Taille relative du pinceau
- Angle de brosse
- Angle de vue
- Voir Zoom

Remarque : Une image doit être active pour que le Touch Ring affecte les attributs. L'anneau correspond par défaut à un changement relatif de la taille du pinceau. Pour éviter tout ajustement accidentel, un balayage en demi-cercle est nécessaire pour déclencher une commande (par exemple, un demi-balayage dans le sens des aiguilles d'une montre double la taille du pinceau).