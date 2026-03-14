---
title: "Mise en cache des pinceaux"
type: docs
---
La mise en cache des pinceaux est conçue pour que vos pinceaux préférés soient rapides le plus tôt possible. Au lieu de recalculer encore et encore le même tampon de pinceau transformé, Lumi peut conserver un cache enregistré des formes de pinceau que vous utilisez réellement et recharger ce cache automatiquement plus tard.

## Aperçu

Cette fonctionnalité est construite autour de l'idée que de nombreux pinceaux expressifs revisitent toujours les mêmes combinaisons pratiques de taille, d'angle, de dureté et de rapport hauteur/largeur lors de la peinture. Lorsque ces combinaisons sont réutilisées, Lumi peut servir le tampon de pinceau transformé directement à partir du cache au lieu de le reconstruire.

Le résultat est :

- démarrage plus rapide de la course après la sauvegarde d'un cache
- utilisation répétée plus fluide des préréglages favoris
- moins de recalcul inutile lors de longues sessions de peinture
- restauration automatique des caches enregistrés lorsque le préréglage est à nouveau utilisé

## Intention

La mise en cache des pinceaux est destinée aux pinceaux auxquels vous revenez souvent : les préréglages de peinture de base, les outils d'encrage préférés, les pinceaux secs texturés et autres pinceaux dont les tampons transformés sont suffisamment chers pour être remarqués.

L’objectif n’est pas de pré-préparer chaque état théorique du pinceau. L'objectif est de laisser l'utilisation réelle de la peinture remplir d'abord les états les plus précieux, puis de sauvegarder ce cache rempli afin que le pinceau soit déjà chaud la prochaine fois que vous l'utiliserez.

## Comment ça marche

La mise en cache des pinceaux fonctionne avec la quantification des pinceaux.

Lorsque la quantification est activée pour un préréglage de dynamique, les sorties affectant la transformation sont alignées sur des étapes discrètes. Cela donne à Lumi un ensemble fini d’états de pinceaux réutilisables. Pendant que vous peignez :

1. Lumi vérifie si le tampon transformé existe déjà dans le cache.
2. Si tel est le cas, le tampon est immédiatement réutilisé.
3. Si ce n'est pas le cas, Lumi le construit une fois et le stocke.
4. Au fil du temps, le cache se remplit avec les états de pinceau que vous utilisez réellement.

Si vous enregistrez ce cache, Lumi peut le charger automatiquement plus tard afin que le pinceau démarre plus près d'un état réchauffé au lieu de tout reconstruire à partir de zéro.

## Flux de travail typique

1. Choisissez un préréglage de pinceau que vous utilisez fréquemment.
2. Activez la quantification pour sa dynamique.
3. Peignez normalement pendant un moment pour que le cache se remplisse de manière organique.
4. Ouvrez **Tool Preset Editor** et inspectez la section **Preset Cache**.
5. Regardez les métriques en direct :
   - **Taux de réussite**
   - **Couverture**
   - **Mémoire**
6. Cliquez sur **Enregistrer** lorsque le cache semble utile.
7. Lors des sessions ultérieures, Lumi charge automatiquement le cache enregistré lorsque le préréglage devient actif.

Cela rend le préréglage plus rapide, en particulier pour les pinceaux avec des transformations coûteuses ou de gros tampons.

## Où le trouver

### Éditeur de dynamique

Utilisez **l'éditeur de dynamique** pour contrôler la quantification :

- activer la quantification
- choisissez le nombre de pas global
- éventuellement remplacer le nombre de pas par axe de sortie

La quantification est ce qui rend le cache pratique en réduisant la variation continue dans les bacs réutilisables.

### Éditeur de préréglages d'outils

Utilisez **Tool Preset Editor** pour gérer le cache du préréglage actuel :

- **Enregistrer** — conserver le cache en mémoire actuel sur le disque
- **Load** — restaure un cache précédemment enregistré
- **Mémoire libre** — libère le cache en mémoire sans supprimer la copie enregistrée
- **Supprimer** — supprime le cache enregistré du disque

L'extension **Preset Cache** affiche également le taux de réussite en direct, la couverture et l'utilisation de la mémoire.

## Ce qui est mis en cache

La mise en cache des pinceaux cible les tampons de pinceaux transformés : les résultats rastérisés coûteux une fois la taille, l'angle, la dureté, le rapport hauteur/largeur et les entrées de transformation associées résolus.

C'est particulièrement utile lorsque :- le pinceau nécessite un travail de transformation coûteux
- le même préréglage est utilisé sur plusieurs sessions
- le pinceau revisite des états dynamiques similaires à plusieurs reprises
- la réactivité au démarrage rapide est importante

C'est moins utile pour les pinceaux dont l'état de transformation change énormément et se répète rarement.

## Chargement automatique

Les caches enregistrés sont destinés à vous aider dès le début d'une session, pas seulement après avoir déjà peint pendant un certain temps.

Lorsqu'un cache enregistré existe pour le préréglage actif, Lumi peut le charger automatiquement afin que votre pinceau préféré démarre avec de nombreux états utiles déjà disponibles. Cela réduit la période de démarrage à froid et rapproche immédiatement la brosse de sa réactivité maximale.

## Sécurité de la mémoire

La mise en cache des pinceaux est conçue pour améliorer la vitesse sans prendre le contrôle de la machine.

Lumi suit l'utilisation de la mémoire cache, l'expose dans l'interface utilisateur et applique des limites d'exécution sous pression de la mémoire. Si le système manque de RAM disponible, la croissance du cache est automatiquement limitée.

## Meilleurs cas d'utilisation

La mise en cache des pinceaux est particulièrement utile pour :

- pinceaux préférés des conducteurs quotidiens
- des pinceaux texturés utilisés tout au long d'une peinture
- de grands pinceaux expressifs avec un coût de transformation élevé
- préréglages de pinceaux partagés dans des flux de travail d'illustration répétés
- les préréglages que vous voulez sentir "prêts" dès que vous les sélectionnez

## En bref

La mise en cache des pinceaux permet à Lumi d'apprendre les états de pinceau que vous utilisez réellement, de les enregistrer et de les restaurer automatiquement plus tard. Il s'agit d'une fonction de vitesse pratique pour les préréglages favoris : peignez avec le pinceau, laissez le cache se remplir, enregistrez-le et les prochaines sessions démarrent plus rapidement.