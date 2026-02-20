---
title: "Navigateur de plug-ins"
type: docs
---
Le navigateur de plug-ins vous permet d'explorer le système de menus et de voir où des plug-ins spécifiques sont installés.

## Ouverture du navigateur de plug-ins

Accédez à **Aide → Programmation → Navigateur de plug-ins**.

## Ce que ça montre

Alors que le navigateur de procédures se concentre sur les *fonctions* brutes du PDB, le navigateur de plug-ins est une vue de sous-ensemble axée sur la découverte de l'interface utilisateur. Il filtre spécifiquement le PDB pour afficher « des éléments qui ressemblent à des plug-ins installés dans le menu ».

En interne, cela utilise une requête qui renvoie uniquement les procédures comportant à la fois un fichier associé sur le disque et un chemin de menu enregistré.

- **Arbre de menu** : affiche une représentation arborescente de la structure du menu Lumi.
- **Emplacements des plug-ins** : vous aide à trouver où un plug-in nouvellement installé s'est imbriqué dans les menus.
- **Métadonnées** : affiche des informations sur l'auteur, la version et la date du plug-in.

## Utilisation

Utilisez le navigateur de plug-ins lorsque vous savez qu'une fonctionnalité existe mais que vous ne la trouvez pas dans les menus, ou lorsque vous concevez votre propre plug-in et souhaitez voir où se trouvent des outils similaires.