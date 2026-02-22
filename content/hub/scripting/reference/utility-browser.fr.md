---
title: "Navigateur d'utilitaires"
type: docs
---
Le navigateur d'utilitaires vous permet d'explorer l'utilitaire Scheme intégré stdlib fourni avec Lumi, sans avoir à quitter l'application ni à parcourir les fichiers sources.

## Ouverture du navigateur d'utilitaires

Accédez à **Aide → Programmation → Navigateur d'utilitaires**.

La fenêtre s'ouvre immédiatement ; aucun plug-in ne doit être chargé à l’avance.

## Ce que ça montre

Le navigateur répertorie toutes les procédures, variables et formulaires de syntaxe exportés par les sept bibliothèques d'utilitaires que Lumi charge automatiquement au démarrage :

| Bibliothèque | Ce qu'il couvre |
|---|---|
| `common.scm` | Aides à usage général (utilitaires de chaîne, de nombre, de liste) |
| `files.scm` | Aides aux fichiers et aux chemins |
| `gegl.scm` | Tampon GEGL et aides aux couleurs |
| `images.scm` | Assistants au niveau de l'image (`image-get-open-list`, etc.) |
| `layers.scm` | Aides de calque et dessinables |
| `parasites.scm` | Aides à la lecture/écriture parasite |
| `paths.scm` | Aides de chemin et de vecteur |

Tous ces éléments sont disponibles dans n’importe quel plug-in Scheme ou dans la console Scheme.

## Recherche et filtrage

- **Champ de recherche** : filtre par nom au fur et à mesure que vous tapez (correspondance de sous-chaîne insensible à la casse).
- **Filtre de type** : limitez les résultats à `procedure`, `variable` ou `syntax`.

Cliquer sur une entrée affiche sa docstring complète et la bibliothèque dont elle provient.

## Le Stdlib comme Wrappers

Les bibliothèques d'utilitaires sont une application pratique du modèle d'encapsulation : chaque assistant donne un nom clair à une opération de bas niveau, masque le passe-partout et fournit un emplacement unique pour mettre à jour si la commande sous-jacente change. Si vous souhaitez comprendre l'approche de conception qui les sous-tend, consultez le didacticiel **[Wrapping](@@LUMI_TOKEN_11@@)**.

## Relation avec le navigateur de procédures

Le navigateur d'utilitaires est distinct de **Filtres → Script-Fu → Console → Parcourir** (le navigateur de procédures). Le navigateur de procédures répertorie les procédures enregistrées dans PDB. Le navigateur d'utilitaires répertorie les définitions d'assistance qui vivent intentionnellement *en dehors* du PDB : elles sont uniquement Scheme et n'ont pas de liaison C.