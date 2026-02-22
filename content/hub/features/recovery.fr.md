---
title: "Récupération de fichiers"
type: docs
---
Lumi maintient deux systèmes de récupération indépendants (sauvegardes automatiques en arrière-plan et points de contrôle incrémentiels manuels), tous deux accessibles à partir d'une seule boîte de dialogue.

## Accès

**Fichier** → **Récupérer l'image**

La boîte de dialogue s'ouvre pré-remplie avec les états de récupération pour le fichier actuellement ouvert. Utilisez le sélecteur de fichier en haut pour passer à un autre fichier `.lum`.

---

## Sauvegarde automatique

Lumi enregistre un instantané en arrière-plan de votre travail à intervalles réguliers pendant l'édition. Les sauvegardes automatiques sont écrites dans un **répertoire de cache séparé**, laissant le fichier de travail `.lum` intact :

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

L'encodage du chemin utilise `~` comme séparateur pour créer un répertoire de cache unique par fichier. Cela signifie que les sauvegardes automatiques sont disponibles même si le fichier de projet lui-même est perdu ou corrompu.

- **Fréquence** : configurable dans **Modifier** → **Préférences** → **Performances** → Intervalle d'enregistrement automatique.
- **Emplacement de stockage** : également défini dans Préférences → Performances.
- **Objectif** : Récupération après incident. L'onglet Enregistrement automatique de la boîte de dialogue Récupérer l'image affiche les états d'enregistrement automatique disponibles avec des horodatages.

Lorsque vous ouvrez un fichier contenant des données de sauvegarde automatique plus récentes, Lumi vous en informe au moment de l'ouverture.

---

## Sauvegardes incrémentielles

La sauvegarde incrémentielle est un système de points de contrôle manuel stocké **dans le fichier de projet** sous `recovery/`. La structure est :

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Une nouvelle ligne de base `primary-NN.lum/` est écrite après **Fichier → Enregistrer**. Les pressions suivantes sur Ctrl+S créent des sous-répertoires `delta-NNNN.lum/` contenant uniquement les tampons qui ont changé depuis la dernière ligne de base. Les deltas de sauvegarde automatique et les deltas de sauvegarde manuelle utilisent des compteurs distincts afin de ne pas interférer avec l'historique de chacun.

Les sauvegardes incrémentielles sont **désactivées par défaut** et doivent être activées par projet :

1. **Fichier** → **Enregistrer sous** (Maj+Ctrl+S).
2. Dans la boîte de dialogue Enregistrer sous, cochez **Enregistrement incrémentiel** et définissez éventuellement une limite de **Enregistrements maximum**.
3. Le paramètre est stocké avec le projet et s'applique à toutes les pressions ultérieures sur Ctrl+S.

Lorsque vous ouvrez un fichier `.lum` contenant des sauvegardes incrémentielles plus récentes que la sauvegarde principale, Lumi affiche une invite **Enregistrement incrémentiel détecté** proposant de charger le point de contrôle le plus récent.

---

## Boîte de dialogue Récupérer l'image

La boîte de dialogue comporte trois onglets et deux boutons d'action.

### Onglet Sauvegarde automatique

Répertorie tous les états d'enregistrement automatique disponibles pour le fichier sélectionné avec des horodatages et des vignettes (le cas échéant). Sélectionnez un état et cliquez sur **Récupérer** pour l'ouvrir.

Utilisez cet onglet pour :
- Récupérer après un crash.
- Revenir à un état antérieur de la même session.

### Onglet incrémentiel

Répertorie tous les états de point de contrôle stockés dans le fichier projet. Chaque entrée affiche l'horodatage du point de contrôle. Sélectionnez un point de contrôle et cliquez sur **Récupérer** pour l'ouvrir.

Utilisez cet onglet pour :
- Revenir à un point antérieur d'une session sans avoir enregistré des fichiers séparés.
- Parcourez l'historique des versions d'un projet.

### Dernier onglet

L'onglet par défaut lorsque la boîte de dialogue s'ouvre. Identifie automatiquement l'état de récupération disponible le plus récent à travers les sauvegardes automatiques et les points de contrôle incrémentiels, et affiche son horodatage. Cliquez sur **Récupérer** pour le charger immédiatement sans parcourir les états individuels.

---

## Boutons

| Bouton | Actions |
|--------|--------|
| **Récupérer** | Ouvre l'état de récupération sélectionné en tant que nouvelle image. |
| **Fermer** | Ferme la boîte de dialogue sans récupérer. |
| **Nettoyer les anciens États…** | Ouvre une invite de nettoyage (voir ci-dessous). |

---

## Nettoyer les anciens étatsL’accumulation d’états de récupération au fil du temps peut consommer une quantité importante d’espace disque. Le bouton **Nettoyer les anciens états…** (en bas à gauche de la boîte de dialogue) ouvre une invite de nettoyage pour l'onglet actif (enregistrement automatique ou incrémentiel).

L'invite affiche :
- Combien de sauvegardes complètes existent pour le fichier.
- L'espace disque total qu'ils occupent.
- Un bouton rotatif **Conserver les plus récentes** pour sélectionner le nombre de sauvegardes à conserver.

Le réglage de **Conserver le plus récent** sur `0` supprime tous les états de récupération. Le prochain Ctrl+S après un nettoyage complet écrira une nouvelle sauvegarde principale.

---

## Récupération de démarrage

Au démarrage, si Lumi détecte que le fichier le plus récemment ouvert contient des données de sauvegarde automatique plus récentes que celles de la dernière sauvegarde complète, il présente une invite de récupération avant le chargement. Vous pouvez accepter (charger la sauvegarde automatique) ou rejeter (ouvrir la sauvegarde principale normalement).