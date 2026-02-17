---
title: "Structures de données"
type: docs
weight: 3
---
Dans Scheme, les **structures de données** sont des outils essentiels pour organiser, stocker et manipuler les données. Ils permettent aux développeurs de créer des scripts efficaces, lisibles et réutilisables. En choisissant la bonne structure de données pour un problème spécifique, vous pouvez optimiser à la fois les performances et la clarté de votre code.

## Structures de données clés dans le schéma

Scheme fournit plusieurs structures de données puissantes et polyvalentes, chacune adaptée à des tâches spécifiques. Les structures de données principales comprennent :

### Listes
Les listes sont des collections ordonnées d'éléments qui peuvent s'agrandir ou se réduire de manière dynamique. Ils sont idéaux pour les données séquentielles ou hiérarchiques et sont largement utilisés en programmation fonctionnelle.

Principales caractéristiques :
- Dimensionné dynamiquement.
- Les éléments peuvent être de types mixtes.
- Couramment utilisé pour les algorithmes récursifs et pour représenter des structures arborescentes.

Exemples d'utilisation :
- Gestion des collections d'articles.
- Représenter des séquences ou des hiérarchies.

---

### Vecteurs
Les vecteurs sont des collections d'éléments de taille fixe, indexées pour un accès rapide. Ils conviennent mieux aux scénarios dans lesquels les performances et l’accès positionnel sont essentiels.

Principales caractéristiques :
- Taille fixe à la création.
- Les éléments sont accessibles par leur index.
- Plus rapide que les listes pour certaines opérations comme l'accès aléatoire.

Exemples d'utilisation :
- Stockage de configurations ou de données de taille fixe.
- Recherches et mises à jour rapides en fonction de la position.

---

### Choisir la bonne structure de données

La décision d'utiliser une **list** ou un **vecteur** dépend des besoins spécifiques de votre script. Voici quelques lignes directrices :

| Fonctionnalité | Listes | Vecteurs |
|------------------------------|------------------------------|--------------------------------|
| **Flexibilité de taille** | Dynamique | Fixe |
| **Vitesse d'accès** | Plus lent (accès séquentiel) | Plus rapide (accès indexé) |
| **Facilité de modification**| Plus facile | Plus difficile (nécessite une réaffectation) |
| **Cas d'utilisation** | Données dynamiques, récursivité | Données statiques, recherches rapides |

---