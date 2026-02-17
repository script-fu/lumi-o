---
title: "quand"
type: docs
weight: 5
---
Dans Scheme, bien que `if` soit élégant et polyvalent, il peut devenir déroutant lorsqu'il est utilisé sans `else` explicite. Cela est particulièrement vrai lorsque l'intention est d'exécuter une seule branche de code uniquement lorsqu'une condition est vraie, sans action alternative pour le cas `false`. Dans de tels scénarios, la construction `when` fournit une alternative plus claire et plus concise.

La forme de base de `when` ressemble à ceci :

```scheme
(when test-is-true
  do-this
  do-that)
```

- Si `test` est évalué à vrai (`#t`), toutes les expressions du corps de la construction `when` sont exécutées séquentiellement.
- Si `test` est évalué à false (`#f`), rien ne se passe et aucune valeur n'est renvoyée.

### Exemple

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Contrastant `if` et `when`

Pour mieux comprendre la différence entre `if` et `when`, considérez l'exemple suivant dans lequel les deux sont utilisés ensemble :

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Explication :

1. **La condition `if`** :
   - Le test `(= 0 1)` vérifie si 0 est égal à 1.
   - Puisque c'est faux (`#f`), la branche `else` du `if` est exécutée.

2. **La construction `when` dans la branche `else`** :
   - Le `when` test `(< 0 1)` vérifie si 0 est inférieur à 1.
   - Puisque cela est vrai (`#t`), toutes les expressions dans le corps du `when` sont exécutées séquentiellement :
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Pourquoi utiliser `when` ici ?

- Utiliser `when` au lieu d'un autre `if` simplifie la logique lorsqu'il n'y a pas besoin d'une branche `else` explicite pour la condition.
- `when` indique clairement que seule la vraie branche est pertinente, réduisant ainsi la confusion potentielle.

### Résumé

- Utilisez `if` lorsque vous avez besoin d'une branche vraie et fausse.
- Utilisez `when` lorsqu'il n'y a qu'une seule branche pour le vrai cas, notamment lorsque plusieurs actions doivent être exécutées.
- La combinaison de `if` et `when` peut aider à structurer des conditions plus complexes de manière claire et concise.