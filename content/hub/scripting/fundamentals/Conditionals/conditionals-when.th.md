---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 61f1a78c3b37d9a33d3dff25f889287b32fc932bea8c22b4c06100052944b6a6
---
En Scheme, `if` est polyvalent, mais sans `else` explicite il devient vite confus — surtout quand seule la branche vraie doit s'exécuter. Dans ce cas, `when` est plus clair et concis.

La forme de base de `when` :

```scheme
(when test-is-true
  do-this
  do-that)
```

- Si `#t`, toutes les expressions du corps s'exécutent en séquence.
- Si `#f`, rien ne se passe ; aucune valeur n'est renvoyée.

### Exemple

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Comparer `if` et `when`

Les deux ensemble dans le même exemple :

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Explication

1. **`if` :** `(= 0 1)` est faux, donc branche `else`.
2. **`when` dans le `else` :** `(< 0 1)` est vrai ; les deux `lumi-message` s'exécutent.

#### Pourquoi `when` ?

- Pas de `else` vide ou factice.
- Montre que seule la branche vraie compte.

### Résumé

- **`if` :** quand les deux branches comptent.
- **`when` :** branche vraie seule, plusieurs actions possibles.
- Les combiner structure clairement des conditions complexes.