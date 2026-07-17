---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a31916ea815a99deebce805ed2023a7bedbf63325938649cebdd80e7eba209ee
---
Dans sa forme la plus simple, `if` en Scheme évalue un test et, selon le résultat, exécute l'un de deux blocs de code :

```scheme
(if test-is-true
  do-this)
```

- Si `#t`, le **consequent** s'exécute (valeur ou effets de bord).

### Exemple

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Test : `(< 0 1)` est vrai.
- `(lumi-message "True!")` s'exécute.

### Branche else : `if-else`

```scheme
(if test
  do-this
  else-do-this)
```

```scheme
(if test
  consequent
  alternative)
```

### Comment ça marche

1. **Tester** d'abord.
2. Si `#t` **consequent**, si `#f` **alternative**.

Les deux blocs peuvent contenir toute expression Scheme valide.

#### Exemple 1 : renvoyer une valeur

```scheme
(if (< 0 1)
  1
  0)
```

Résultat : **1**

#### Exemple 2 : bloc `begin`

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

Résultat : **Affiche « False condition met, calculating... » et renvoie 12.**

#### Exemple 3 : expression `let`

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

Résultat : **Affiche « True condition met, calculating... » et renvoie -10.**

### Résumé

- `if` évalue un test et exécute le bloc adapté.
- Expressions simples ou groupes `begin`/`let`.
- Sans `else` explicite, seul le **consequent** si vrai.