---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 32d7e6d0c54bc515f245b0c108d23441754f7248c2510c61a552c693f37d0382
---
En Scheme, le conditionnel `cond` sélectionne l'un de plusieurs blocs à exécuter selon plusieurs tests — comme un `if` à branches multiples, évalué dans l'ordre jusqu'au premier succès.

### Syntaxe

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Tests dans l'ordre d'écriture.
- Premier `#t` : **consequent** exécuté, `cond` s'arrête.
- `else` optionnel en repli.

### Comment ça marche

1. **Tester chaque condition** dans l'ordre.
2. **Exécuter le consequent** correspondant ; sinon `else` si présent.

### Exemples

#### Exemple 1 : conséquents sur une expression

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

Résultat : **"This will run"**

#### Exemple 2 : actions multiples avec `begin`

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

Résultat : **Affiche « Condition met » et renvoie 25.**

#### Exemple 3 : bloc `let` dans un conséquent

```scheme
(cond
  ;; Przypadek 1: gdy 0 jest mniejsze niż -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Przypadek 2: gdy 0 jest większe niż -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Domyślnie: gdy żaden z powyższych warunków nie jest spełniony
  (else
    (let ((z 0))
      z)))
```

Résultat : **Affiche « Positive condition met » et renvoie 40.**

#### Exemple 4 : repli avec `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

Résultat : **"Fallback value"**

### Résumé

- `cond` pour plusieurs conditions clairement.
- Conséquents simples ou groupés avec `begin`.
- `let` pour variables locales ; `else` recommandé en repli.