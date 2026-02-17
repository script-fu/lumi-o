---
title: "cond"
type: docs
weight: 5
---
Dans Scheme, le conditionnel `cond` est utilisé pour sélectionner l'un des nombreux blocs de code possibles à exécuter, sur la base de plusieurs tests. C'est comme un `if` multi-branches, où chaque branche est vérifiée dans l'ordre jusqu'à ce qu'une correspondance soit trouvée.

### Syntaxe

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Chaque test est évalué dans l'ordre dans lequel il est rédigé.
- Lorsqu'un test est évalué comme vrai (`#t`), son **conséquent** correspondant est exécuté et l'expression `cond` arrête d'évaluer d'autres tests.
- La clause `else` est facultative et sert de solution de secours si aucun des tests n'est évalué comme vrai.

### Comment ça marche

1. **Testez chaque condition** :
   - `cond` évalue les tests dans l'ordre dans lequel ils sont répertoriés.

2. **Exécuter le résultat correspondant** :
   - Lorsque le premier test évalué comme vrai (`#t`) est trouvé, son **conséquent** est exécuté.
   - Si aucun test n'est évalué comme vrai et qu'il existe une clause `else`, le **fallback-consequent** est exécuté.

### Exemples

#### Exemple 1 : Conséquences d'une expression unique

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Le premier test `(< 3 2)` est évalué à faux (`#f`).
- Le deuxième test `(= 3 3)` est évalué comme vrai (`#t`), donc `"This will run"` est renvoyé.
- La clause `else` n'est pas exécutée car une correspondance a déjà été trouvée.

Résultat : **"Cela fonctionnera"**

#### Exemple 2 : actions multiples utilisant `begin`

Lorsqu'un conséquent implique plusieurs actions, utilisez `begin` pour les regrouper :

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

- Le premier test `(< 5 3)` est évalué à faux (`#f`).
- Le deuxième test `(> 5 3)` est évalué à vrai (`#t`) :
  - Il imprime `"Condition met"`.
  - Ensuite, il calcule `(* 5 5)` et renvoie `25`.

Résultat : **Imprime « Condition remplie » et renvoie 25.**

#### Exemple 3 : Utilisation d'un bloc `let` dans un Conséquent

Lorsque vous devez introduire des variables locales, utilisez un bloc `let` :

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- Le premier test `(< 0 -1)` est faux.
- Le deuxième test `(> 0 -1)` est vrai, donc :
  - Un bloc `let` est exécuté, liant `y` à `20`.
  - Il imprime `"Positive condition met"`.
  - Ensuite, il calcule `(+ y y)` et renvoie `40`.

Résultat : **Imprime « Condition positive remplie » et renvoie 40.**

#### Exemple 4 : Repli avec `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Aucun des deux premiers tests n'est évalué comme vrai.
- La clause `else` est exécutée et renvoie `"Fallback value"`.

Résultat : **"Valeur de repli"**

### Résumé

- Utilisez `cond` pour gérer plusieurs conditions de manière claire et concise.
- Les conséquences peuvent être des expressions simples ou des actions groupées utilisant `begin`.
- Utilisez `let` dans les conséquents pour déclarer des variables locales pour les calculs.
- Incluez toujours une clause `else` comme solution de secours pour gérer les cas inattendus.

Cette flexibilité fait de `cond` un outil puissant et lisible pour gérer une logique de branchement complexe.