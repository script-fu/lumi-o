---
title: "si"
type: docs
weight: 4
---
Dans sa forme la plus simple, le conditionnel `if` dans Scheme évalue un test et, en fonction du résultat, exécute l'un des deux blocs de code possibles. La forme la plus simple ressemble à ceci :

```scheme
(if test-is-true
  do-this)
```

- Si `test` est évalué à vrai (`#t`), le **bloc de code dans le conséquent** est exécuté. Le bloc peut renvoyer une valeur ou effectuer d'autres actions, telles que l'attribution d'une variable ou l'impression d'une sortie.

### Exemple

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Dans ce cas, le `test` est `(< 0 1)` (en vérifiant si 0 est inférieur à 1).
- Puisque le test est évalué comme vrai (`#t`), le bloc de code `(lumi-message "True!")` est exécuté, qui imprime `"True!"`.

### Ajout d'une condition Else : `if-else`

Lorsque vous utilisez un conditionnel `if` avec un bloc de code alternatif (le cas `else`), la structure ressemble à ceci :

```scheme
(if test
  do-this
  else-do-this)
```

- Si `test` est évalué à vrai (`#t`), le bloc de code **conséquent** est exécuté.
- Si `test` est évalué à false (`#f`), le bloc de code **alternatif** est exécuté.

```scheme
(if test
  consequent
  alternative)
```

### Comment ça marche

1. **Expression de test** :
   - L'expression `test` est évaluée en premier.

2. **Résultat basé sur le test** :
   - Si `test` est évalué à vrai (`#t`), le **bloc de code conséquent** est exécuté.
   - Si `test` est évalué à false (`#f`), le **bloc de code alternatif** est exécuté.

Les blocs de code `consequent` et `alternative` peuvent effectuer n'importe quelle opération Scheme valide, y compris le renvoi de valeurs, la modification de variables ou l'exécution de procédures.

### Exemples

#### Exemple 1 : renvoyer une valeur

```scheme
(if (< 0 1)
  1
  0)
```

- Ici, le `test` est `(< 0 1)` (en vérifiant si 0 est inférieur à 1).
- Puisque le test est évalué à vrai (`#t`), le bloc **conséquent** (`1`) est exécuté et sa valeur est renvoyée.

Résultat : **1**

#### Exemple 2 : évaluation d'un bloc de début

Dans les cas où vous devez effectuer plusieurs actions lorsque la condition est vraie ou fausse, vous pouvez utiliser `begin` ou un `let` pour les regrouper.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Dans cet exemple, le `test` est `(= 0 1)` (en vérifiant si 0 est égal à 1).
- Puisque le test est évalué à faux (`#f`), le bloc **alternatif** est exécuté :
  - Tout d'abord, il imprime `"False condition met, calculating..."`.
  - Ensuite, il calcule `(* 3 4)` et renvoie `12`.

Résultat : **Imprime "Fausse condition remplie, calcul..." et renvoie 12.**

#### Exemple 3 : évaluation d'une instruction let

L'utilisation d'un `let` nous permet de déclarer des variables de portée locale dans le bloc de code.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Dans cet exemple, le `test` est `(= 1 1)` (vérifier si 1 est égal à 1).
- Puisque le test est évalué à vrai (`#t`), le bloc **conséquent** est exécuté :
  - Tout d'abord, il imprime `"True condition met, calculating..."`.
  - Ensuite, il calcule `(* -1 10)` et renvoie `-10`.

Résultat : **Imprime "Condition réelle remplie, calcul..." et renvoie -10.**

### Résumé- Le conditionnel `if` est un outil puissant dans Scheme pour évaluer les tests et exécuter les blocs de code correspondants.
- Il peut gérer à la fois des expressions simples et des blocs de code complexes qui renvoient des valeurs, modifient des variables ou effectuent des effets secondaires.
- N'oubliez pas : s'il n'y a pas de bloc `else` explicite, le `if` n'évalue et n'exécute le **conséquent** que si le test est vrai. Sinon, il évalue et exécute l'**alternative**.