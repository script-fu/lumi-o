---
title: "laisser"
type: docs
weight: 4
---
Le nom `let` est utilisé car il reflète ses origines mathématiques d'introduction de liaisons temporaires, comme dans _"Let \( x = 2 \) et \( y = 3 \)"_.

Une instruction `let` dans Scheme est une **construction de liaison** utilisée pour définir des variables dans une portée localisée. Il vous permet de créer des liaisons temporaires pour les variables, puis d'exécuter un bloc de code à l'aide de ces liaisons. Ceci est particulièrement utile pour garder le code modulaire et éviter la pollution variable globale.

Il existe trois formes principales de `let` dans Scheme :

- **`let`** : Lettrage standard pour créer des liaisons locales simples.
- **`let*`** : Let séquentiel, où les liaisons peuvent dépendre des résultats des liaisons précédentes.
- **Named `let`** : Une forme spéciale de `let` qui crée des boucles récursives ou des procédures nommées.

Dans sa forme la plus simple, `let` crée des liaisons de variables locales et évalue une expression avec ces liaisons.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Liaisons** : une liste de paires où chaque paire attribue un `value` à un `variable`.
- **Expression** : Le corps du `let`, qui peut utiliser les variables définies localement.

### Exemple

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Ceci définit deux variables locales, `x` (10) et `y` (20).
- Il calcule ensuite `(+ x y)` en utilisant ces variables.

**Résultat** : `30`

---

## La construction `let*`

La construction `let*` est similaire à `let`, mais les liaisons sont évaluées **séquentiellement**. Cela signifie que les liaisons ultérieures peuvent dépendre des liaisons antérieures.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Exemple

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- La première liaison attribue `10` à `x`.
- La deuxième liaison calcule `y` comme `(+ x 5)`, en utilisant la valeur de `x`.
- Le corps calcule `(* x y)`.

**Résultat** : `150`

---

## Nommé `let`

Un `let` nommé est une forme spéciale de `let` qui fournit un nom pour le bloc `let` lui-même, le transformant en une procédure récursive. Ceci est utile pour créer des boucles ou des calculs récursifs.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nom** : Le bloc `let` reçoit un nom, définissant effectivement une fonction.
- **Liaisons** : valeurs initiales des variables, similaires à un `let` standard.
- **Corps** : L'expression peut appeler le nommé `let` de manière récursive.

### Exemple : boucle avec Named `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- La fonction `loop` commence par `n = 5` et `result = 1`.
- Si `n` est `0`, il renvoie le `result`.
- Sinon, il s'appelle récursivement avec `n - 1` et `result * n`.

**Résultat** : `120` (Factorial de 5)

---

## Tableau récapitulatif| Construire | Descriptif | Cas d'utilisation |
|------------|-----------------------------------------------|-------------------------------------------------------------------------------------------------|
| **`let`** | Définit les liaisons locales pour les variables.    | À utiliser lorsque toutes les liaisons sont indépendantes et ne dépendent pas les unes des autres.     |
| **`let*`** | Définit les liaisons locales séquentielles.       | À utiliser lorsque les liaisons ultérieures dépendent des résultats des liaisons antérieures.           |
| **Nommé `let`** | Définit des procédures locales récursives. | Utilisez des boucles for, des calculs itératifs ou une récursivité dans un contexte local. |

---

## Exemples

### Utilisation de `let` pour le calcul local

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Résultat** : `13` (Calcule `x² + y²`)

---

### Utilisation de `let*` pour les dépendances séquentielles

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Résultat** : `8` (Calcule `x³`)

---

### Utilisation de Named `let` pour le calcul récursif

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Résultat** : `120` (Factorial de 5)

---

En utilisant `let`, `let*` et nommé `let`, Scheme permet une programmation modulaire, récursive et séquentielle avec des règles de portée claires.