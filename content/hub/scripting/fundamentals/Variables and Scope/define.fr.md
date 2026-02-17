---
title: "définir"
type: docs
weight: 3
---
L'instruction `define` dans Scheme est une construction polyvalente utilisée pour créer des liaisons globales ou locales. Il est le plus souvent utilisé pour définir des variables et des fonctions, les rendant réutilisables et accessibles tout au long d'un script ou dans une portée spécifique. Comprendre `define` est crucial pour écrire des programmes Scheme modulaires, réutilisables et lisibles.

### Objectif de `define`

La construction `define` sert à plusieurs fins :
- **Définition de variables** : attribue des valeurs aux noms de variables, les rendant disponibles pour une utilisation ultérieure.
- **Définition de fonctions** : crée des procédures réutilisables qui encapsulent une logique spécifique.
- **Définitions locales** : lorsqu'il est utilisé dans une fonction, `define` crée des liaisons locales qui n'affectent pas l'espace de noms global.

---

### Définir des variables avec `define`

Une utilisation de base de `define` consiste à créer des variables contenant des valeurs constantes ou calculées.

#### Syntaxe
```scheme
(define variable-name value)
```

#### Exemple : Définition d'une constante
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Résultat** : `6.28318`

---

### Définir des fonctions avec `define`

Vous pouvez utiliser `define` pour créer des procédures réutilisables.

#### Syntaxe
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Exemple : Définition d'une fonction simple
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Résultat** : `16`

---

### Définitions locales avec `define`

Lorsqu'il est utilisé dans une fonction, `define` crée des liaisons locales qui ne sont accessibles que dans la fonction englobante. Cela évite de polluer l'espace de noms global et aide à organiser votre code.

#### Exemple : Fonctions d'assistance locales
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Résultat** : `41` (Calcule \(2^2 + 3^3 + 4^2\))

---

### Principales fonctionnalités de `define`

1. **Portée mondiale ou locale** :
   - Lorsqu'il est utilisé au niveau supérieur, `define` crée des variables ou des fonctions globales.
   - Lorsqu'il est utilisé dans une autre fonction, `define` crée des liaisons locales.

2. **Réutilisabilité** :
   - Les fonctions définies avec `define` peuvent être réutilisées plusieurs fois dans différents contextes.

3. **Lisibilité améliorée** :
   - Diviser la logique en fonctions plus petites et bien nommées améliore la clarté et la maintenabilité de votre code.

---

### Différences entre `define` et `let`

| **Aspect** | **`define`** | **`let`** |
|----------------------------------------|--------------------------------------------------|---------------------------------------------|
| **Objectif** | Crée des liaisons globales ou locales pour des variables ou des fonctions. | Crée des liaisons temporaires dans une portée localisée. |
| **Portée** | Global lorsqu'il est au plus haut niveau ; local à l'intérieur d'une autre fonction. | Toujours local au bloc `let`.       |
| **Réutilisabilité** | Les fonctions et variables peuvent être réutilisées à plusieurs endroits. | Les variables sont liées temporairement à un seul bloc. |
| **Syntaxe** | Définit explicitement des variables ou des fonctions.       | Combine la liaison de variable avec l'évaluation d'expression. |