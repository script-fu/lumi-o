---
title: "Let nommé ou définition locale"
type: docs
weight: 5
---
**named `let`** et **local `define`** sont des outils puissants dans Scheme pour structurer votre code, mais ils servent à des fins différentes. Comprendre quand utiliser chacun aide à créer des scripts propres, modulaires et efficaces.

### Aperçu

- **Nommé `let`** : Une construction qui combine la liaison de variables et la récursivité dans une portée localisée, généralement utilisée pour les calculs itératifs ou récursifs.
- **Local `define`** : Un moyen de définir des fonctions d'assistance ou des variables dans la portée d'une fonction englobante, les rendant réutilisables dans différentes parties de cette fonction.

---

### Nommé `let`

#### Caractéristiques :
1. Combine les liaisons de variables et la récursivité en une seule construction.
2. Portée sur le corps du bloc `let`.
3. Idéal pour la **récursion localisée** ou les processus itératifs spécifiques à une seule tâche.

#### Syntaxe
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Exemple : additionner les éléments d'une liste
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Résultat** : `10`

- **Comment ça marche** : La fonction `loop` est définie dans `let`, permettant des appels récursifs avec des liaisons mises à jour.

---

### Local `define`

#### Caractéristiques :
1. Permet la création de fonctions d'assistance ou de variables réutilisables dans la fonction englobante.
2. Limité à la fonction enveloppante mais visible dans tout son corps.
3. Idéal pour modulariser le code avec plusieurs étapes ou une logique réutilisable.

#### Syntaxe
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Exemple : traitement de plusieurs valeurs
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Résultat** : `41` (Calcule \(2^2 + 3^3 + 4^2\))

- **Comment ça marche** : Les fonctions d'assistance `square` et `cube` sont réutilisables dans la fonction `process-values`, permettant une logique modulaire.

---

### Différences clés

| **Aspect** | **Nommé `let`** | **Local `define`** |
|------------------------------------|--------------------------------------------------|------------------------------------------------|
| **Objectif** | Combine la récursivité et l'itération de manière localisée. | Définit des fonctions ou des variables d'assistance réutilisables. |
| **Portée** | Limité au corps du bloc `let`.           | Visible dans toute la fonction englobante.      |
| **Réutilisabilité** | Non réutilisable en dehors du bloc `let`.             | Réutilisable plusieurs fois dans la fonction.    |
| **Meilleur cas d'utilisation** | Récursivité localisée ou itération liée à une seule tâche. | Modularisation du code avec plusieurs étapes réutilisables. |
| **Syntaxe** | Combine la liaison et la récursivité dans une seule construction.  | Définit explicitement des fonctions ou des variables.      |

---

### Quand utiliser Named `let`

1. **Logique à usage unique** : lorsque la récursivité ou l'itération est spécifique à un seul calcul.
2. **Encapsulation** : pour éviter d'ajouter des noms de fonctions supplémentaires à l'espace de noms de la fonction englobante.
3. **Itération** : lors de la gestion de variables intermédiaires dans une construction en boucle.

**Exemple : Calcul factoriel**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Résultat** : `120`

---

### Quand utiliser le local `define`

1. **Aide réutilisables** : lorsque la logique doit être réutilisée dans plusieurs parties de la fonction.
2. **Conception modulaire** : pour diviser les calculs complexes en sous-tâches plus petites et nommées.
3. **Étapes multiples** : lorsque plusieurs fonctions d'assistance sont nécessaires pour différentes parties du calcul.**Exemple : Traitement des entrées**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Résultat** : `(13 36)` (Calcule \(2^2 + 3^2\) et \(2^2 \cdot 3^2\))

---

### Combinaison de la déclaration et de la saisie dans Named `let`

L'une des fonctionnalités les plus puissantes d'un `let` nommé est sa capacité à combiner la **déclaration de variable locale** et les **paramètres d'entrée** pour la récursivité en une seule construction. Cela rend le nom `let` à la fois concis et expressif pour les tâches itératives ou récursives.

#### Déclaration de variable locale
Dans un `let` nommé, les liaisons entre parenthèses agissent comme des **variables locales** initialisées avec des valeurs spécifiques. Ces variables sont limitées au corps du `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` et `y`** sont des variables locales définies et initialisées dans le cadre de `let`.

---

#### Paramètres d'entrée pour la récursivité
Les mêmes variables agissent également comme **paramètres d'entrée** pour les appels récursifs au `let` nommé. Lorsque le `let` nommé s'appelle, il met à jour ces variables avec de nouvelles valeurs.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Première itération** : `x = 1`, `y = 2`
- **Deuxième itération** : `x = 2`, `y = 4`
- **Troisième itération** : `x = 3`, `y = 8`, et ainsi de suite...

---

#### Équivalent en utilisant Local `define`

Un `let` nommé inclut l'initialisation des variables dans le cadre de sa syntaxe. Cela élimine le besoin d’une étape distincte pour configurer les valeurs initiales. Les deux exemples suivants sont équivalents :

##### Utilisation de Named `let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Utilisation locale `define`
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Les deux effectuent le même calcul, mais le `let` nommé combine la déclaration de variable et la configuration de la récursion en une seule construction concise.

---

#### Avantages de la combinaison de la déclaration et de la saisie

1. **Concision** : nommé `let` réduit le passe-partout en fusionnant l'initialisation des variables et la récursivité en une seule construction.
2. **Clarté** : cela indique clairement que la récursivité est locale au `let` et liée à une tâche spécifique.
3. **Encapsulation** : la logique récursive reste autonome et ne pollue pas l'espace de noms de la fonction englobante.

Cette nature à double usage d'un `let` nommé - à la fois en tant que déclaration de variable et mécanisme de saisie récursif - est ce qui en fait une fonctionnalité puissante et unique dans la programmation Scheme.

### Résumé

- Utilisez **named `let`** pour la **récursion localisée** ou l'**itération**, en particulier lorsque la logique est étroitement couplée à une seule tâche.
- Utilisez **local `define`** pour **modulariser le code** avec des fonctions ou des variables d'assistance réutilisables.

En comprenant leurs différences, vous pouvez écrire des programmes Scheme plus concis, organisés et maintenables.