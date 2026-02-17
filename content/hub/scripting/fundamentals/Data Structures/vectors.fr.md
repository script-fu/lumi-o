---
title: "Vecteurs"
type: docs
weight: 5
---
Dans Scheme, un vecteur est une autre structure de données fondamentale utilisée pour regrouper des valeurs. Contrairement aux listes, les vecteurs sont des collections d’éléments indexés de taille fixe, offrant un accès aléatoire et des mises à jour plus rapides. Chaque élément d'un vecteur peut être de n'importe quel type, y compris un autre vecteur. Les vecteurs sont représentés par # suivi de parenthèses. `#(1 2 3)`

Bien que les vecteurs et les listes puissent sembler similaires, ils remplissent des objectifs différents dans la programmation Scheme :

- Les listes sont plus couramment utilisées pour les opérations récursives et les structures dynamiques, car leur implémentation de nœuds liés permet une manipulation efficace de leur début et de leur parcours via une décomposition récursive.

- Les vecteurs, en revanche, sont optimisés pour les scénarios dans lesquels un accès aléatoire à des éléments ou des mises à jour d'index spécifiques sont requis, ce qui les rend plus adaptés aux cas d'utilisation tels que les tables de recherche, les configurations de taille fixe ou les opérations indexées critiques en termes de performances.

Essentiellement, les listes constituent le choix naturel pour les algorithmes récursifs et les données de taille dynamique, tandis que les vecteurs brillent lorsque les modèles d'accès de taille fixe ou indexés sont primordiaux.

### Vecteurs simples

```scheme
(vector 1 2 3)
```

- Crée un vecteur de trois éléments : `1`, `2` et `3`.

Résultat : **`#(1 2 3)`**

#### Accès aux éléments vectoriels

Les éléments d'un vecteur sont accessibles à l'aide de la procédure `vector-ref`, elle récupère l'élément à un index spécifié (à partir de `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Itération : traitement de chaque élément dans un vecteur

Vous pouvez parcourir un vecteur à l’aide d’une boucle ou d’une récursion. Scheme fournit `vector-length` pour déterminer la taille d'un vecteur. Voici une boucle simple pour imprimer chaque élément d'un vecteur :

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Cas de base :** Si l'index `i` atteint la longueur du vecteur, arrêtez la boucle.
- **Cas récursif :** Imprimez l'élément à l'index `i`, puis incrémentez `i`.

#### Exemple d'utilisation

```scheme
(print-elements (vector 1 2 3))
```

Résultat :

- `"1"`
- `"2"`
- `"3"`

Résultat : "fait"

### Vecteurs mixtes

Les vecteurs peuvent inclure des éléments de différents types, notamment des chaînes, des booléens, des nombres, d'autres vecteurs ou même le résultat d'expressions :

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Cela crée un vecteur avec :
  - Un numéro (`42`)
  - Une chaîne (`"hello"`)
  - Un booléen (`#t`)
  - Un autre vecteur (`#(1 2)`)
  - Le résultat d'une expression (`(+ 3 4)`, qui s'évalue à `7`)

Résultat : **`#(42 "hello" #t #(1 2) 7)`**

### Construire des vecteurs

Les vecteurs sont créés en utilisant `vector`, ou en utilisant `make-vector` pour créer un vecteur de taille fixe avec une valeur initiale.

```scheme
(make-vector 5 0)
```

Crée un vecteur de taille `5` avec tous les éléments initialisés à `0`.

Résultat : `#(0 0 0 0 0)`

### Mise à jour des vecteurs

La procédure `vector-set!` met à jour un élément dans un vecteur à un index spécifié.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Résultat : `#(1 42 3)`

### Vérification des vecteurs

La procédure `vector?` vérifie si une valeur donnée est un vecteur.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Résultat :

- `(vector? (vector 1 2 3))` renvoie `#t` (vrai)
- `(vector? 42)` renvoie `#f` (faux)

### Vecteurs et comportement de passage par référenceDans Scheme, les vecteurs sont mutables et transmis par référence. Cela signifie que lorsque vous transmettez un vecteur à une fonction, la fonction peut modifier directement le vecteur d'origine. Toute modification apportée au vecteur à l’intérieur de la fonction sera également reflétée à l’extérieur de la fonction. Ce comportement est utile pour partager et mettre à jour efficacement les données entre plusieurs fonctions, mais il nécessite également de la prudence pour éviter les effets secondaires involontaires.

#### Exemple : Modification d'un vecteur dans une fonction

Voici un exemple illustrant comment les vecteurs sont passés par référence et modifiés :

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Résultat : `#(10 99 30)`

#### Explication étape par étape

1. **Créer un vecteur :** `my-vector` est initialisé avec les valeurs `10`, `20` et `30`.
2. **Passer à une fonction :** `my-vector` est transmis à `modify-vector` avec l'index et la nouvelle valeur à mettre à jour.
3. **Modifier dans la fonction :** La procédure `vector-set!` met à jour la valeur à l'index spécifié directement dans le vecteur d'origine.
4. **Refléter les modifications :** Étant donné que les vecteurs sont transmis par référence, les modifications apportées au sein de la fonction sont reflétées dans le vecteur d'origine.

#### Implications du passage par référence

- **Performance :** Le passage de vecteurs par référence est efficace car cela évite de copier de grandes structures.
- **Effets secondaires :** Soyez prudent lorsque vous partagez des vecteurs entre fonctions afin d'éviter des modifications involontaires des données partagées.

### Opérations sur les vecteurs

Scheme fournit plusieurs procédures intégrées pour travailler avec des vecteurs, notamment :

- `vector-length` : Renvoie le nombre d'éléments dans un vecteur.
- `vector->list` : Convertit un vecteur en liste.
- `list->vector` : Convertit une liste en vecteur.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Résultat :

- `(vector-length (vector 1 2 3))` renvoie `3`
- `(vector->list (vector 1 2 3))` renvoie `(1 2 3)`
- `(list->vector (list 1 2 3))` renvoie `#(1 2 3)`

### Vecteurs imbriqués

Les vecteurs dans Scheme peuvent contenir d'autres vecteurs en tant qu'éléments, créant ainsi une structure imbriquée.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Crée un vecteur de trois éléments, chacun étant lui-même un vecteur.

Résultat : **`#(#(1 2) #(3 4) #(5))`**

#### Accès aux données imbriquées

Pour accéder aux éléments d'un vecteur imbriqué, utilisez `vector-ref` plusieurs fois pour naviguer dans la structure.

#### Exemple : accès aux éléments

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Résumé

- Les **Vecteurs** dans Scheme sont des structures de données indexées de taille fixe.
- Utilisez `vector` pour créer un vecteur, `vector-ref` pour accéder aux éléments et `vector-set!` pour mettre à jour les éléments.
- Les procédures intégrées telles que `vector-length`, `vector->list` et `list->vector` permettent des opérations flexibles.
- Les vecteurs imbriqués permettent des structures de données complexes et hiérarchiques.