---
title: "Listes"
type: docs
weight: 4
---
Dans Scheme, une **list** est une structure de données fondamentale utilisée pour regrouper des valeurs. Les listes sont des collections ordonnées d'éléments où chaque élément peut être de n'importe quel type, y compris une autre liste. Les listes sont largement utilisées dans Scheme à la fois pour le stockage de données et la structure du programme.

### Exemple 1 : Liste simple

```scheme
(list 1 2 3)
```

- Crée une liste de trois éléments : `1`, `2` et `3`.

Résultat : **`(1 2 3)`**

---

#### Accéder aux éléments de la liste

Les éléments d'une liste sont accessibles à l'aide des procédures `car` et `cdr` :

- `car` récupère le premier élément d'une liste.
- `cdr` récupère le reste de la liste (tout sauf le premier élément).

#### Exemples

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Résultat :

- `(car my-list)` renvoie `1`
- `(cdr my-list)` renvoie `(2 3)`

---

#### Récursivité simple : parcourir une liste

En appelant récursivement `car` sur le `cdr` d'une liste, vous pouvez traiter chaque élément un par un jusqu'à ce que la liste soit parcourue. Ceci constitue la base de nombreux algorithmes de traitement de listes.

#### Exemple : Impression de chaque élément d'une liste

Voici une fonction récursive simple pour imprimer chaque élément d'une liste :

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Cas de base :** Si la liste est vide (`null? lst`), arrêtez la récursivité.
- **Cas récursif :** Imprimez le premier élément (`car lst`), puis appelez la fonction sur le reste de la liste (`cdr lst`).

#### Exemple d'utilisation

```scheme
(print-elements (list 1 2 3))
```

Sortie :

- `"1"`
- `"2"`
- `"3"`

Résultat : "fait"

---

#### Comment ça marche

1. La fonction récupère le premier élément de la liste à l'aide de `car` et le traite.
2. Il s'appelle ensuite avec le reste de la liste (`cdr`).
3. Ce processus se répète jusqu'à ce que la liste soit vide (`null? lst`).

---

### Exemple 2 : Types mixtes

Les listes peuvent inclure des éléments de différents types, notamment des chaînes, des booléens, des nombres, d'autres listes ou même le résultat d'expressions :

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Cela crée une liste avec :
  - Un numéro (`42`)
  - Une chaîne (`"hello"`)
  - Un booléen (`#t`)
  - Une autre liste (`(1 2)`)
  - Le résultat d'une expression (`(+ 3 4)`, qui s'évalue à `7`)

Résultat : **`(42 "hello" #t (1 2) 7)`**

---

Ces exemples démontrent la polyvalence des listes dans Scheme, ce qui en fait un outil puissant pour organiser et manipuler les données.

### Construire des listes

La procédure `cons` permet de construire une nouvelle liste en combinant un élément avec une liste existante.

```scheme
(cons new-element existing-list)
```

#### Exemple

```scheme
(cons 0 (list 1 2 3))
```

- Ajoute `0` au début de la liste `(1 2 3)`.

Résultat : **`(0 1 2 3)`**

---

### Vérification des listes

La procédure `list?` vérifie si une valeur donnée est une liste.

```scheme
(list? value)
```

#### Exemple : liste ?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Résultat :

- `(list? (list 1 2 3))` renvoie `#t` (vrai)
- `(list? 42)` renvoie `#f` (faux)

---

### Opérations sur les listes

Scheme fournit plusieurs procédures intégrées pour travailler avec des listes, notamment :

- `length` : Renvoie le nombre d'éléments dans une liste.
- `append` : Combine deux ou plusieurs listes en une seule.
- `reverse` : Renvoie une nouvelle liste avec les éléments dans l'ordre inverse.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Résultat :

- `(length (list 1 2 3))` renvoie `3`
- `(append (list 1 2) (list 3 4))` renvoie `(1 2 3 4)`
- `(reverse (list 1 2 3))` renvoie `(3 2 1)`#### Utilisation de `list-ref`

La procédure `list-ref` récupère l'élément à un index spécifié d'une liste (index de base zéro).

```scheme
(list-ref lst index)
```

- **`lst`** : La liste à partir de laquelle récupérer l'élément.
- **`index`** : Un index de base zéro indiquant quel élément renvoyer.

##### Exemple : list-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Résultat : `30`

---

### Listes imbriquées

Les listes dans Scheme peuvent contenir d'autres listes en tant qu'éléments, créant ainsi une structure imbriquée.

#### Exemple : Création d'une liste imbriquée

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Crée une liste de trois éléments, dont chacun est lui-même une liste.

Résultat : **`((1 2) (3 4) (5))`**

---

#### Accès aux données imbriquées

Pour accéder aux éléments d'une liste imbriquée, vous pouvez utiliser des combinaisons de `car` et `cdr` pour naviguer dans la structure.

#### Exemple : accès aux éléments

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Explication

1. **`car nested-list`** :
   - Récupère le premier élément de `nested-list`, qui est `(1 2)`.

2. **`car (car nested-list)`** :
   - Récupère le premier élément de `(1 2)`, qui est `1`.

3. **`cdr (car nested-list)`** :
   - Récupère le reste de `(1 2)`, qui est `(2)`.

4. **`car (cdr (car nested-list))`** :
   - Récupère le premier élément de `(2)`, qui est `2`.

---

#### Exemple : accès aux éléments à partir d'autres sous-listes

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Cette approche vous permet de naviguer et d'accéder systématiquement à des éléments spécifiques dans une liste imbriquée, offrant ainsi une grande flexibilité pour travailler avec des données hiérarchiques.

### Résumé

- Les **Listes** dans Scheme sont des structures de données polyvalentes et essentielles.
- Utilisez `list` pour créer une liste, `car` et `cdr` pour accéder aux éléments et `cons` pour construire des listes.
- Les procédures intégrées telles que `length`, `append`, `reverse` et `list-ref` rendent les opérations de liste faciles et efficaces.
- Les listes peuvent être imbriquées, permettant des structures de données complexes pour des cas d'utilisation avancés.