---
title: "Listes d'associations (Alists)"
type: docs
weight: 6
---
Une **liste d'associations** (ou **aliste**) est une structure de données fondamentale dans Scheme utilisée pour représenter des collections de paires clé-valeur. Il est implémenté sous la forme d'une liste de paires, où chaque paire associe une clé (généralement un symbole) à une valeur. Les listes sont simples, flexibles et bien adaptées aux ensembles de données de petite et moyenne taille.

### Structure d'une liste d'associations

Une liste est une liste où chaque élément est une **paire** (construite avec `cons`). Chaque binôme est composé de :

- **Clé** : Le premier élément (généralement un symbole).
- **Valeur** : Le deuxième élément, qui peut être de n'importe quel type de données.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Clé** : `'name`, `'age`, `'city`
- **Valeur** : `"Alice"`, `30`, `"Paris"`
- **Structure** : Une liste de paires :
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Création d'une liste

Vous pouvez créer une liste en construisant manuellement des paires ou en la construisant par programme à l'aide de `cons`.

#### Utilisation du guillemet unique (`'`)

Le guillemet simple (`'`) est un raccourci pour **quoting**, ce qui empêche Scheme d'évaluer l'expression. Cela le rend idéal pour créer des listes statiques où toutes les clés et valeurs sont codées en dur.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Résultat** :
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Utilisation du backquote (`` ` ``) and Comma (`,`)

L'opérateur backquote (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Ceci est utile pour créer des listes dans lesquelles les clés ou les valeurs sont calculées au moment de l'exécution.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Résultat** :
`((name . "Alice") (age . 30) (city . "Paris"))`

### Exemple de comparaison

Liste statique utilisant `'` :

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Liste dynamique utilisant `` ` `` and `,` :

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Accéder aux données dans une liste

Pour récupérer une valeur d'une liste, vous pouvez utiliser la fonction `assoc`, qui recherche une paire par sa clé.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Extraction de la valeur

Une fois que vous avez récupéré une paire à l'aide de `assoc`, utilisez `cdr` pour extraire la valeur :

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Résumé des principales fonctionnalités

- **Single Quote (`'`)** : crée une liste statique où tous les éléments sont des données littérales.
- **Citation arrière (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Dot Notation (`.`)** : Utilisé pour construire des paires, associant une clé à une valeur dans une liste.