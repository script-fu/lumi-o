---
title: "Listes d'associations (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
---
Une **liste d'associations** (ou **alist**) est une structure de données fondamentale en Scheme pour représenter des collections de paires clé-valeur. Elle est implémentée sous la forme d'une liste de paires, où chaque paire associe une clé (généralement un symbole) à une valeur. Les alists sont simples, flexibles et bien adaptées aux ensembles de données de petite et moyenne taille.

### Structure d'une liste d'associations

Une alist est une liste dont chaque élément est une **paire** (construite avec `cons`). Chaque paire est composée de :

- **Clé** : Le premier élément (généralement un symbole).
- **Valeur** : Le deuxième élément, qui peut être de n'importe quel type de données.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Clé** : `'name`, `'age`, `'city`
- **Valeur** : `"Alice"`, `30`, `"Paris"`
- **Structure** : Une liste de paires :
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Créer une alist

Vous pouvez créer une alist en construisant manuellement des paires ou en la construisant par programme à l'aide de `cons`.

#### Utilisation du guillemet unique (`'`)

Le guillemet simple (`'`) est un raccourci pour **quoting**, ce qui empêche Scheme d'évaluer l'expression. Cela convient parfaitement pour créer des alists statiques où toutes les clés et valeurs sont codées en dur.

```scheme
;; Définir une alist manuellement
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Ajouter programmatiquement une nouvelle paire
(define updated-alist (cons '(country . "France") alist))
```

**Résultat** :
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Utilisation du backquote (`` ` ``) et de la virgule (`,`)

Le backquote (`` ` ``) ressemble au guillemet simple, mais permet d'insérer dynamiquement des expressions évaluées avec l'opérateur virgule (`,`). C'est utile pour créer des alists dans lesquelles les clés ou les valeurs sont calculées au moment de l'exécution.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Résultat** :
`((name . "Alice") (age . 30) (city . "Paris"))`

### Exemple de comparaison

Alist statique avec `'` :

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Alist dynamique avec `` ` `` et `,` :

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Accéder aux données d'une alist

Pour récupérer une valeur d'une alist, vous pouvez utiliser la fonction `assoc`, qui recherche une paire par sa clé.

```scheme
(assoc 'name alist)   ; Renvoie (name . "Alice")
(assoc 'country alist) ; Renvoie #f (clé introuvable)
```

### Extraction de la valeur

Une fois que vous avez récupéré une paire à l'aide de `assoc`, utilisez `cdr` pour extraire la valeur :

```scheme
(cdr (assoc 'name alist))   ; Renvoie "Alice"
```

### Résumé des principales fonctionnalités

- **Single Quote (`'`)** : crée une liste statique où tous les éléments sont des données littérales.
- **Backquote (`` ` ``)** : permet de créer des alists dynamiques en mélangeant éléments statiques et expressions évaluées (avec `,`).
- **Notation point (`.`)** : sert à construire des paires, en associant une clé à une valeur dans une alist.