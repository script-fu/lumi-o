---
title: "carte"
type: docs
weight: 3
---
La fonction `map` dans Scheme est utilisée pour appliquer une procédure à chaque élément d'une liste (ou plusieurs listes) et **renvoyer une nouvelle liste** contenant les résultats. Cela le rend idéal pour transformer les données.

La forme la plus simple de `map` ressemble à ceci :

```scheme
(map procedure list)
```

- **Procédure** : Une fonction à appliquer à chaque élément de la liste.
- **Liste** : La liste dont les éléments seront transformés.

---

### Exemple : Doublez chaque élément

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Ici, la fonction `double` est appliquée à chaque élément de la liste `(1 2 3 4)`.
- Le résultat est une nouvelle liste avec chaque élément doublé.

**Sortie** : `(2 4 6 8)`

---

### Comment ça marche

1. **Crée une nouvelle liste** :
   - `map` applique la procédure fournie à chaque élément de la liste et collecte les résultats dans une nouvelle liste.

2. **Transforme les données** :
   - Il est principalement utilisé pour les transformations de données plutôt que pour effectuer des effets secondaires.

---

#### Exemple : Utilisation avec plusieurs listes

Si plusieurs listes sont fournies, `map` traite les éléments correspondants de chaque liste.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- La fonction `sum` ajoute les éléments correspondants des deux listes et renvoie les résultats sous forme d'une nouvelle liste.

**Sortie** : `(5 7 9)`

---

### Résumé

- La fonction `map` est un outil puissant pour transformer des listes en appliquant une procédure à chaque élément.
- Contrairement à `for-each`, `map` **produit une nouvelle liste** contenant les résultats de l'application de la procédure.
- Il prend en charge plusieurs listes, permettant des opérations par éléments sur elles.

En utilisant `map`, vous pouvez créer efficacement des versions transformées de vos données tout en conservant les listes d'origine inchangées.