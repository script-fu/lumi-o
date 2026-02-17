---
title: "pour chaque"
type: docs
weight: 5
---
La fonction `for-each` dans Scheme est utilisée pour appliquer une procédure à chaque élément d'une liste (ou de plusieurs listes). Contrairement à `map`, qui renvoie une nouvelle liste avec les résultats, `for-each` est utilisé pour ses **effets secondaires**, tels que l'impression ou la mise à jour de variables.

La forme la plus simple de `for-each` ressemble à ceci :

```scheme
(for-each procedure list)
```

- **Procédure** : Une fonction à appliquer à chaque élément de la liste.
- **Liste** : La liste dont les éléments seront traités.

---

### Exemple : Imprimer une liste

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Ici, la fonction `print-item` est appliquée à chaque élément de la liste `(1 2 3 4)`.
- Cela provoque l'impression séquentielle de chaque numéro.

**Sortie** : `1 2 3 4`

---

### Comment ça marche

1. **Itère sur chaque élément** :
   - La procédure fournie est exécutée pour chaque élément de la liste, dans l'ordre.

2. **Effectue des effets secondaires** :
   - Les effets secondaires courants incluent l'impression, la journalisation ou la modification de variables externes. Contrairement à `map`, `for-each` ne renvoie pas de nouvelle liste.

---

#### Exemple : Utilisation avec plusieurs listes

Si plusieurs listes sont fournies, `for-each` traite les éléments correspondants de chaque liste.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- La fonction `sum-and-print` additionne les éléments correspondants des deux listes et imprime les résultats.

**Sortie** : `5 7 9`

---

### Résumé

- La fonction `for-each` est utile pour réaliser des effets secondaires sur chaque élément d'une liste.
- Contrairement à `map`, `for-each` ne produit pas de nouvelle liste : elle se concentre uniquement sur les effets secondaires de la procédure.
- Il peut gérer plusieurs listes simultanément, en appliquant la procédure aux éléments correspondants.

En utilisant `for-each`, vous pouvez traiter efficacement des listes lorsque l'objectif est d'effectuer des actions plutôt que de transformer des données.