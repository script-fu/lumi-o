---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e1e9a2537cadc894d45c7e25e28e9234f35e06298c289c5be57c15e7800cb8cd
---
La fonction `for-each` en Scheme applique une procédure à chaque élément d'une liste (ou de plusieurs listes). Contrairement à `map`, qui renvoie une nouvelle liste, `for-each` sert aux **effets de bord** : affichage, journalisation ou modification de variables.

La forme la plus simple de `for-each` :

```scheme
(for-each procedure list)
```

- **Procédure :** Fonction par élément.
- **Liste :** Liste à parcourir.

---

### Exemple : afficher une liste

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- `print-item` est appliqué à `(1 2 3 4)`.
- Chaque nombre est affiché successivement.

**Sortie** : `1 2 3 4`

---

### Comment ça marche

1. **Parcourir chaque élément :** La procédure s'exécute dans l'ordre.
2. **Effets de bord :** Affichage, journalisation ou modification — sans nouvelle liste.

---

#### Plusieurs listes

Avec plusieurs listes, `for-each` traite les éléments correspondants.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

**Sortie** : `5 7 9`

---

### Résumé

- `for-each` convient aux effets de bord sur chaque élément.
- Contrairement à `map`, **pas de nouvelle liste**.
- Plusieurs listes simultanément.

Utilisez `for-each` lorsque l'action prime sur la transformation.