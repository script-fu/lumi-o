---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c11f2c7984493d3fda20fca757958884b8752ef9a15640e4a7357c544e29c6c6
---
A função `map` em Scheme aplica um procedimento a cada elemento de uma lista (ou várias listas) e **retorna uma nova lista** com os resultados. É ideal para transformar dados.

La forme la plus simple de `map` ressemble à ceci :

```scheme
(map procedure list)
```

- **Procédure :** Fonction appliquée à chaque élément.
- **Liste :** Liste à transformer.

---

### Exemple : doubler chaque élément

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- `double` est appliqué à `(1 2 3 4)`.
- Résultat : nouvelle liste avec valeurs doublées.

**Sortie** : `(2 4 6 8)`

---

### Comment ça marche

1. **Nouvelle liste :** `map` collecte les résultats.
2. **Transformation :** Plutôt que des effets de bord.

---

#### Plusieurs listes

Avec plusieurs listes, `map` traite les éléments correspondants.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

**Sortie** : `(5 7 9)`

---

### Résumé

- `map` transforme des listes élément par élément.
- Contrairement à `for-each`, `map` **produit une nouvelle liste**.
- Plusieurs listes sont traitées par paires.

Avec `map`, créez des versions transformées tout en conservant les listes d'origine.