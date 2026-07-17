---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c11f2c7984493d3fda20fca757958884b8752ef9a15640e4a7357c544e29c6c6
---
La función `map` en Scheme aplica un procedimiento a cada elemento de una lista (o varias listas) y **devuelve una nueva lista** con los resultados. Ideal para transformar datos.

La forma más simple de `map` es:

```scheme
(map procedure list)
```

- **Procedimiento:** Función por elemento.
- **Lista:** Lista a transformar.

---

### Ejemplo: duplicar cada elemento

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- `double` se aplica a `(1 2 3 4)`.
- Resultado: nueva lista duplicada.

**Salida**: `(2 4 6 8)`

---

### Cómo funciona

1. **Nueva lista:** `map` recopila resultados.
2. **Transformación:** Más que efectos secundarios.

---

#### Varias listas

Con varias listas, `map` procesa elementos correspondientes.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

**Salida**: `(5 7 9)`

---

### Resumen

- `map` transforma listas elemento a elemento.
- A diferencia de `for-each`, `map` **produce una nueva lista**.
- Varias listas se procesan por pares.

Con `map`, cree versiones transformadas manteniendo las listas originales.