---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e1e9a2537cadc894d45c7e25e28e9234f35e06298c289c5be57c15e7800cb8cd
---
La función `for-each` en Scheme aplica un procedimiento a cada elemento de una lista (o varias listas). A diferencia de `map`, que devuelve una nueva lista, `for-each` se usa por sus **efectos secundarios**: imprimir, registrar o modificar variables.

La forma más simple de `for-each`:

```scheme
(for-each procedure list)
```

- **Procedimiento:** Función por elemento.
- **Lista:** Lista a procesar.

---

### Ejemplo: imprimir una lista

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- `print-item` se aplica a `(1 2 3 4)`.
- Cada número se imprime en secuencia.

**Salida**: `1 2 3 4`

---

### Cómo funciona

1. **Iterar cada elemento:** El procedimiento se ejecuta en orden.
2. **Efectos secundarios:** Imprimir o modificar estado — sin nueva lista.

---

#### Varias listas

Con varias listas, `for-each` procesa elementos correspondientes.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

**Salida**: `5 7 9`

---

### Resumen

- `for-each` sirve para efectos secundarios por elemento.
- A diferencia de `map`, **sin nueva lista**.
- Varias listas a la vez.

Use `for-each` cuando la acción importa más que transformar datos.