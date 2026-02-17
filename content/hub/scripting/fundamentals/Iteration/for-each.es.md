---
title: "para cada uno"
type: docs
weight: 5
---
La función `for-each` en Scheme se utiliza para aplicar un procedimiento a cada elemento de una lista (o varias listas). A diferencia de `map`, que devuelve una nueva lista con los resultados, `for-each` se utiliza por sus **efectos secundarios**, como imprimir o actualizar variables.

La forma más simple de `for-each` se ve así:

```scheme
(for-each procedure list)
```

- **Procedimiento**: Una función para aplicar a cada elemento de la lista.
- **Lista**: La lista cuyos elementos serán procesados.

---

### Ejemplo: imprimir una lista

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Aquí, la función `print-item` se aplica a cada elemento de la lista `(1 2 3 4)`.
- Esto hace que cada número se imprima secuencialmente.

**Salida**: `1 2 3 4`

---

### Cómo funciona

1. **Itera sobre cada elemento**:
   - El procedimiento proporcionado se ejecuta para cada elemento de la lista, en orden.

2. **Realiza efectos secundarios**:
   - Los efectos secundarios comunes incluyen imprimir, registrar o modificar variables externas. A diferencia de `map`, `for-each` no devuelve una lista nueva.

---

#### Ejemplo: uso con varias listas

Si se proporcionan varias listas, `for-each` procesa los elementos correspondientes de cada lista.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- La función `sum-and-print` suma los elementos correspondientes de las dos listas e imprime los resultados.

**Salida**: `5 7 9`

---

### Resumen

- La función `for-each` es útil para realizar efectos secundarios en cada elemento de una lista.
- A diferencia de `map`, `for-each` no produce una nueva lista; se centra únicamente en los efectos secundarios del procedimiento.
- Puede manejar múltiples listas simultáneamente, aplicando el procedimiento a los elementos correspondientes.

Al utilizar `for-each`, puede procesar listas de manera efectiva cuando el objetivo es realizar acciones en lugar de transformar datos.