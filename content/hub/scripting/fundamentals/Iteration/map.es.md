---
title: "mapa"
type: docs
weight: 3
---
La función `map` en Scheme se utiliza para aplicar un procedimiento a cada elemento de una lista (o varias listas) y **devolver una nueva lista** que contiene los resultados. Esto lo hace ideal para transformar datos.

La forma más simple de `map` se ve así:

```scheme
(map procedure list)
```

- **Procedimiento**: Una función para aplicar a cada elemento de la lista.
- **Lista**: La lista cuyos elementos se transformarán.

---

### Ejemplo: duplicar cada elemento

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Aquí, la función `double` se aplica a cada elemento de la lista `(1 2 3 4)`.
- El resultado es una nueva lista con cada elemento duplicado.

**Salida**: `(2 4 6 8)`

---

### Cómo funciona

1. **Crea una nueva lista**:
   - `map` aplica el procedimiento proporcionado a cada elemento de la lista y recopila los resultados en una nueva lista.

2. **Transforma datos**:
   - Se utiliza principalmente para transformaciones de datos en lugar de realizar efectos secundarios.

---

#### Ejemplo: uso con varias listas

Si se proporcionan varias listas, `map` procesa los elementos correspondientes de cada lista.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- La función `sum` agrega los elementos correspondientes de las dos listas y devuelve los resultados como una nueva lista.

**Salida**: `(5 7 9)`

---

### Resumen

- La función `map` es una poderosa herramienta para transformar listas aplicando un procedimiento a cada elemento.
- A diferencia de `for-each`, `map` **produce una nueva lista** que contiene los resultados de la aplicación del procedimiento.
- Admite múltiples listas, lo que permite operaciones por elementos entre ellas.

Al utilizar `map`, puede crear eficientemente versiones transformadas de sus datos manteniendo las listas originales sin cambios.