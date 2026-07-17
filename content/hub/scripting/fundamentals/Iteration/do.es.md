---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db8c12b44717a78fddabba563fc62d081db9644b8a1f2b09d74db91eec84bfd1
---
La función `do` en Scheme es un bucle con inicialización, actualización y condición de terminación. Útil para ejecutar una secuencia un número de veces o hasta cumplir una condición.

La forma general de `do`:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable:** variable(s) del bucle.
- **Initial-value:** valor inicial.
- **Update-expression:** actualización por iteración.
- **Termination-condition:** condición de parada.
- **Result-expression:** valor al terminar.
- **Body:** código por iteración.

---

### Ejemplo: suma del 1 al 5

```scheme
(do ((i 1 (+ i 1))      ; Inicializar i en 1, incrementar en 1
     (sum 0 (+ sum i))) ; Inicializar suma en 0, añadir i a la suma
    ((> i 5) sum)       ; Terminar cuando i > 5, devolver sum
  (lumi-message (number->string sum))) ; Imprime la suma en cada paso
```

- `i` empieza en 1 e incrementa.
- `sum` acumula la suma.
- Parada cuando `i > 5`, retorno de `sum`.

**Salida**: `15`

---

### Cómo funciona

1. **Inicialización:** valores iniciales.
2. **Comprobación de parada:** al inicio de cada vuelta.
3. **Iteración:** ejecutar cuerpo, actualizar variables.

---

### Resumen

- `do` ofrece bucles flexibles con varias variables.
- Útil cuando el estado cambia en cada vuelta.
- La condición de parada fija el fin y el resultado.

`do` combina **enlaces** (como `let`) y **control iterativo**.