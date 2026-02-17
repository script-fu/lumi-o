---
title: "hacer"
type: docs
weight: 5
---
La función `do` en Scheme es un mecanismo de bucle que permite la iteración con condiciones de inicialización, actualización y terminación. Es particularmente útil cuando necesita realizar una secuencia de operaciones una cantidad específica de veces o hasta que se cumpla una condición.

La forma general de `do` es:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable**: Las variables del bucle.
- **Valor inicial**: el valor inicial de cada variable de bucle.
- **Expresión-actualización**: La expresión para actualizar las variables del bucle al final de cada iteración.
- **Condición de terminación**: La condición para detener el bucle.
- **Expresión-resultado**: el valor que se devolverá cuando finalice el ciclo.
- **Cuerpo**: El código a ejecutar en cada iteración.

---

### Ejemplo: Sumar los números del 1 al 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- La variable de bucle `i` comienza en 1 y aumenta en 1 en cada iteración.
- La variable `sum` acumula la suma de `i`.
- El bucle termina cuando `i > 5`, devolviendo el valor final de `sum`.

**Salida**: `15`

---

### Cómo funciona

1. **Inicialización**:
   - A cada variable del bucle se le asigna su valor inicial.

2. **Verificación de terminación**:
   - Al inicio de cada iteración, se comprueba la condición de terminación. Si es verdadero, el ciclo se detiene y se evalúa la expresión resultante.

3. **Iteración**:
   - Si la condición de terminación es falsa, se ejecuta el cuerpo y las variables del bucle se actualizan utilizando sus respectivas expresiones de actualización.

---

### Resumen

- La construcción `do` proporciona una forma flexible de implementar bucles con múltiples variables y condiciones de terminación complejas.
- Es útil para tareas que requieren actualizaciones de estado entre iteraciones.
- La condición de terminación determina cuándo termina el ciclo y puede devolver un resultado final.

Al utilizar `do`, puede implementar algoritmos iterativos en Scheme con control preciso sobre la inicialización, las actualizaciones y la terminación. Esto hace que `do` sea una combinación de un **mecanismo de enlace con alcance** (como `let`) y una **estructura de control iterativa**, lo que le permite manejar bucles y estados temporales de una manera limpia y concisa.