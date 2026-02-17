---
title: "si"
type: docs
weight: 4
---
En su forma más simple, el condicional `if` en Scheme evalúa una prueba y, según el resultado, ejecuta uno de los dos bloques de código posibles. La forma más simple se ve así:

```scheme
(if test-is-true
  do-this)
```

- Si `test` se evalúa como verdadero (`#t`), se ejecuta el **bloque de código en el consecuente**. El bloque puede devolver un valor o realizar otras acciones, como asignar una variable o imprimir una salida.

### Ejemplo

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- En este caso, el `test` es `(< 0 1)` (comprobando si 0 es menor que 1).
- Dado que la prueba se evalúa como verdadera (`#t`), se ejecuta el bloque de código `(lumi-message "True!")`, que imprime `"True!"`.

### Agregar una condición más: `if-else`

Cuando se utiliza un condicional `if` con un bloque de código alternativo (el caso `else`), la estructura se ve así:

```scheme
(if test
  do-this
  else-do-this)
```

- Si `test` se evalúa como verdadero (`#t`), se ejecuta el bloque de código **consecuente**.
- Si `test` se evalúa como falso (`#f`), se ejecuta el bloque de código **alternativo**.

```scheme
(if test
  consequent
  alternative)
```

### Cómo funciona

1. **Expresión de prueba**:
   - La expresión `test` se evalúa primero.

2. **Resultado basado en la prueba**:
   - Si `test` se evalúa como verdadero (`#t`), se ejecuta el **bloque de código consiguiente**.
   - Si `test` se evalúa como falso (`#f`), se ejecuta el **bloque de código alternativo**.

Tanto los bloques de código `consequent` como `alternative` pueden realizar cualquier operación válida de Scheme, incluida la devolución de valores, la modificación de variables o la ejecución de procedimientos.

### Ejemplos

#### Ejemplo 1: Devolver un valor

```scheme
(if (< 0 1)
  1
  0)
```

- Aquí, el `test` es `(< 0 1)` (comprobando si 0 es menor que 1).
- Dado que la prueba se evalúa como verdadera (`#t`), el bloque **consecuente** (`1`) se ejecuta y se devuelve su valor.

Resultado: **1**

#### Ejemplo 2: Evaluación de un bloque inicial

En los casos en los que necesite realizar varias acciones cuando la condición sea verdadera o falsa, puede usar `begin` o `let` para agruparlas.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- En este ejemplo, `test` es `(= 0 1)` (verificando si 0 es igual a 1).
- Dado que la prueba se evalúa como falsa (`#f`), se ejecuta el bloque **alternativo**:
  - Primero, imprime `"False condition met, calculating..."`.
  - Luego, calcula `(* 3 4)` y devuelve `12`.

Resultado: **Imprime "Condición falsa cumplida, calculando..." y devuelve 12.**

#### Ejemplo 3: Evaluación de una declaración let

El uso de `let` nos permite declarar variables de alcance local dentro del bloque de código.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- En este ejemplo, `test` es `(= 1 1)` (comprobando si 1 es igual a 1).
- Dado que la prueba se evalúa como verdadera (`#t`), se ejecuta el bloque **consecuente**:
  - Primero, imprime `"True condition met, calculating..."`.
  - Luego, calcula `(* -1 10)` y devuelve `-10`.

Resultado: **Imprime "Se cumplió la condición verdadera, calculando..." y devuelve -10.**

### Resumen- El condicional `if` es una poderosa herramienta en Scheme para evaluar pruebas y ejecutar los bloques de código correspondientes.
- Puede manejar tanto expresiones simples como bloques de código complejos que devuelven valores, modifican variables o realizan efectos secundarios.
- Recuerde: Si no hay un bloque `else` explícito, el `if` solo evalúa y ejecuta el **consecuente** si la prueba es verdadera. De lo contrario, evalúa y ejecuta la **alternativa**.