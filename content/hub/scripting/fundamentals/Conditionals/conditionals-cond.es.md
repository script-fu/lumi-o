---
title: "cond"
type: docs
weight: 5
---
En Scheme, el condicional `cond` se utiliza para seleccionar uno de varios bloques de código posibles para ejecutar, en función de múltiples pruebas. Es como una `if` de múltiples ramas, donde cada rama se verifica en orden hasta que se encuentra una coincidencia.

### Sintaxis

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Cada prueba se evalúa en el orden en que se escriben.
- Cuando una prueba se evalúa como verdadera (`#t`), su **consecuente** correspondiente se ejecuta y la expresión `cond` deja de evaluar más pruebas.
- La cláusula `else` es opcional y sirve como alternativa si ninguna de las pruebas se evalúa como verdadera.

### Cómo funciona

1. **Pruebe cada condición**:
   - `cond` evalúa las pruebas en el orden en que aparecen.

2. **Ejecutar el consecuente coincidente**:
   - Cuando se encuentra la primera prueba que se evalúa como verdadera (`#t`), se ejecuta su **consecuente**.
   - Si ninguna prueba se evalúa como verdadera y hay una cláusula `else`, se ejecuta **fallback-consequent**.

### Ejemplos

#### Ejemplo 1: Consecuentes de expresión única

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- La primera prueba `(< 3 2)` se evalúa como falsa (`#f`).
- La segunda prueba `(= 3 3)` se evalúa como verdadera (`#t`), por lo que se devuelve `"This will run"`.
- La cláusula `else` no se ejecuta porque ya se encontró una coincidencia.

Resultado: **"Esto se ejecutará"**

#### Ejemplo 2: Acciones múltiples usando `begin`

Cuando un consecuente implica múltiples acciones, use `begin` para agruparlas:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- La primera prueba `(< 5 3)` se evalúa como falsa (`#f`).
- La segunda prueba `(> 5 3)` se evalúa como verdadera (`#t`):
  - Imprime `"Condition met"`.
  - Luego calcula `(* 5 5)` y devuelve `25`.

Resultado: **Imprime "Condición cumplida" y devuelve 25.**

#### Ejemplo 3: uso de un bloque `let` en un consecuente

Cuando necesites introducir variables locales, utiliza un bloque `let`:

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- La primera prueba `(< 0 -1)` es falsa.
- La segunda prueba `(> 0 -1)` es verdadera, entonces:
  - Se ejecuta un bloque `let`, vinculando `y` a `20`.
  - Imprime `"Positive condition met"`.
  - Luego calcula `(+ y y)` y devuelve `40`.

Resultado: **Imprime "Condición positiva cumplida" y devuelve 40.**

#### Ejemplo 4: respaldo con `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Ninguna de las dos primeras pruebas se evalúa como verdadera.
- La cláusula `else` se ejecuta y devuelve `"Fallback value"`.

Resultado: **"Valor de reserva"**

### Resumen

- Utilice `cond` para manejar múltiples condiciones de manera clara y concisa.
- Los consecuentes pueden ser expresiones individuales o acciones agrupadas usando `begin`.
- Utilice `let` en consecuencia para declarar variables locales para los cálculos.
- Incluya siempre una cláusula `else` como alternativa para manejar casos inesperados.

Esta flexibilidad convierte a `cond` en una herramienta poderosa y legible para manejar lógica de ramificación compleja.