---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
En Scheme, el condicional `cond` elige uno de varios bloques según múltiples pruebas — como un `if` multirrama evaluado en orden hasta el primer acierto.

### Sintaxis

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Pruebas en orden.
- Primer `#t`: **consequent** ejecutado, `cond` para.
- `else` opcional como respaldo.

### Cómo funciona

1. **Probar cada condición** en orden.
2. **Ejecutar el consequent** correspondiente; si no, `else` si existe.

### Ejemplos

#### Ejemplo 1: consecuentes de una expresión

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

Resultado: **"This will run"**

#### Ejemplo 2: varias acciones con `begin`

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

Resultado: **Imprime "Condition met" y devuelve 25.**

#### Ejemplo 3: bloque `let` en el consecuente

```scheme
(cond
  ;; Caso 1: si 0 es menor que -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Caso 2: si 0 es mayor que -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Caso por defecto: si ninguna condición anterior se cumple
  (else
    (let ((z 0))
      z)))
```

Resultado: **Imprime "Positive condition met" y devuelve 40.**

#### Ejemplo 4: respaldo con `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

Resultado: **"Fallback value"**

### Resumen

- `cond` para varias condiciones con claridad.
- Consecuentes simples o con `begin`.
- `let` para variables locales; `else` recomendado.