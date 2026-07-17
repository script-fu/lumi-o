---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
En su forma más simple, `if` en Scheme evalúa una prueba y, según el resultado, ejecuta uno de dos bloques de código:

```scheme
(if test-is-true
  do-this)
```

- Si `#t`, se ejecuta el **consequent** (valor o efectos secundarios).

### Ejemplo

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Prueba: `(< 0 1)` es verdadera.
- Se ejecuta `(lumi-message "True!")`.

### Rama else: `if-else`

```scheme
(if test
  do-this
  else-do-this)
```

```scheme
(if test
  consequent
  alternative)
```

### Cómo funciona

1. **Evaluar** la prueba primero.
2. Si `#t` **consequent**, si `#f` **alternative**.

Ambos bloques pueden ser cualquier expresión Scheme válida.

### Ejemplos

#### Ejemplo 1: devolver un valor

```scheme
(if (< 0 1)
  1
  0)
```

Resultado: **1**

#### Ejemplo 2: bloque `begin`

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

Resultado: **Imprime "False condition met, calculating..." y devuelve 12.**

#### Ejemplo 3: expresión `let`

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

Resultado: **Imprime "True condition met, calculating..." y devuelve -10.**

### Resumen

- `if` evalúa pruebas y ejecuta el bloque adecuado.
- Expresiones simples o grupos `begin`/`let`.
- Sin `else` explícito, solo **consequent** si es verdadero.