---
title: "definir"
type: docs
weight: 3
---
La declaración `define` en Scheme es una construcción versátil que se utiliza para crear enlaces globales o locales. Se usa más comúnmente para definir variables y funciones, haciéndolas reutilizables y accesibles a través de un script o dentro de un alcance específico. Comprender `define` es crucial para escribir programas Scheme modulares, reutilizables y legibles.

### Propósito de `define`

La construcción `define` tiene múltiples propósitos:
- **Definición de variables**: asigna valores a nombres de variables, haciéndolos disponibles para su uso posterior.
- **Definición de funciones**: crea procedimientos reutilizables que encapsulan una lógica específica.
- **Definiciones locales**: cuando se usa dentro de una función, `define` crea enlaces locales que no afectan el espacio de nombres global.

---

### Definición de variables con `define`

Un uso básico de `define` es crear variables que contengan valores constantes o calculados.

#### Sintaxis
```scheme
(define variable-name value)
```

#### Ejemplo: definición de una constante
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Resultado**: `6.28318`

---

### Definición de funciones con `define`

Puede utilizar `define` para crear procedimientos reutilizables.

#### Sintaxis
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Ejemplo: definición de una función simple
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Resultado**: `16`

---

### Definiciones locales con `define`

Cuando se usa dentro de una función, `define` crea enlaces locales a los que solo se puede acceder dentro de la función adjunta. Esto evita contaminar el espacio de nombres global y ayuda a organizar su código.

#### Ejemplo: funciones auxiliares locales
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultado**: `41` (Calcula \(2^2 + 3^3 + 4^2\))

---

### Características clave de `define`

1. **Alcance Global o Local**:
   - Cuando se usa en el nivel superior, `define` crea variables o funciones globales.
   - Cuando se usa dentro de otra función, `define` crea enlaces locales.

2. **Reutilizabilidad**:
   - Las funciones definidas con `define` se pueden reutilizar varias veces en diferentes contextos.

3. **Legibilidad mejorada**:
   - Dividir la lógica en funciones más pequeñas y bien nombradas mejora la claridad y la facilidad de mantenimiento de su código.

---

### Diferencias entre `define` y `let`

| **Aspecto** | **`define`** | **`let`** |
|---------------------------------|--------------------------------------------------|----------------------------------------|
| **Propósito** | Crea enlaces globales o locales para variables o funciones. | Crea enlaces temporales en un ámbito localizado. |
| **Alcance** | Global cuando está en el nivel superior; local cuando está dentro de otra función. | Siempre local para el bloque `let`.       |
| **Reutilizabilidad** | Las funciones y variables se pueden reutilizar en varios lugares. | Las variables están vinculadas temporalmente para un solo bloque. |
| **Sintaxis** | Define explícitamente variables o funciones.       | Combina enlace variable con evaluación de expresión. |