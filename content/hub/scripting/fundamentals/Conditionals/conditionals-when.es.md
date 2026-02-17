---
title: "cuando"
type: docs
weight: 5
---
En Scheme, si bien `if` es elegante y versátil, puede resultar confuso cuando se usa sin un `else` explícito. Esto es particularmente cierto cuando la intención es ejecutar una sola rama de código solo cuando una condición es verdadera, sin ninguna acción alternativa para el caso `false`. En tales escenarios, la construcción `when` proporciona una alternativa más clara y concisa.

La forma básica de `when` se ve así:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Si `test` se evalúa como verdadero (`#t`), todas las expresiones en el cuerpo de la construcción `when` se ejecutan secuencialmente.
- Si `test` se evalúa como falso (`#f`), no sucede nada y no se devuelve ningún valor.

### Ejemplo

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Contrastando `if` y `when`

Para comprender mejor la diferencia entre `if` y `when`, considere el siguiente ejemplo donde ambos se usan juntos:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Explicación:

1. **La condición `if`**:
   - La prueba `(= 0 1)` comprueba si 0 es igual a 1.
   - Como esto es falso (`#f`), se ejecuta la rama `else` del `if`.

2. **La construcción `when` en la rama `else`**:
   - La prueba `when` `(< 0 1)` comprueba si 0 es menor que 1.
   - Dado que esto es cierto (`#t`), todas las expresiones dentro del cuerpo del `when` se ejecutan secuencialmente:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### ¿Por qué utilizar `when` aquí?

- Usar `when` en lugar de otro `if` simplifica la lógica cuando no hay necesidad de una rama `else` explícita para la condición.
- `when` deja claro que solo la rama verdadera es relevante, lo que reduce la posible confusión.

### Resumen

- Utilice `if` cuando necesite tanto una rama verdadera como una falsa.
- Utilice `when` cuando solo haya una rama para el caso real, especialmente cuando se deben ejecutar múltiples acciones.
- Combinar `if` y `when` puede ayudar a estructurar condicionales más complejos de forma clara y concisa.