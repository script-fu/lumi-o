---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
En Scheme, `if` es versátil, pero sin un `else` explícito puede confundir — sobre todo cuando solo debe ejecutarse la rama verdadera. Entonces `when` es más claro y conciso.

La forma básica de `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Si `#t`, todas las expresiones del cuerpo se ejecutan en secuencia.
- Si `#f`, no ocurre nada; no se devuelve valor.

### Ejemplo

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Comparar `if` y `when`

Ambos juntos en el mismo ejemplo:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Explicación

1. **`if`:** `(= 0 1)` es falso, rama `else`.
2. **`when` en el `else`:** `(< 0 1)` es verdadero; ambos `lumi-message` se ejecutan.

#### ¿Por qué `when`?

- Sin `else` vacío o ficticio.
- Deja claro que solo importa la rama verdadera.

### Resumen

- **`if`:** cuando importan ambas ramas.
- **`when`:** solo rama verdadera, varias acciones.
- Combinarlos estructura condiciones complejas con claridad.