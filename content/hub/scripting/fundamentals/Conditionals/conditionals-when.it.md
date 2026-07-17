---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
In Scheme `if` è versatile, ma senza un ramo `else` esplicito può confondere — soprattutto quando deve essere eseguito solo il ramo vero, senza alternativa per il caso falso. In questi casi, `when` è più chiaro e conciso.

La forma base di `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` è vero (`#t`), tutte le espressioni nel corpo di `when` vengono eseguite in sequenza.
- Se `test` è falso (`#f`), non succede nulla e non viene restituito alcun valore.

### Esempio

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Confronto tra `if` e `when`

Entrambi nello stesso esempio:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Spiegazione

1. **Condizione `if`:**
   - Il test `(= 0 1)` verifica se 0 è uguale a 1.
   - Poiché è falso (`#f`), viene eseguito il ramo `else`.

2. **`when` nel ramo `else`:**
   - Il test `(< 0 1)` verifica se 0 è minore di 1.
   - Poiché è vero (`#t`), tutte le espressioni nel corpo di `when` vengono eseguite in sequenza.

#### Perché `when`?

- Evita un `else` vuoto o fittizio.
- Chiarisce che conta solo il ramo vero.

### Riepilogo

- Usa `if` quando servono entrambi i rami, vero e falso.
- Usa `when` quando c'è solo il ramo vero, soprattutto per più azioni.
- Combinare `if` e `when` aiuta a strutturare condizioni complesse in modo chiaro e conciso.
