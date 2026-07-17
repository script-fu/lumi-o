---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 61f1a78c3b37d9a33d3dff25f889287b32fc932bea8c22b4c06100052944b6a6
---
In Scheme ist `if` vielseitig, wird aber ohne explizites `else` schnell unübersichtlich — besonders wenn nur der wahre Zweig ausgeführt werden soll. Dann ist `when` klarer und kompakter.

Die Grundform von `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Bei `#t` werden alle Ausdrücke im Body nacheinander ausgeführt.
- Bei `#f` passiert nichts; es wird kein Wert zurückgegeben.

### Beispiel

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### `if` und `when` im Vergleich

Beide zusammen im selben Beispiel:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Erklärung

1. **`if`:** `(= 0 1)` ist falsch, daher der `else`-Zweig.
2. **`when` im `else`:** `(< 0 1)` ist wahr; beide `lumi-message`-Aufrufe laufen.

#### Warum `when`?

- Kein leerer oder Dummy-`else` nötig.
- Macht deutlich, dass nur der wahre Zweig zählt.

### Zusammenfassung

- **`if`:** wenn beide Zweige gebraucht werden.
- **`when`:** nur wahrer Zweig, ggf. mehrere Aktionen.
- Kombination strukturiert komplexe Bedingungen klar.