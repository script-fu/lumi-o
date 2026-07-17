---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c11f2c7984493d3fda20fca757958884b8752ef9a15640e4a7357c544e29c6c6
---
Die Funktion `map` in Scheme wendet eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) an und **gibt eine neue Liste** mit den Ergebnissen zurück. Ideal für Datentransformationen.

Die einfachste Form von `map` sieht so aus:

```scheme
(map procedure list)
```

- **Prozedur:** Funktion für jedes Listenelement.
- **Liste:** Zu transformierende Liste.

---

### Beispiel: Elemente verdoppeln

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- `double` wird auf `(1 2 3 4)` angewendet.
- Ergebnis: neue Liste mit verdoppelten Werten.

**Ausgabe**: `(2 4 6 8)`

---

### So funktioniert es

1. **Neue Liste:** `map` sammelt Ergebnisse in einer neuen Liste.
2. **Transformation:** Für Datentransformation, nicht Nebeneffekte.

---

#### Mehrere Listen

Mit mehreren Listen verarbeitet `map` Elemente paarweise.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

**Ausgabe**: `(5 7 9)`

---

### Zusammenfassung

- `map` transformiert Listen elementweise.
- Im Gegensatz zu `for-each` **erzeugt `map` eine neue Liste**.
- Mehrere Listen werden paarweise verarbeitet.

Mit `map` erstellen Sie transformierte Datensätze, während Originallisten unverändert bleiben.