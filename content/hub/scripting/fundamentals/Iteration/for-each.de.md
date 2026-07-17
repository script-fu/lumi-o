---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
url: "hub/scripting/fundamentals/Iteration/for-each"
---
Die Funktion `for-each` in Scheme wendet eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) an. Im Gegensatz zu `map`, das eine neue Liste zurückgibt, dient `for-each` **Nebeneffekten** wie Ausgabe oder Variablenänderung.

Die einfachste Form von `for-each`:

```scheme
(for-each procedure list)
```

- **Prozedur:** Funktion pro Element.
- **Liste:** Zu verarbeitende Liste.

---

### Beispiel: Liste ausgeben

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- `print-item` wird auf `(1 2 3 4)` angewendet.
- Jede Zahl wird nacheinander ausgegeben.

**Ausgabe**: `1 2 3 4`

---

### So funktioniert es

1. **Über jedes Element iterieren:** Die Prozedur wird der Reihe nach ausgeführt.
2. **Nebeneffekte:** Drucken, Protokollieren oder externe Variablen ändern — ohne neue Liste.

---

#### Mehrere Listen

Mit mehreren Listen verarbeitet `for-each` entsprechende Elemente paarweise.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

**Ausgabe**: `5 7 9`

---

### Zusammenfassung

- `for-each` eignet sich für Nebeneffekte pro Listenelement.
- Im Gegensatz zu `map` **keine neue Liste**.
- Mehrere Listen werden gleichzeitig verarbeitet.

Nutzen Sie `for-each`, wenn Aktionen wichtiger sind als Datentransformation.