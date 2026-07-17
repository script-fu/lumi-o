---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db8c12b44717a78fddabba563fc62d081db9644b8a1f2b09d74db91eec84bfd1
---
Die Funktion `do` in Scheme ist eine Schleife mit Initialisierung, Aktualisierung und Abbruchbedingung. Nützlich, wenn eine Sequenz eine bestimmte Anzahl von Malen oder bis zu einer Bedingung laufen soll.

Die allgemeine Form von `do`:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable:** Schleifenvariable(n).
- **Initial-value:** Startwert.
- **Update-expression:** Aktualisierung pro Iteration.
- **Termination-condition:** Abbruchbedingung.
- **Result-expression:** Rückgabewert beim Abbruch.
- **Body:** Code pro Iteration.

---

### Beispiel: Summe von 1 bis 5

```scheme
(do ((i 1 (+ i 1))      ; i auf 1 initialisieren, um 1 erhöhen
     (sum 0 (+ sum i))) ; Summe auf 0 initialisieren, i zur Summe addieren
    ((> i 5) sum)       ; Beenden, wenn i > 5, sum zurückgeben
  (lumi-message (number->string sum))) ; Gibt die Summe bei jedem Schritt aus
```

- `i` startet bei 1 und wird inkrementiert.
- `sum` akkumuliert die Summe.
- Abbruch bei `i > 5`, Rückgabe von `sum`.

**Ausgabe**: `15`

---

### So funktioniert es

1. **Initialisierung:** Startwerte zuweisen.
2. **Abbruchprüfung:** Zu Beginn jeder Iteration.
3. **Iteration:** Body ausführen, Variablen aktualisieren.

---

### Zusammenfassung

- `do` bietet flexible Schleifen mit mehreren Variablen.
- Nützlich bei Zustandsänderungen über Iterationen.
- Die Abbruchbedingung bestimmt Ende und Ergebnis.

`do` verbindet **gebundene Variablen** (wie `let`) mit **iterativer Steuerung**.