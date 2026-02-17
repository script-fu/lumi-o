---
title: "Karte"
type: docs
weight: 3
---
Die Funktion `map` in Scheme wird verwendet, um eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) anzuwenden und **eine neue Liste zurückzugeben**, die die Ergebnisse enthält. Dies macht es ideal für die Transformation von Daten.

Die einfachste Form von `map` sieht so aus:

```scheme
(map procedure list)
```

- **Prozedur**: Eine Funktion, die auf jedes Element der Liste angewendet wird.
- **Liste**: Die Liste, deren Elemente transformiert werden.

---

### Beispiel: Jedes Element verdoppeln

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Hier wird die Funktion `double` auf jedes Element der Liste `(1 2 3 4)` angewendet.
- Das Ergebnis ist eine neue Liste, in der jedes Element verdoppelt wird.

**Ausgabe**: `(2 4 6 8)`

---

### Wie es funktioniert

1. **Erstellt eine neue Liste**:
   - `map` wendet die bereitgestellte Prozedur auf jedes Element der Liste an und sammelt die Ergebnisse in einer neuen Liste.

2. **Transformiert Daten**:
   – Es wird hauptsächlich für Datentransformationen und nicht für die Ausführung von Nebenwirkungen verwendet.

---

#### Beispiel: Verwendung mit mehreren Listen

Wenn mehrere Listen bereitgestellt werden, verarbeitet `map` entsprechende Elemente aus jeder Liste.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Die Funktion `sum` fügt entsprechende Elemente aus den beiden Listen hinzu und gibt die Ergebnisse als neue Liste zurück.

**Ausgabe**: `(5 7 9)`

---

### Zusammenfassung

- Die Funktion `map` ist ein leistungsstarkes Werkzeug zum Transformieren von Listen durch Anwenden einer Prozedur auf jedes Element.
- Im Gegensatz zu `for-each` erstellt `map` **eine neue Liste** mit den Ergebnissen der Anwendung des Verfahrens.
- Es unterstützt mehrere Listen und ermöglicht elementweise Operationen über diese hinweg.

Durch die Verwendung von `map` können Sie effizient transformierte Versionen Ihrer Daten erstellen und dabei die ursprünglichen Listen unverändert lassen.