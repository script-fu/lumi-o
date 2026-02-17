---
title: "für-jeden"
type: docs
weight: 5
---
Die Funktion `for-each` in Scheme wird verwendet, um eine Prozedur auf jedes Element einer Liste (oder mehrerer Listen) anzuwenden. Im Gegensatz zu `map`, das eine neue Liste mit den Ergebnissen zurückgibt, wird `for-each` wegen seiner **Nebeneffekte**, wie dem Drucken oder Aktualisieren von Variablen, verwendet.

Die einfachste Form von `for-each` sieht so aus:

```scheme
(for-each procedure list)
```

- **Prozedur**: Eine Funktion, die auf jedes Element der Liste angewendet wird.
- **Liste**: Die Liste, deren Elemente verarbeitet werden.

---

### Beispiel: Eine Liste drucken

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Hier wird die Funktion `print-item` auf jedes Element der Liste `(1 2 3 4)` angewendet.
- Dadurch wird jede Zahl nacheinander gedruckt.

**Ausgabe**: `1 2 3 4`

---

### Wie es funktioniert

1. **Iteriert über jedes Element**:
   – Die bereitgestellte Prozedur wird für jedes Element in der Liste der Reihe nach ausgeführt.

2. **Führt zu Nebenwirkungen**:
   - Zu den häufigen Nebenwirkungen gehören das Drucken, Protokollieren oder Ändern externer Variablen. Im Gegensatz zu `map` gibt `for-each` keine neue Liste zurück.

---

#### Beispiel: Verwendung mit mehreren Listen

Wenn mehrere Listen bereitgestellt werden, verarbeitet `for-each` entsprechende Elemente aus jeder Liste.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Die Funktion `sum-and-print` summiert entsprechende Elemente aus den beiden Listen und gibt die Ergebnisse aus.

**Ausgabe**: `5 7 9`

---

### Zusammenfassung

– Die Funktion `for-each` ist nützlich, um Nebenwirkungen auf jedes Element einer Liste auszuführen.
- Im Gegensatz zu `map` erstellt `for-each` keine neue Liste – es konzentriert sich ausschließlich auf die Nebenwirkungen des Verfahrens.
- Es kann mehrere Listen gleichzeitig verarbeiten und das Verfahren auf entsprechende Elemente anwenden.

Durch die Verwendung von `for-each` können Sie Listen effektiv verarbeiten, wenn das Ziel eher darin besteht, Aktionen auszuführen als Daten zu transformieren.