---
title: "Einfache Rekursion"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47fd79f37d5542e30722efaf4f87cd10efb77d825101f2045b191e3640137168
---
Rekursion in Scheme bedeutet, dass eine Funktion sich selbst aufruft, um kleinere Teilprobleme zu lösen. **Einfache Rekursion** hat einen Basisfall zum Stoppen und einen rekursiven Fall zur Problemverkleinerung.

Allgemeine Struktur:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Base Condition:** stoppt die Rekursion.
- **Base Result:** Wert im Basisfall.
- **Recursive Call:** Aufruf mit angepassten Argumenten.

---

### Beispiel: Summe 1 bis n

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Basisfall: Stopp, wenn n 0 ist
    0                          ; Basisergebnis: Summe ist 0
    (+ n (sum-to-n (- n 1))))) ; Rekursiver Aufruf: aktuelles n mit Ergebnis des kleineren Problems addieren
```

#### Zerlegen und wieder zusammensetzen

Rekursion zerlegt das Problem; jeder Aufruf bearbeitet ein Stück. Am Basisfall setzt sich das Ergebnis wieder zusammen.

#### Schritt für Schritt: sum-to-n 3

1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*
2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*
3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*
4. *sum-to-n 0* → *0*

#### Ergebnis zusammensetzen

1. *sum-to-n 0* → *0*
2. *sum-to-n 1* → *1*
3. *sum-to-n 2* → *3*
4. *sum-to-n 3* → *6*

---

### Beispiel: Listenelemente ausgeben

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Gibt das erste Element aus
      (print-elements (cdr lst)))))             ; Verarbeitet den Rest der Liste
```

- **Basisfall:** leere Liste → `"done"`.
- **Rekursiv:** `car` ausgeben, Rest mit `cdr` verarbeiten.

#### Verwendung

```scheme
(print-elements (list 1 2 3))
```

Ausgabe: *"1"*, *"2"*, *"3"* — Ergebnis: *"done"*

### Zusammenfassung

- Basisfall stoppt; rekursiver Fall verkleinert das Problem.
- Jeder Aufruf nähert sich dem Basisfall.
- Immer einen Basisfall definieren — sonst endlose Rekursion.