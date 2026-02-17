---
title: "Einfache Rekursion"
type: docs
weight: 5
---
Rekursion ist ein leistungsstarkes Konzept in Scheme, bei dem sich eine Funktion selbst aufruft, um kleinere Teilprobleme des ursprünglichen Problems zu lösen. Ein **einfaches Rekursionsmuster** umfasst einen Basisfall, um die Rekursion zu stoppen, und einen rekursiven Fall, um das Problem zu reduzieren.

Die allgemeine Struktur einer rekursiven Funktion sieht folgendermaßen aus:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Grundbedingung**: Stoppt die Rekursion.
- **Basisergebnis**: Der zurückgegebene Wert, wenn die Basisbedingung erfüllt ist.
- **Rekursiver Aufruf**: Ein Aufruf der Funktion selbst mit geänderten Argumenten, die die Berechnung näher an den Basisfall heranführen.

---

### Beispiel: Summe der Zahlen (1 bis n)

Eine einfache rekursive Funktion zur Berechnung der Summe der Zahlen von 1 bis n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Wie es funktioniert: Zerlegen und wieder zusammenbauen

Bei der Rekursion wird das ursprüngliche Problem in kleinere Teile zerlegt. Jeder Aufruf der Funktion verarbeitet einen Teil und gibt den Rest weiter. Sobald der einfachste Fall erreicht ist, werden die Ergebnisse nach Abschluss der Berechnung wieder zusammengesetzt.

#### Schritt-für-Schritt-Nachverfolgung der Summe zu n 3

1. **Erster Aufruf**: *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Zweiter Aufruf**: *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Dritter Aufruf**: *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Basisfall**: *Summe-zu-n 0*
   → *0*

---

#### Zusammenbau des Endergebnisses

Sobald der einfachste Fall gelöst ist, wird jede Schicht der Berechnung abgeschlossen:

1. *sum-to-n 0* ergibt *0*
2. *sum-to-n 1* wird zu *(+ 1 0) = 1*
3. *sum-to-n 2* wird zu *(+ 2 1) = 3*
4. *sum-to-n 3* wird zu *(+ 3 3) = 6*

---

### Beispiel: Jedes Element einer Liste drucken

Hier ist eine einfache rekursive Funktion zum Drucken jedes Elements in einer Liste:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Basisfall:** Wenn die Liste leer ist (*null? lst*), Rekursion stoppen.
- **Rekursiver Fall:** Drucken Sie das erste Element (*car lst*) und rufen Sie dann die Funktion für den Rest der Liste auf (*cdr lst*).

#### Beispielverwendung

```scheme
(print-elements (list 1 2 3))
```

Ausgabe:

- *"1"*
- *"2"*
- *"3"*

Ergebnis: *"erledigt"*

---

#### Wie es funktioniert

1. Die Funktion ruft mit *car* das erste Element der Liste ab und verarbeitet es.
2. Anschließend ruft es sich selbst mit dem Rest der Liste (*cdr*) auf.
3. Dieser Vorgang wiederholt sich, bis die Liste leer ist (*null? lst*).

---

### Zusammenfassung

- Einfache Rekursion besteht aus:
  1. **Basisfall**: Stoppt die Rekursion.
  2. **Rekursiver Fall**: Reduziert das Problem in Richtung des Basisfalls.
- Jeder rekursive Aufruf treibt die Berechnung bis zum Abschluss voran.
– Sobald der Basisfall erreicht ist, werden die Ergebnisse kombiniert, während die Rekursion abgeschlossen ist.

Die Rekursion spiegelt die Struktur des Problems wider und sorgt für einen klaren, logischen Ablauf. Stellen Sie immer einen Basisfall sicher, um eine unendliche Rekursion zu vermeiden.