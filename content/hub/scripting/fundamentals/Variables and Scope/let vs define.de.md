---
title: "Benanntes Let oder lokales Definieren"
type: docs
weight: 5
---
Sowohl **mit dem Namen `let`** als auch **lokal `define`** sind leistungsstarke Tools in Scheme zum Strukturieren Ihres Codes, dienen jedoch unterschiedlichen Zwecken. Wenn Sie wissen, wann die einzelnen Elemente verwendet werden sollten, können Sie saubere, modulare und effiziente Skripte erstellen.

### Übersicht

- **Mit dem Namen `let`**: Ein Konstrukt, das Variablenbindung und Rekursion in einem lokalisierten Bereich kombiniert und normalerweise für iterative oder rekursive Berechnungen verwendet wird.
- **Lokal `define`**: Eine Möglichkeit, Hilfsfunktionen oder Variablen im Rahmen einer umschließenden Funktion zu definieren und sie so in verschiedenen Teilen dieser Funktion wiederverwendbar zu machen.

---

### Benannt `let`

#### Eigenschaften:
1. Kombiniert Variablenbindungen und Rekursion in einem einzigen Konstrukt.
2. Bezogen auf den Hauptteil des `let`-Blocks.
3. Ideal für **lokale Rekursion** oder iterative Prozesse speziell für eine einzelne Aufgabe.

#### Syntax
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Beispiel: Elemente einer Liste summieren
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Ergebnis**: `10`

- **So funktioniert es**: Die Funktion `loop` ist innerhalb von `let` definiert und ermöglicht rekursive Aufrufe mit aktualisierten Bindungen.

---

### Lokal `define`

#### Eigenschaften:
1. Ermöglicht die Erstellung von Hilfsfunktionen oder Variablen, die innerhalb der umschließenden Funktion wiederverwendbar sind.
2. Auf die umschließende Funktion beschränkt, aber im gesamten Körper sichtbar.
3. Ideal für die Modularisierung von Code mit mehreren Schritten oder wiederverwendbarer Logik.

#### Syntax
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Beispiel: Verarbeitung mehrerer Werte
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Ergebnis**: `41` (Berechnet \(2^2 + 3^3 + 4^2\))

- **So funktioniert es**: Die Hilfsfunktionen `square` und `cube` sind innerhalb der Funktion `process-values` wiederverwendbar und ermöglichen modulare Logik.

---

### Hauptunterschiede

| **Aspekt** | **Mit Namen `let`** | **Lokal `define`** |
|-------------------------|-------------------------------------------------|------------------------------------------------|
| **Zweck** | Kombiniert Rekursion und Iteration auf lokalisierte Weise. | Definiert wiederverwendbare Hilfsfunktionen oder Variablen. |
| **Umfang** | Beschränkt auf den Hauptteil des `let`-Blocks.           | Sichtbar in der gesamten Umschließungsfunktion.      |
| **Wiederverwendbarkeit** | Außerhalb des `let`-Blocks nicht wiederverwendbar.             | Innerhalb der Funktion mehrfach wiederverwendbar.    |
| **Bester Anwendungsfall** | Lokalisierte Rekursion oder Iteration, die an eine einzelne Aufgabe gebunden ist. | Modularisierender Code mit mehreren wiederverwendbaren Schritten. |
| **Syntax** | Kombiniert Bindung und Rekursion in einem Konstrukt.  | Definiert explizit Funktionen oder Variablen.      |

---

### Wann ist das benannte `let` zu verwenden?

1. **Einmallogik**: Wenn Rekursion oder Iteration spezifisch für eine einzelne Berechnung ist.
2. **Kapselung**: Um das Hinzufügen zusätzlicher Funktionsnamen zum Namensraum der umschließenden Funktion zu vermeiden.
3. **Iteration**: Bei der Verwaltung von Zwischenvariablen in einem Schleifenkonstrukt.

**Beispiel: Faktorielle Berechnung**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Ergebnis**: `120`

---

### Wann man lokales `define` verwenden sollte

1. **Wiederverwendbare Helfer**: Wenn Logik in mehreren Teilen der Funktion wiederverwendet werden muss.
2. **Modularer Aufbau**: Um komplexe Berechnungen in kleinere, benannte Unteraufgaben zu unterteilen.
3. **Mehrere Schritte**: Wenn mehrere Hilfsfunktionen für verschiedene Teile der Berechnung benötigt werden.**Beispiel: Eingaben verarbeiten**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Ergebnis**: `(13 36)` (Berechnet \(2^2 + 3^2\) und \(2^2 \cdot 3^2\))

---

### Kombination von Deklaration und Eingabe im benannten `let`

Eine der leistungsstärksten Funktionen eines benannten `let` ist seine Fähigkeit, **lokale Variablendeklaration** und **Eingabeparameter** für die Rekursion in einem einzigen Konstrukt zu kombinieren. Dies macht das benannte `let` sowohl prägnant als auch ausdrucksstark für iterative oder rekursive Aufgaben.

#### Lokale Variablendeklaration
In einem benannten `let` fungieren die Bindungen in den Klammern als **lokale Variablen**, die mit bestimmten Werten initialisiert werden. Diese Variablen beziehen sich auf den Hauptteil von `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` und `y`** sind lokale Variablen, die als Teil von `let` definiert und initialisiert werden.

---

#### Eingabeparameter für die Rekursion
Dieselben Variablen fungieren auch als **Eingabeparameter** für die rekursiven Aufrufe des benannten `let`. Wenn sich das benannte `let` selbst aufruft, aktualisiert es diese Variablen mit neuen Werten.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Erste Iteration**: `x = 1`, `y = 2`
- **Zweite Iteration**: `x = 2`, `y = 4`
- **Dritte Iteration**: `x = 3`, `y = 8` und so weiter ...

---

#### Äquivalent mit lokalem `define`

Ein benanntes `let` beinhaltet die Variableninitialisierung als Teil seiner Syntax. Dadurch entfällt die Notwendigkeit eines separaten Schritts zum Einrichten der Anfangswerte. Die folgenden zwei Beispiele sind äquivalent:

##### Verwendung des benannten `let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Lokales `define` verwenden
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Beide führen die gleiche Berechnung durch, aber das benannte `let` kombiniert die Variablendeklaration und die Rekursionseinrichtung in einem prägnanten Konstrukt.

---

#### Vorteile der Kombination von Deklaration und Eingabe

1. **Prägnanz**: Der Name `let` reduziert den Boilerplate, indem Variableninitialisierung und Rekursion in einem einzigen Konstrukt zusammengeführt werden.
2. **Klarheit**: Es macht deutlich, dass die Rekursion lokal für `let` ist und an eine bestimmte Aufgabe gebunden ist.
3. **Kapselung**: Rekursive Logik bleibt in sich geschlossen und verschmutzt nicht den Namensraum der einschließenden Funktion.

Dieser doppelte Zweck eines benannten `let` – sowohl als Variablendeklaration als auch als rekursiver Eingabemechanismus – macht es zu einer leistungsstarken und einzigartigen Funktion in der Scheme-Programmierung.

### Zusammenfassung

– Verwenden Sie **mit dem Namen `let`** für **lokalisierte Rekursion** oder **Iteration**, insbesondere wenn die Logik eng an eine einzelne Aufgabe gekoppelt ist.
- Verwenden Sie **local `define`** für die **Modularisierung von Code** mit wiederverwendbaren Hilfsfunktionen oder Variablen.

Wenn Sie ihre Unterschiede verstehen, können Sie prägnantere, organisiertere und wartbarere Scheme-Programme schreiben.