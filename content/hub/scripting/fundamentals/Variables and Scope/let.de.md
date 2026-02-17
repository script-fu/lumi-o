---
title: "lassen"
type: docs
weight: 4
---
Der Name `let` wird verwendet, weil er seinen mathematischen Ursprung der Einführung temporärer Bindungen widerspiegelt, wie in _"Seien \( x = 2 \) und \( y = 3 \)"_.

Eine `let`-Anweisung in Scheme ist ein **Bindungskonstrukt**, das zum Definieren von Variablen innerhalb eines lokalisierten Bereichs verwendet wird. Sie können damit temporäre Bindungen für Variablen erstellen und dann mithilfe dieser Bindungen einen Codeblock ausführen. Dies ist besonders nützlich, um den Code modular zu halten und eine Verschmutzung durch globale Variablen zu vermeiden.

Es gibt drei Hauptformen von `let` in Scheme:

- **`let`**: Standard-Let zum Erstellen einfacher lokaler Bindungen.
- **`let*`**: Sequentielles Let, wobei Bindungen von den Ergebnissen vorheriger Bindungen abhängen können.
- **Benannt `let`**: Eine spezielle Form von `let`, die rekursive Schleifen oder benannte Prozeduren erstellt.

In seiner einfachsten Form erstellt `let` lokale Variablenbindungen und wertet einen Ausdruck mit diesen Bindungen aus.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Bindungen**: Eine Liste von Paaren, wobei jedes Paar ein `value` einem `variable` zuweist.
- **Ausdruck**: Der Hauptteil des `let`, der die lokal definierten Variablen verwenden kann.

### Beispiel

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Dies definiert zwei lokale Variablen, `x` (10) und `y` (20).
- Anschließend wird `(+ x y)` unter Verwendung dieser Variablen berechnet.

**Ergebnis**: `30`

---

## Das `let*` Konstrukt

Das `let*`-Konstrukt ähnelt `let`, Bindungen werden jedoch **sequentiell** ausgewertet. Dies bedeutet, dass spätere Bindungen von früheren abhängig sein können.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Beispiel

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

– Die erste Bindung weist `10` `x` zu.
– Die zweite Bindung berechnet `y` als `(+ x 5)` unter Verwendung des Werts von `x`.
- Der Körper berechnet `(* x y)`.

**Ergebnis**: `150`

---

## Benannt `let`

Ein benannter `let` ist eine Sonderform von `let`, der einen Namen für den `let`-Block selbst bereitstellt und ihn in eine rekursive Prozedur umwandelt. Dies ist nützlich zum Erstellen von Schleifen oder rekursiven Berechnungen.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Name**: Der `let`-Block erhält einen Namen, der effektiv eine Funktion definiert.
- **Bindungen**: Anfangswerte für Variablen, ähnlich einem Standard `let`.
- **Body**: Der Ausdruck kann das benannte `let` rekursiv aufrufen.

### Beispiel: Schleife mit benanntem `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- Die Funktion `loop` beginnt mit `n = 5` und `result = 1`.
- Wenn `n` `0` ist, wird `result` zurückgegeben.
- Ansonsten ruft es sich rekursiv mit `n - 1` und `result * n` auf.

**Ergebnis**: `120` (Fakultät von 5)

---

## Übersichtstabelle| Konstruieren | Beschreibung | Anwendungsfall |
|------------|-------------|-------------------------------------------------------------------------|
| **`let`** | Definiert lokale Bindungen für Variablen.    | Verwenden Sie diese Option, wenn alle Bindungen unabhängig sind und nicht voneinander abhängig sind.     |
| **`let*`** | Definiert sequentielle lokale Bindungen.       | Verwenden Sie diese Option, wenn spätere Bindungen von den Ergebnissen früherer Bindungen abhängen.           |
| **Mit Namen `let`** | Definiert rekursive lokale Prozeduren. | Verwendung für Schleifen, iterative Berechnungen oder Rekursion in einem lokalen Kontext. |

---

## Beispiele

### Verwendung von `let` für die lokale Berechnung

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Ergebnis**: `13` (Berechnet `x² + y²`)

---

### Verwendung von `let*` für sequentielle Abhängigkeiten

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Ergebnis**: `8` (Berechnet `x³`)

---

### Verwendung des benannten `let` für rekursive Berechnungen

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Ergebnis**: `120` (Fakultät von 5)

---

Durch die Verwendung von `let`, `let*` und dem Namen `let` ermöglicht Scheme modulare, rekursive und sequentielle Programmierung mit klaren Scoping-Regeln.