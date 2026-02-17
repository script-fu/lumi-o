---
title: "Lambda-Funktionen"
type: docs
weight: 1
---
**Lambda-Funktionen** in Scheme sind anonyme Funktionen, das heißt, sie sind Funktionen ohne Namen. Diese Funktionen werden inline definiert und werden typischerweise für kurze, einmalige Vorgänge verwendet. Das `lambda`-Konstrukt ist ein leistungsstarkes Werkzeug in der funktionalen Programmierung, mit dem Sie im Handumdrehen prägnante und flexible Logik erstellen können.

Lambda-Funktionen sind besonders nützlich, wenn:

- Sie benötigen eine kleine Funktion für einen bestimmten, vorübergehenden Zweck.
- Übergabe von Funktionen als Argumente an Funktionen höherer Ordnung wie `map`, `filter` oder `fold`.
- Rückgabe von Funktionen von anderen Funktionen.

### Syntax von Lambda-Funktionen

Lambda-Funktionen können eigenständig definiert werden ...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...oder sofort aufgerufen:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Die Parameter, die die Funktion akzeptiert.
- **`body-expression`:** Die Logik, die ausgeführt wird, wenn die Funktion aufgerufen wird.
- **Sofortiger Aufruf:** Das zweite Formular zeigt, wie ein Lambda sofort mit Argumenten aufgerufen wird.

### Beispiele für Lambda-Funktionen

#### Verwendung von Lambda für einfache Berechnungen

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Hier:

– Es wird eine Lambda-Funktion erstellt, um zwei Zahlen hinzuzufügen (`x` und `y`).
- Die Funktion wird sofort mit den Argumenten `3` und `5` aufgerufen.

#### Inline-Lambda-Funktionen

Das folgende Beispiel zeigt, wie `for-each` sowohl mit einer benannten Funktion als auch mit einer Lambda-Funktion verwendet wird:

**Verwenden einer benannten Funktion:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Erklärung**:
  - `print-item` ist eine benannte Funktion, die eine Zahl in eine Zeichenfolge (`number->string`) umwandelt und diese mit `lumi-message` ausgibt.
  - `for-each` wendet `print-item` auf jedes Element in der Liste `(1 2 3 4)` an.

**Ausgabe**: 1 2 3 4

**Verwenden einer Lambda-Funktion:**

Dieselbe Logik kann inline mit einer Lambda-Funktion geschrieben werden, sodass keine separate benannte Funktion erforderlich ist:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Erklärung**:
  - Das `(lambda (x) (lumi-message (number->string x)))` definiert eine anonyme Funktion.
  - Diese Funktion wird von `for-each` auf jedes Element der Liste `(1 2 3 4)` angewendet.

**Ausgabe**: 1 2 3 4

#### Lambda fungiert als Argumente

Lambda-Funktionen werden oft direkt an Funktionen höherer Ordnung wie `map` oder `filter` übergeben.

#### Eine Liste von Zahlen quadrieren

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- Die Funktion `lambda` quadriert jedes Element der Liste.
- Die Funktion `map` wendet den `lambda` auf jedes Element an.

#### Lambda fungiert als Rückgabewerte

Sie können eine Lambda-Funktion von einer anderen Funktion zurückgeben, um dynamisches Verhalten zu erzeugen.

#### Generieren einer Addiererfunktion

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` generiert eine neue Lambda-Funktion, die eine bestimmte Zahl hinzufügt (`n`).
– Das zurückgegebene Lambda wird in `add5` gespeichert, wodurch `5` zu seiner Eingabe hinzugefügt wird.

#### Verwendung von Lambda mit `let`

Lambdas werden oft mit `let` verwendet, um lokal gültige, temporäre Funktionen zu erstellen.

#### Lokales Lambda zur Addition

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- Das `let` bindet eine Lambda-Funktion an den Namen `add`.
- Das Lambda wird dann als normale Funktion im `let`-Bereich verwendet.

#### Lambdas mit Funktionen höherer Ordnung kombinieren

Lambdas glänzen, wenn sie mit Funktionen höherer Ordnung kombiniert werden, um komplexe Datentransformationen durchzuführen.

#### Gerade Zahlen filtern

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- Der `lambda` prüft, ob eine Zahl gerade ist.
– Die Funktion `filter` verwendet das Lambda, um nur gerade Zahlen aus der Liste zu behalten.

### Vorteile von Lambda-Funktionen

- **Prägnanz:** Lambdas reduzieren den Boilerplate-Code, indem sie die Notwendigkeit beseitigen, separate benannte Funktionen zu definieren.
- **Flexibilität:** Sie können sie überall dort definieren und verwenden, wo sie benötigt werden, wodurch der Code modularer wird.
- **Verbesserte Lesbarkeit:** Bei kurzen, spezifischen Aufgaben machen Lambdas die Absicht klar, ohne den Code mit zusätzlichen benannten Funktionen zu überladen.

### Wann man Lambda-Funktionen verwendet

Verwenden Sie Lambda-Funktionen, wenn:

- Die Logik ist kurz und in sich geschlossen.
- Die Funktion wird nur vorübergehend oder in einem bestimmten Umfang benötigt.
- Sie arbeiten mit Funktionen höherer Ordnung wie `map`, `filter` oder `reduce`.

Vermeiden Sie die Verwendung von Lambdas für komplexe, mehrzeilige Logik, da dies die Lesbarkeit beeinträchtigen kann. Für umfangreichere Vorgänge verwenden Sie stattdessen eine benannte Funktion.

### Fazit

Lambda-Funktionen in Scheme bieten eine übersichtliche und leistungsstarke Möglichkeit, anonyme Funktionen für bestimmte Aufgaben zu definieren. Ihre Flexibilität und Benutzerfreundlichkeit machen sie zu einem unverzichtbaren Werkzeug für jeden Scheme-Programmierer. Wenn Sie wissen, wie Sie `lambda` effektiv nutzen, können Sie sauberere, modularere und effizientere Skripte schreiben.