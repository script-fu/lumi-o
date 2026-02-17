---
title: "Variadische Funktionen"
type: docs
weight: 2
---
**Variadische Funktionen** in Scheme sind Funktionen, die eine variable Anzahl von Argumenten akzeptieren. Diese Funktionen sind äußerst vielseitig und ermöglichen die Erstellung flexiblen und wiederverwendbaren Codes. In der funktionalen Programmierung vereinfachen variadische Funktionen Vorgänge, die eine beliebige Anzahl von Eingaben verarbeiten müssen, z. B. das Summieren einer Liste von Zahlen oder das Verketten von Zeichenfolgen.

Variadische Funktionen sind besonders nützlich, wenn:

- Die Anzahl der Argumente kann nicht im Voraus bestimmt werden.
- Sie müssen denselben Vorgang auf eine dynamische Liste von Eingaben anwenden.
- Schreiben von Dienstprogrammen für die Datenaggregation oder -transformation.

### Syntax variadischer Funktionen

Variadische Funktionen werden mit dem Symbol `.` vor dem letzten Parameternamen definiert. Dieser letzte Parameter sammelt alle verbleibenden Argumente in einer Liste.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Alle erforderlichen, festen Argumente, die die Funktion akzeptiert.
- **`variadic-parameter`:** Ein spezieller Parameter mit vorangestelltem `.`, der zusätzliche Argumente als Liste sammelt.
- **`body-expression`:** Die Logik, die ausgeführt wird, wenn die Funktion aufgerufen wird.

### Beispiele für variadische Funktionen

#### Grundlegende variadische Funktion

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Erklärung**:
  - `numbers` sammelt alle Argumente in einer Liste.
  - `apply` wendet die Funktion `+` auf alle Elemente der Liste an.

**Verwendung**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Variadische Funktion mit festen Parametern

Sie können feste Parameter mit einem variadischen Parameter kombinieren, um flexiblere Funktionen zu erstellen.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Erklärung**:
  - `prefix` ist ein festes Argument.
  - `names` sammelt die verbleibenden Argumente in einer Liste.
  - Jedem Namen wird die angegebene Zeichenfolge mit `map` und `lambda` vorangestellt.

**Verwendung**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Kombination von fester und variadischer Logik

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Erklärung**:
  - `collection-name` ist ein fester Parameter.
  - `items` sammelt zusätzliche Argumente in einer Liste.
  – Die Funktion verkettet den Sammlungsnamen und die Elemente zu einer einzigen Zeichenfolge.

**Verwendung**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Erweiterte Anwendungsfälle

#### Verarbeitung willkürlicher Eingaben

Variadische Funktionen zeichnen sich durch die Verarbeitung beliebiger Daten aus. Hier ist ein Beispiel für die Summierung nur positiver Zahlen:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtert nicht positive Zahlen vor der Summierung heraus.

**Verwendung**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Variadische Funktionen mit rekursiver Logik

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Erklärung**:
  - `first` behandelt das erste Argument.
  - `rest` sammelt verbleibende Argumente in einer Liste.
  - Berechnet rekursiv den Maximalwert.

**Verwendung**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Vorteile variadischer Funktionen

- **Flexibilität:** Sie bearbeiten ein breites Spektrum an Eingabefällen.
- **Prägnanz:** Reduzieren Sie den Bedarf an mehreren überladenen Funktionen.
- **Dynamische Operationen:** Ermöglichen Sie die Datenverarbeitung zur Laufzeit, ohne vorher die Anzahl der Argumente zu kennen.

### Wann man variadische Funktionen verwenden sollte

Verwenden Sie variadische Funktionen, wenn:

- Die Funktion muss eine unbekannte Anzahl von Argumenten verarbeiten.
- Eine einzige Operation gilt für alle Eingaben (z. B. Summieren, Verketten oder Zuordnen).
- Vereinfachung der Logik höherer Ordnung mit dynamischen Argumenten.

Vermeiden Sie variadische Funktionen, wenn:

- Die Eingabevalidierung oder Typprüfung ist komplex.
- Für die erforderliche Logik genügen feste Argumente.
- Die Lesbarkeit wird durch zu komplexe Vorgänge beeinträchtigt.

### AbschlussVariadische Funktionen in Scheme bieten einen robusten Mechanismus zur Verarbeitung dynamischer Eingaben. Wenn Sie deren Syntax und Verwendung verstehen, können Sie flexible und leistungsstarke Skripte erstellen, die sich an verschiedene Szenarien anpassen. In Kombination mit Funktionen höherer Ordnung machen variadische Funktionen Ihren Code prägnanter und ausdrucksvoller.