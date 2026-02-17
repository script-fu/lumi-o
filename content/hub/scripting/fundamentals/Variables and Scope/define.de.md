---
title: "definieren"
type: docs
weight: 3
---
Die `define`-Anweisung in Scheme ist ein vielseitiges Konstrukt, das zum Erstellen globaler oder lokaler Bindungen verwendet wird. Es wird am häufigsten zum Definieren von Variablen und Funktionen verwendet, um sie im gesamten Skript oder innerhalb eines bestimmten Bereichs wiederverwendbar und zugänglich zu machen. Das Verständnis von `define` ist entscheidend für das Schreiben modularer, wiederverwendbarer und lesbarer Scheme-Programme.

### Zweck von `define`

Das `define`-Konstrukt dient mehreren Zwecken:
- **Variablen definieren**: Weist Variablennamen Werte zu und macht sie für die spätere Verwendung verfügbar.
- **Funktionen definieren**: Erstellt wiederverwendbare Prozeduren, die bestimmte Logik kapseln.
- **Lokale Definitionen**: Bei Verwendung innerhalb einer Funktion erstellt `define` lokale Bindungen, die sich nicht auf den globalen Namespace auswirken.

---

### Variablen mit `define` definieren

Eine grundlegende Verwendung von `define` besteht darin, Variablen zu erstellen, die konstante oder berechnete Werte enthalten.

#### Syntax
```scheme
(define variable-name value)
```

#### Beispiel: Definieren einer Konstante
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Ergebnis**: `6.28318`

---

### Funktionen mit `define` definieren

Sie können `define` verwenden, um wiederverwendbare Prozeduren zu erstellen.

#### Syntax
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Beispiel: Definieren einer einfachen Funktion
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Ergebnis**: `16`

---

### Lokale Definitionen mit `define`

Bei Verwendung innerhalb einer Funktion erstellt `define` lokale Bindungen, auf die nur innerhalb der umschließenden Funktion zugegriffen werden kann. Dies vermeidet eine Verschmutzung des globalen Namespace und hilft bei der Organisation Ihres Codes.

#### Beispiel: Lokale Hilfsfunktionen
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Ergebnis**: `41` (Berechnet \(2^2 + 3^3 + 4^2\))

---

### Hauptmerkmale von `define`

1. **Globaler oder lokaler Geltungsbereich**:
   - Bei Verwendung auf der obersten Ebene erstellt `define` globale Variablen oder Funktionen.
   - Bei Verwendung innerhalb einer anderen Funktion erstellt `define` lokale Bindungen.

2. **Wiederverwendbarkeit**:
   - Mit `define` definierte Funktionen können in verschiedenen Kontexten mehrfach wiederverwendet werden.

3. **Verbesserte Lesbarkeit**:
   - Die Aufteilung der Logik in kleinere, gut benannte Funktionen verbessert die Klarheit und Wartbarkeit Ihres Codes.

---

### Unterschiede zwischen `define` und `let`

| **Aspekt** | **`define`** | **`let`** |
|-----------|-------------------------------------------------|------------------------------------------------------|
| **Zweck** | Erstellt globale oder lokale Bindungen für Variablen oder Funktionen. | Erstellt temporäre Bindungen in einem lokalisierten Bereich. |
| **Umfang** | Global, wenn auf der obersten Ebene; lokal, wenn es sich innerhalb einer anderen Funktion befindet. | Immer lokal für den `let` Block.       |
| **Wiederverwendbarkeit** | Funktionen und Variablen können an mehreren Stellen wiederverwendet werden. | Variablen werden vorübergehend für einen einzelnen Block gebunden. |
| **Syntax** | Definiert explizit Variablen oder Funktionen.       | Kombiniert Variablenbindung mit Ausdrucksauswertung. |