---
title: "Vektoren"
type: docs
weight: 5
---
In Scheme ist ein Vektor eine weitere grundlegende Datenstruktur, die zum Gruppieren von Werten verwendet wird. Im Gegensatz zu Listen sind Vektoren indizierte Elementsammlungen fester Größe, die einen schnelleren Direktzugriff und schnellere Aktualisierungen ermöglichen. Jedes Element in einem Vektor kann von beliebigem Typ sein, einschließlich eines anderen Vektors. Vektoren werden durch # gefolgt von Klammern dargestellt. `#(1 2 3)`

Obwohl Vektoren und Listen ähnlich aussehen mögen, dienen sie in der Scheme-Programmierung unterschiedlichen Zwecken:

- Listen werden häufiger für rekursive Operationen und dynamische Strukturen verwendet, da ihre Linked-Node-Implementierung eine effiziente Manipulation ihres Anfangs und Durchlaufs durch rekursive Zerlegung ermöglicht.

- Vektoren hingegen sind für Szenarien optimiert, in denen wahlfreier Zugriff auf Elemente oder Aktualisierungen bei bestimmten Indizes erforderlich sind, wodurch sie besser für Anwendungsfälle wie Nachschlagetabellen, Konfigurationen mit fester Größe oder leistungskritische indizierte Vorgänge geeignet sind.

Im Wesentlichen sind Listen die natürliche Wahl für rekursive Algorithmen und Daten mit dynamischer Größe, während Vektoren glänzen, wenn es auf feste Größe oder indizierte Zugriffsmuster ankommt.

### Einfache Vektoren

```scheme
(vector 1 2 3)
```

- Erstellt einen Vektor aus drei Elementen: `1`, `2` und `3`.

Ergebnis: **`#(1 2 3)`**

#### Zugriff auf Vektorelemente

Auf Elemente in einem Vektor wird mit der Prozedur `vector-ref` zugegriffen. Sie ruft das Element an einem angegebenen Index ab (beginnend bei `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iteration: Verarbeiten jedes Elements in einem Vektor

Sie können einen Vektor mithilfe einer Schleife oder Rekursion durchlaufen. Das Schema stellt `vector-length` zur Verfügung, um die Größe eines Vektors zu bestimmen. Hier ist eine einfache Schleife, um jedes Element in einem Vektor zu drucken:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Basisfall:** Wenn der Index `i` die Länge des Vektors erreicht, stoppen Sie die Schleife.
- **Rekursiver Fall:** Drucken Sie das Element am Index `i` und erhöhen Sie dann `i`.

#### Beispielverwendung

```scheme
(print-elements (vector 1 2 3))
```

Ergebnis:

- `"1"`
- `"2"`
- `"3"`

Ergebnis: „erledigt“

### Gemischte Vektoren

Vektoren können Elemente unterschiedlichen Typs enthalten, darunter Zeichenfolgen, boolesche Werte, Zahlen, andere Vektoren oder sogar das Ergebnis von Ausdrücken:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Dadurch entsteht ein Vektor mit:
  - Eine Zahl (`42`)
  - Eine Zeichenfolge (`"hello"`)
  – Ein boolescher Wert (`#t`)
  - Ein weiterer Vektor (`#(1 2)`)
  – Das Ergebnis eines Ausdrucks (`(+ 3 4)`, der zu `7` ausgewertet wird)

Ergebnis: **`#(42 "hello" #t #(1 2) 7)`**

### Vektoren konstruieren

Vektoren werden mit `vector` oder mit `make-vector` erstellt, um einen Vektor mit fester Größe und einem Anfangswert zu erstellen.

```scheme
(make-vector 5 0)
```

Erstellt einen Vektor der Größe `5`, wobei alle Elemente auf `0` initialisiert sind.

Ergebnis: `#(0 0 0 0 0)`

### Vektoren aktualisieren

Die Prozedur `vector-set!` aktualisiert ein Element in einem Vektor an einem angegebenen Index.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Ergebnis: `#(1 42 3)`

### Nach Vektoren suchen

Die Prozedur `vector?` prüft, ob ein gegebener Wert ein Vektor ist.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Ergebnis:

- `(vector? (vector 1 2 3))` gibt `#t` zurück (wahr)
- `(vector? 42)` gibt `#f` zurück (falsch)

### Vektoren und Pass-by-Reference-VerhaltenIn Scheme sind Vektoren veränderbar und werden als Referenz übergeben. Das heißt, wenn Sie einen Vektor an eine Funktion übergeben, kann die Funktion den ursprünglichen Vektor direkt ändern. Alle am Vektor innerhalb der Funktion vorgenommenen Änderungen werden auch außerhalb der Funktion widergespiegelt. Dieses Verhalten ist nützlich für die effiziente gemeinsame Nutzung und Aktualisierung von Daten über mehrere Funktionen hinweg, erfordert aber auch Vorsicht, um unbeabsichtigte Nebenwirkungen zu vermeiden.

#### Beispiel: Ändern eines Vektors in einer Funktion

Hier ist ein Beispiel, das zeigt, wie Vektoren als Referenz übergeben und geändert werden:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Ergebnis: `#(10 99 30)`

#### Schritt-für-Schritt-Erklärung

1. **Erstellen Sie einen Vektor:** `my-vector` wird mit den Werten `10`, `20` und `30` initialisiert.
2. **Übergabe an eine Funktion:** `my-vector` wird zusammen mit dem Index und dem neuen zu aktualisierenden Wert an `modify-vector` übergeben.
3. **In Funktion ändern:** Die Prozedur `vector-set!` aktualisiert den Wert am angegebenen Index direkt im Originalvektor.
4. **Änderungen widerspiegeln:** Da Vektoren als Referenz übergeben werden, werden innerhalb der Funktion vorgenommene Änderungen im ursprünglichen Vektor widergespiegelt.

#### Implikationen von Pass-by-Reference

- **Leistung:** Die Übergabe von Vektoren als Referenz ist effizient, da dadurch das Kopieren großer Strukturen vermieden wird.
- **Nebenwirkungen:** Seien Sie vorsichtig, wenn Sie Vektoren funktionsübergreifend teilen, um unbeabsichtigte Änderungen an gemeinsam genutzten Daten zu vermeiden.

### Operationen auf Vektoren

Scheme bietet mehrere integrierte Verfahren zum Arbeiten mit Vektoren, darunter:

- `vector-length`: Gibt die Anzahl der Elemente in einem Vektor zurück.
- `vector->list`: Konvertiert einen Vektor in eine Liste.
- `list->vector`: Konvertiert eine Liste in einen Vektor.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Ergebnis:

- `(vector-length (vector 1 2 3))` gibt `3` zurück
- `(vector->list (vector 1 2 3))` gibt `(1 2 3)` zurück
- `(list->vector (list 1 2 3))` gibt `#(1 2 3)` zurück

### Verschachtelte Vektoren

Vektoren in Scheme können andere Vektoren als Elemente enthalten, wodurch eine verschachtelte Struktur entsteht.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Erstellt einen Vektor aus drei Elementen, von denen jedes selbst ein Vektor ist.

Ergebnis: **`#(#(1 2) #(3 4) #(5))`**

#### Zugriff auf verschachtelte Daten

Um auf Elemente innerhalb eines verschachtelten Vektors zuzugreifen, verwenden Sie mehrmals `vector-ref`, um durch die Struktur zu navigieren.

#### Beispiel: Auf Elemente zugreifen

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Zusammenfassung

- **Vektoren** in Scheme sind indizierte Datenstrukturen fester Größe.
- Verwenden Sie `vector` zum Erstellen eines Vektors, `vector-ref` zum Zugriff auf Elemente und `vector-set!` zum Aktualisieren von Elementen.
- Integrierte Prozeduren wie `vector-length`, `vector->list` und `list->vector` ermöglichen flexible Abläufe.
- Verschachtelte Vektoren ermöglichen komplexe, hierarchische Datenstrukturen.