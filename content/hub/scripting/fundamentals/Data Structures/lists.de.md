---
title: "Listen"
type: docs
weight: 4
---
In Scheme ist eine **Liste** eine grundlegende Datenstruktur, die zum Gruppieren von Werten verwendet wird. Listen sind geordnete Sammlungen von Elementen, wobei jedes Element einen beliebigen Typ haben kann, einschließlich einer anderen Liste. Listen werden in Scheme häufig sowohl zur Datenspeicherung als auch zur Programmstruktur verwendet.

### Beispiel 1: Einfache Liste

```scheme
(list 1 2 3)
```

- Erstellt eine Liste mit drei Elementen: `1`, `2` und `3`.

Ergebnis: **`(1 2 3)`**

---

#### Zugriff auf Listenelemente

Auf Elemente in einer Liste wird mit den Prozeduren `car` und `cdr` zugegriffen:

- `car` ruft das erste Element einer Liste ab.
- `cdr` ruft den Rest der Liste ab (alles außer dem ersten Element).

#### Beispiele

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Ergebnis:

- `(car my-list)` gibt `1` zurück
- `(cdr my-list)` gibt `(2 3)` zurück

---

#### Einfache Rekursion: Durch eine Liste iterieren

Durch den rekursiven Aufruf von `car` auf dem `cdr` einer Liste können Sie jedes Element einzeln verarbeiten, bis die Liste durchlaufen ist. Dies bildet die Grundlage vieler Listenverarbeitungsalgorithmen.

#### Beispiel: Jedes Element einer Liste drucken

Hier ist eine einfache rekursive Funktion zum Drucken jedes Elements in einer Liste:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Basisfall:** Wenn die Liste leer ist (`null? lst`), stoppen Sie die Rekursion.
- **Rekursiver Fall:** Drucken Sie das erste Element (`car lst`) und rufen Sie dann die Funktion für den Rest der Liste auf (`cdr lst`).

#### Beispielverwendung

```scheme
(print-elements (list 1 2 3))
```

Ausgabe:

- `"1"`
- `"2"`
- `"3"`

Ergebnis: „erledigt“

---

#### Wie es funktioniert

1. Die Funktion ruft mit `car` das erste Element der Liste ab und verarbeitet es.
2. Anschließend ruft es sich selbst mit dem Rest der Liste auf (`cdr`).
3. Dieser Vorgang wiederholt sich, bis die Liste leer ist (`null? lst`).

---

### Beispiel 2: Gemischte Typen

Listen können Elemente unterschiedlichen Typs enthalten, darunter Zeichenfolgen, boolesche Werte, Zahlen, andere Listen oder sogar das Ergebnis von Ausdrücken:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Dadurch wird eine Liste erstellt mit:
  - Eine Zahl (`42`)
  - Eine Zeichenfolge (`"hello"`)
  – Ein boolescher Wert (`#t`)
  - Eine weitere Liste (`(1 2)`)
  – Das Ergebnis eines Ausdrucks (`(+ 3 4)`, der zu `7` ausgewertet wird)

Ergebnis: **`(42 "hello" #t (1 2) 7)`**

---

Diese Beispiele veranschaulichen die Vielseitigkeit von Listen in Scheme und machen sie zu einem leistungsstarken Werkzeug zum Organisieren und Bearbeiten von Daten.

### Listen erstellen

Die Prozedur `cons` wird verwendet, um eine neue Liste zu erstellen, indem ein Element mit einer vorhandenen Liste kombiniert wird.

```scheme
(cons new-element existing-list)
```

#### Beispiel

```scheme
(cons 0 (list 1 2 3))
```

- Fügt `0` am Anfang der Liste `(1 2 3)` hinzu.

Ergebnis: **`(0 1 2 3)`**

---

### Nach Listen suchen

Die Prozedur `list?` prüft, ob ein gegebener Wert eine Liste ist.

```scheme
(list? value)
```

#### Beispiel: Liste?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Ergebnis:

- `(list? (list 1 2 3))` gibt `#t` zurück (wahr)
- `(list? 42)` gibt `#f` zurück (falsch)

---

### Operationen auf Listen

Scheme bietet mehrere integrierte Verfahren zum Arbeiten mit Listen, darunter:

- `length`: Gibt die Anzahl der Elemente in einer Liste zurück.
- `append`: Kombiniert zwei oder mehr Listen zu einer.
- `reverse`: Gibt eine neue Liste mit Elementen in umgekehrter Reihenfolge zurück.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Ergebnis:

- `(length (list 1 2 3))` gibt `3` zurück
- `(append (list 1 2) (list 3 4))` gibt `(1 2 3 4)` zurück
- `(reverse (list 1 2 3))` gibt `(3 2 1)` zurück#### Verwendung von `list-ref`

Die Prozedur `list-ref` ruft das Element an einem angegebenen Index einer Liste ab (nullbasierter Index).

```scheme
(list-ref lst index)
```

- **`lst`**: Die Liste, aus der das Element abgerufen werden soll.
- **`index`**: Ein nullbasierter Index, der angibt, welches Element zurückgegeben werden soll.

##### Beispiel: Listenreferenz

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Ergebnis: `30`

---

### Verschachtelte Listen

Listen in Scheme können andere Listen als Elemente enthalten, wodurch eine verschachtelte Struktur entsteht.

#### Beispiel: Erstellen einer verschachtelten Liste

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Erstellt eine Liste mit drei Elementen, von denen jedes selbst eine Liste ist.

Ergebnis: **`((1 2) (3 4) (5))`**

---

#### Zugriff auf verschachtelte Daten

Um auf Elemente innerhalb einer verschachtelten Liste zuzugreifen, können Sie Kombinationen aus `car` und `cdr` verwenden, um durch die Struktur zu navigieren.

#### Beispiel: Auf Elemente zugreifen

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Erklärung

1. **`car nested-list`**:
   – Ruft das erste Element von `nested-list` ab, nämlich `(1 2)`.

2. **`car (car nested-list)`**:
   – Ruft das erste Element von `(1 2)` ab, nämlich `1`.

3. **`cdr (car nested-list)`**:
   – Ruft den Rest von `(1 2)` ab, also `(2)`.

4. **`car (cdr (car nested-list))`**:
   – Ruft das erste Element von `(2)` ab, nämlich `2`.

---

#### Beispiel: Zugriff auf Elemente aus anderen Unterlisten

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Dieser Ansatz ermöglicht Ihnen die systematische Navigation und den Zugriff auf bestimmte Elemente in einer verschachtelten Liste und bietet so eine leistungsstarke Flexibilität für die Arbeit mit hierarchischen Daten.

### Zusammenfassung

- **Listen** in Scheme sind vielseitige und wesentliche Datenstrukturen.
- Verwenden Sie `list` zum Erstellen einer Liste, `car` und `cdr` für den Zugriff auf Elemente und `cons` zum Erstellen von Listen.
- Integrierte Prozeduren wie `length`, `append`, `reverse` und `list-ref` machen Listenoperationen einfach und effizient.
- Listen können verschachtelt werden, was komplexe Datenstrukturen für erweiterte Anwendungsfälle ermöglicht.