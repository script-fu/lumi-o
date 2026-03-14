---
title: "Assoziationslisten (Alisten)"
type: docs
weight: 6
---
Eine **Assoziationsliste** (oder **Alist**) ist eine grundlegende Datenstruktur in Scheme, die zur Darstellung von Sammlungen von Schlüssel-Wert-Paaren verwendet wird. Es wird als Liste von Paaren implementiert, wobei jedes Paar einen Schlüssel (normalerweise ein Symbol) einem Wert zuordnet. Alisten sind einfach, flexibel und eignen sich gut für kleine bis mittelgroße Datensätze.

### Aufbau einer Assoziationsliste

Eine Liste ist eine Liste, in der jedes Element ein **Paar** ist (konstruiert mit `cons`). Jedes Paar besteht aus:

- **Schlüssel**: Das erste Element (normalerweise ein Symbol).
- **Wert**: Das zweite Element, das einen beliebigen Datentyp haben kann.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Schlüssel**: `'name`, `'age`, `'city`
- **Wert**: `"Alice"`, `30`, `"Paris"`
- **Struktur**: Eine Liste von Paaren:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Erstellen einer Alist

Sie können eine Liste erstellen, indem Sie manuell Paare erstellen oder sie programmgesteuert mit `cons` erstellen.

#### Verwendung des einfachen Anführungszeichens (`'`)

Das einfache Anführungszeichen (`'`) ist eine Abkürzung für **quoting**, was Scheme daran hindert, den Ausdruck auszuwerten. Dies macht es ideal für die Erstellung statischer Listen, bei denen alle Schlüssel und Werte fest codiert sind.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Ergebnis**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Verwendung des Backquotes („` ` ``) and Comma (`,`)

Der Backquote-Operator („` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,“). Dies ist nützlich zum Erstellen von Listen, bei denen Schlüssel oder Werte zur Laufzeit berechnet werden.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Ergebnis**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Beispielvergleich

Statische Liste mit `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Dynamische Liste mit `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Zugriff auf Daten in einer Alist

Um einen Wert aus einer Liste abzurufen, können Sie die Funktion `assoc` verwenden, die ein Paar anhand seines Schlüssels sucht.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Den Wert extrahieren

Sobald Sie ein Paar mit `assoc` abgerufen haben, verwenden Sie `cdr`, um den Wert zu extrahieren:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Zusammenfassung der wichtigsten Funktionen

- **Einfaches Anführungszeichen (`'`)**: Erstellt eine statische Liste, in der alle Elemente Literaldaten sind.
- **Backquote (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Punktnotation (`.`)**: Wird zum Erstellen von Paaren verwendet, um einen Schlüssel einem Wert in einer Liste zuzuordnen.