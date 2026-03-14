---
title: "Funktionen"
type: docs
weight: 7
---
Funktionen sind ein Kernkonzept in Scheme und bieten die Möglichkeit, Logik zu kapseln, die Wiederverwendung von Code zu ermöglichen und Ihre Skripte effektiv zu strukturieren. Mit Funktionen können Sie modulare, wartbare Skripte erstellen, die ein breites Aufgabenspektrum abdecken, von grundlegenden Vorgängen bis hin zu erweiterten Arbeitsabläufen in Lumi.

Dieser Abschnitt dient als Einführung in die Funktionen in Scheme und legt die Grundlage für das Verständnis ihrer Typen, Definitionen und Verwendungen. In den folgenden Abschnitten wird tiefer auf bestimmte Funktionstypen und ihre einzigartigen Fähigkeiten eingegangen.

## Minimale Syntax und Ausdrücke

Der Schemacode besteht aus **Ausdrücken**. Ein Ausdruck ergibt einen Wert. Die Syntax ist einheitlich: Klammern bilden einen Aufruf, wobei der Operator- oder Funktionsname zuerst steht.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Da alles ein Ausdruck ist, passt der Kontrollfluss natürlich in denselben Stil wie Funktionsaufrufe.

## Warum Funktionen wichtig sind

Funktionen spielen in Scheme aus mehreren Gründen eine zentrale Rolle:

- **Wiederverwendbarkeit des Codes:** Vermeiden Sie Wiederholungen, indem Sie die Logik in wiederverwendbare Komponenten kapseln.
- **Modularität:** Teilen Sie komplexe Aufgaben in kleinere, überschaubare Teile auf.
- **Dynamisches Verhalten:** Akzeptieren Sie Parameter, um verschiedene Eingaben zu verarbeiten oder sich an verschiedene Situationen anzupassen.
- **Höhere Abstraktion:** Vereinfachen Sie die Logik, indem Sie sich darauf konzentrieren, „was“ eine Funktion tut, und nicht darauf, „wie“ sie es tut.

## Übersicht über Funktionstypen

Scheme bietet eine Vielzahl von Funktionskonstrukten, die jeweils für bestimmte Anwendungsfälle geeignet sind:

1. **Benannte Funktionen**
   Dies sind Standardfunktionen, die mit `define` definiert sind. Sie bilden das Rückgrat der meisten Skripte.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Anonyme Funktionen**
   Auch als **Lambda-Funktionen** bekannt, handelt es sich hierbei um unbenannte Funktionen, die inline für den einmaligen Gebrauch definiert sind.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Funktionen höherer Ordnung**
   Funktionen, die andere Funktionen als Argumente verwenden oder Funktionen als Ergebnisse zurückgeben und so leistungsstarke Abstraktionen wie Zuordnen, Filtern und Reduzieren ermöglichen.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Allgemeine Syntax für Funktionen

Funktionen in Scheme haben eine einfache und konsistente Syntax:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Der Name der Funktion.
- **`parameter1, parameter2, ...`:** Die Argumente, die die Funktion annimmt.
- **`body-expression`:** Die Logik, die ausgeführt wird, wenn die Funktion aufgerufen wird.

Beispiel:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Nebenwirkungen und globaler Zustand

In Lumi haben viele nützliche Prozeduren **Nebeneffekte**: Sie ändern ein Bild, ändern ein Zeichenelement, schreiben eine Datei oder zeigen die Ausgabe an.

- Isolieren Sie Nebenwirkungen in kleinen, klar benannten Verfahren.
- Vermeiden Sie es, den globalen Kontext zu ändern, es sei denn, dies ist erforderlich.
- Wenn Sie den Kontext ändern (Farben, Pinsel usw.), schließen Sie die Arbeit mit `lumi-context-push` und `lumi-context-pop` ein, damit der Status des Benutzers wiederhergestellt wird.