---
title: "Wenn"
type: docs
weight: 4
---
In seiner einfachsten Form wertet die Bedingung `if` in Scheme einen Test aus und führt basierend auf dem Ergebnis einen von zwei möglichen Codeblöcken aus. Die einfachste Form sieht so aus:

```scheme
(if test-is-true
  do-this)
```

– Wenn das `test` als wahr ausgewertet wird (`#t`), wird der **Codeblock in der Folge** ausgeführt. Der Block kann einen Wert zurückgeben oder andere Aktionen ausführen, z. B. das Zuweisen einer Variablen oder das Drucken von Ausgaben.

### Beispiel

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- In diesem Fall ist `test` `(< 0 1)` (überprüft, ob 0 kleiner als 1 ist).
- Da der Test „true“ ergibt (`#t`), wird der Codeblock `(lumi-message "True!")` ausgeführt, der `"True!"` ausgibt.

### Hinzufügen einer Else-Bedingung: `if-else`

Bei Verwendung einer `if`-Bedingung mit einem alternativen Codeblock (der `else`-Fall) sieht die Struktur wie folgt aus:

```scheme
(if test
  do-this
  else-do-this)
```

– Wenn das `test` als wahr ausgewertet wird (`#t`), wird der **konsequente** Codeblock ausgeführt.
– Wenn `test` als falsch ausgewertet wird (`#f`), wird der **alternative** Codeblock ausgeführt.

```scheme
(if test
  consequent
  alternative)
```

### Wie es funktioniert

1. **Testausdruck**:
   - Der Ausdruck `test` wird zuerst ausgewertet.

2. **Ergebnis basierend auf Test**:
   – Wenn das `test` als wahr ausgewertet wird (`#t`), wird der **konsequente Codeblock** ausgeführt.
   - Wenn `test` als falsch ausgewertet wird (`#f`), wird der **alternative Codeblock** ausgeführt.

Sowohl die Codeblöcke `consequent` als auch `alternative` können jede gültige Scheme-Operation ausführen, einschließlich der Rückgabe von Werten, der Änderung von Variablen oder der Ausführung von Prozeduren.

### Beispiele

#### Beispiel 1: Einen Wert zurückgeben

```scheme
(if (< 0 1)
  1
  0)
```

- Hier ist `test` `(< 0 1)` (überprüft, ob 0 kleiner als 1 ist).
- Da der Test „true“ ergibt (`#t`), wird der **konsequente** Block (`1`) ausgeführt und sein Wert zurückgegeben.

Ergebnis: **1**

#### Beispiel 2: Auswertung eines begin-Blocks

In Fällen, in denen Sie mehrere Aktionen ausführen müssen, wenn die Bedingung wahr oder falsch ist, können Sie `begin` oder `let` verwenden, um sie zu gruppieren.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- In diesem Beispiel ist `test` `(= 0 1)` (überprüft, ob 0 gleich 1 ist).
– Da der Test „falsch“ ergibt (`#f`), wird der **alternative** Block ausgeführt:
  - Zuerst wird `"False condition met, calculating..."` gedruckt.
  - Anschließend wird `(* 3 4)` berechnet und `12` zurückgegeben.

Ergebnis: **Gibt „Falsche Bedingung erfüllt, Berechnung...“ aus und gibt 12 zurück.**

#### Beispiel 3: Auswertung einer let-Anweisung

Durch die Verwendung eines `let` können wir lokale Bereichsvariablen innerhalb des Codeblocks deklarieren.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- In diesem Beispiel ist `test` `(= 1 1)` (überprüft, ob 1 gleich 1 ist).
– Da der Test „true“ ergibt (`#t`), wird der **konsequente** Block ausgeführt:
  - Zuerst wird `"True condition met, calculating..."` gedruckt.
  - Anschließend wird `(* -1 10)` berechnet und `-10` zurückgegeben.

Ergebnis: **Gibt „Wahre Bedingung erfüllt, Berechnung...“ aus und gibt -10 zurück.**

### Zusammenfassung- Die Bedingung `if` ist ein leistungsstarkes Tool in Scheme zum Auswerten von Tests und zum Ausführen entsprechender Codeblöcke.
– Es kann sowohl einfache Ausdrücke als auch komplexe Codeblöcke verarbeiten, die Werte zurückgeben, Variablen ändern oder Nebenwirkungen ausführen.
- Denken Sie daran: Wenn es keinen expliziten `else`-Block gibt, wertet der `if` die **Konsequenz** nur aus und führt sie aus, wenn der Test wahr ist. Andernfalls wertet es die **Alternative** aus und führt sie aus.