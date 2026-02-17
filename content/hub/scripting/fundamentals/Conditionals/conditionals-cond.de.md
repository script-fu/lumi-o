---
title: "kond"
type: docs
weight: 5
---
In Scheme wird die Bedingung `cond` verwendet, um basierend auf mehreren Tests einen von mehreren möglichen Codeblöcken auszuwählen, die ausgeführt werden sollen. Es ist wie ein `if` mit mehreren Zweigen, bei dem jeder Zweig der Reihe nach überprüft wird, bis eine Übereinstimmung gefunden wird.

### Syntax

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Jeder Test wird in der Reihenfolge ausgewertet, in der er geschrieben wurde.
– Wenn ein Test „true“ ergibt (`#t`), wird seine entsprechende **Konsequenz** ausgeführt und der Ausdruck `cond` stoppt die Auswertung weiterer Tests.
– Die `else`-Klausel ist optional und dient als Fallback, wenn keiner der Tests „true“ ergibt.

### Wie es funktioniert

1. **Testen Sie jede Bedingung**:
   - `cond` wertet die Tests in der Reihenfolge aus, in der sie aufgelistet sind.

2. **Führen Sie die Matching-Konsequenz aus**:
   – Wenn der erste Test gefunden wird, der „true“ ergibt (`#t`), wird seine **Konsequenz** ausgeführt.
   – Wenn keine Tests „true“ ergeben und eine `else`-Klausel vorhanden ist, wird die **Fallback-Konsequenz** ausgeführt.

### Beispiele

#### Beispiel 1: Konsequenzen einzelner Ausdrücke

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

– Der erste Test `(< 3 2)` ergibt „falsch“ (`#f`).
– Der zweite Test `(= 3 3)` ergibt „true“ (`#t`), daher wird `"This will run"` zurückgegeben.
– Die Klausel `else` wird nicht ausgeführt, da bereits eine Übereinstimmung gefunden wurde.

Ergebnis: **"Dies wird ausgeführt"**

#### Beispiel 2: Mehrere Aktionen mit `begin`

Wenn eine Konsequenz mehrere Aktionen umfasst, verwenden Sie `begin`, um sie zu gruppieren:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

– Der erste Test `(< 5 3)` ergibt „falsch“ (`#f`).
- Der zweite Test `(> 5 3)` ergibt wahr (`#t`):
  - Es wird `"Condition met"` gedruckt.
  - Dann berechnet es `(* 5 5)` und gibt `25` zurück.

Ergebnis: **Gibt „Bedingung erfüllt“ aus und gibt 25 zurück.**

#### Beispiel 3: Verwendung eines `let` Blocks in einer Konsequenz

Wenn Sie lokale Variablen einführen müssen, verwenden Sie einen `let` Block:

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- Der erste Test `(< 0 -1)` ist falsch.
- Der zweite Test `(> 0 -1)` ist wahr, also:
  - Ein `let` Block wird ausgeführt, der `y` an `20` bindet.
  - Es wird `"Positive condition met"` gedruckt.
  - Dann berechnet es `(+ y y)` und gibt `40` zurück.

Ergebnis: **Gibt „Positive Bedingung erfüllt“ aus und gibt 40 zurück.**

#### Beispiel 4: Fallback mit `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Keiner der ersten beiden Tests ergibt „true“.
- Die Klausel `else` wird ausgeführt und gibt `"Fallback value"` zurück.

Ergebnis: **"Fallback-Wert"**

### Zusammenfassung

- Verwenden Sie `cond`, um mehrere Bedingungen klar und prägnant zu behandeln.
- Konsequenzen können einzelne Ausdrücke oder gruppierte Aktionen mit `begin` sein.
- Verwenden Sie `let` in Folge, um lokale Variablen für Berechnungen zu deklarieren.
– Fügen Sie immer eine `else`-Klausel als Fallback zur Behandlung unerwarteter Fälle ein.

Diese Flexibilität macht `cond` zu einem leistungsstarken und lesbaren Werkzeug für die Handhabung komplexer Verzweigungslogik.