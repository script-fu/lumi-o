---
title: "Wann"
type: docs
weight: 5
---
Obwohl `if` in Scheme elegant und vielseitig ist, kann es verwirrend werden, wenn es ohne explizites `else` verwendet wird. Dies gilt insbesondere dann, wenn die Absicht besteht, einen einzelnen Codezweig nur dann auszuführen, wenn eine Bedingung wahr ist, ohne alternative Aktion für den Fall `false`. In solchen Szenarien bietet das Konstrukt `when` eine klarere und prägnantere Alternative.

Die Grundform von `when` sieht so aus:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Wenn `test` als wahr ausgewertet wird (`#t`), werden alle Ausdrücke im Hauptteil des `when`-Konstrukts nacheinander ausgeführt.
– Wenn `test` als falsch ausgewertet wird (`#f`), passiert nichts und es werden keine Werte zurückgegeben.

### Beispiel

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Gegenüberstellung von `if` und `when`

Um den Unterschied zwischen `if` und `when` besser zu verstehen, betrachten Sie das folgende Beispiel, in dem beide zusammen verwendet werden:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Erklärung:

1. **Die `if` Bedingung**:
   - Der Test `(= 0 1)` prüft, ob 0 gleich 1 ist.
   - Da dies falsch ist (`#f`), wird der `else` Zweig des `if` ausgeführt.

2. **Das `when` Konstrukt im `else` Zweig**:
   - Der `when` Test `(< 0 1)` prüft, ob 0 kleiner als 1 ist.
   – Da dies wahr ist (`#t`), werden alle Ausdrücke im Hauptteil des `when` nacheinander ausgeführt:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Warum hier `when` verwenden?

– Die Verwendung von `when` anstelle eines anderen `if` vereinfacht die Logik, wenn für die Bedingung kein expliziter `else` Zweig erforderlich ist.
- `when` macht deutlich, dass nur der wahre Zweig relevant ist, wodurch mögliche Verwirrung vermieden wird.

### Zusammenfassung

- Verwenden Sie `if`, wenn Sie sowohl einen wahren als auch einen falschen Zweig benötigen.
- Verwenden Sie `when`, wenn es nur einen einzigen Zweig für den wahren Fall gibt, insbesondere wenn mehrere Aktionen ausgeführt werden müssen.
- Die Kombination von `if` und `when` kann dabei helfen, komplexere Bedingungen klar und prägnant zu strukturieren.