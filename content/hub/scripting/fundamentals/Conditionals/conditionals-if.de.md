---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a31916ea815a99deebce805ed2023a7bedbf63325938649cebdd80e7eba209ee
---
In seiner einfachsten Form wertet `if` in Scheme einen Test aus und führt je nach Ergebnis einen von zwei Codeblöcken aus:

```scheme
(if test-is-true
  do-this)
```

- Bei `#t` wird der **consequent** ausgeführt (Wert zurückgeben oder Nebeneffekte).

### Beispiel

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Test: `(< 0 1)` ist wahr.
- `(lumi-message "True!")` wird ausgeführt.

### Else-Zweig: `if-else`

```scheme
(if test
  do-this
  else-do-this)
```

```scheme
(if test
  consequent
  alternative)
```

### So funktioniert es

1. **Test** zuerst auswerten.
2. Bei `#t` **consequent**, bei `#f` **alternative**.

Beide Blöcke können jeden gültigen Scheme-Ausdruck enthalten.

#### Beispiel 1: Wert zurückgeben

```scheme
(if (< 0 1)
  1
  0)
```

Ergebnis: **1**

#### Beispiel 2: `begin`-Block

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

Ergebnis: **Gibt "False condition met, calculating..." aus und liefert 12.**

#### Beispiel 3: `let`-Ausdruck

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

Ergebnis: **Gibt "True condition met, calculating..." aus und liefert -10.**

### Zusammenfassung

- `if` wertet Tests aus und führt passende Blöcke aus.
- Einfache Ausdrücke oder `begin`/`let`-Gruppen möglich.
- Ohne explizites `else` nur **consequent** bei wahrem Test.