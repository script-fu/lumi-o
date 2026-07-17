---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
I Scheme används villkoret `cond` för att välja ett av flera möjliga kodblock att köra, baserat på flera test. Det liknar ett flergrenigt `if`, där varje gren kontrolleras i ordning tills en match hittas.

### Syntax

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Varje test utvärderas i den ordning de skrivs.
- När ett test utvärderas till sant (`#t`) körs motsvarande **consequent** och `cond` slutar utvärdera fler test.
- Klausulen `else` är valfri och fungerar som reserv om inget test är sant.

### Så fungerar det

1. **Testa varje villkor:**
   - `cond` utvärderar testen i listordning.

2. **Kör matchande consequent:**
   - När det första testet som utvärderas till sant (`#t`) hittas körs dess **consequent**.
   - Om inget test är sant och det finns `else` körs **fallback-consequent**.

### Exempel

#### Exempel 1: consequent med enstaka uttryck

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Första testet `(< 3 2)` utvärderas till falskt (`#f`).
- Andra testet `(= 3 3)` utvärderas till sant (`#t`), så `"This will run"` returneras.
- `else` körs inte eftersom en match redan hittades.

Resultat: **"This will run"**

#### Exempel 2: flera åtgärder med `begin`

När consequent innehåller flera åtgärder, gruppera dem med `begin`:

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

- Första testet `(< 5 3)` utvärderas till falskt (`#f`).
- Andra testet `(> 5 3)` utvärderas till sant (`#t`):
  - Det skriver ut `"Condition met"`.
  - Sedan beräknas `(* 5 5)` och `25` returneras.

Resultat: **Skriver ut "Condition met" och returnerar 25.**

#### Exempel 3: `let`-block i consequent

När du behöver lokala variabler, använd `let`:

```scheme
(cond
  ;; Fall 1: om 0 är mindre än -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Fall 2: om 0 är större än -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Standardfall
  (else
    (let ((z 0))
      z)))
```

- Första testet `(< 0 -1)` är falskt.
- Andra testet `(> 0 -1)` är sant, så:
  - Ett `let`-block körs och binder `y` till `20`.
  - Det skriver ut `"Positive condition met"`.
  - Sedan beräknas `(+ y y)` och `40` returneras.

Resultat: **Skriver ut "Positive condition met" och returnerar 40.**

#### Exempel 4: reserv med `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Varken av de två första testen utvärderas till sant.
- `else` körs och returnerar `"Fallback value"`.

Resultat: **"Fallback value"**

### Sammanfattning

- Använd `cond` för att hantera flera villkor på ett tydligt och koncist sätt.
- Consequent kan vara enstaka uttryck eller grupperade åtgärder med `begin`.
- Använd `let` i consequent för lokala variabler vid beräkningar.
- Inkludera alltid `else` som reserv för oväntade fall.

Denna flexibilitet gör `cond` till ett kraftfullt och läsbart verktyg för komplex förgreningslogik.
