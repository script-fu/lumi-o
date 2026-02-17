---
title: "kond"
type: docs
weight: 5
---
I Schema används `cond` för att välja ett av flera möjliga kodblock att exekvera, baserat på flera tester. Det är som en multi-gren `if`, där varje gren kontrolleras i ordning tills en matchning hittas.

### Syntax

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

– Varje prov utvärderas i den ordning de skrivs.
- När ett test utvärderas till sant (`#t`), exekveras dess motsvarande **konsekvens** och uttrycket `cond` slutar utvärdera ytterligare tester.
- Klausulen `else` är valfri och fungerar som en reserv om inget av testerna utvärderas till sant.

### Hur det fungerar

1. **Testa varje tillstånd**:
   - `cond` utvärderar testerna i den ordning de är listade.

2. **Utför matchningsföljden**:
   - När det första testet som utvärderas till sant (`#t`) hittas, exekveras dess **följande**.
   - Om inga test utvärderas till sant och det finns en `else`-sats, exekveras **fallback-consequent**.

### Exempel

#### Exempel 1: Enstaka uttryckskonsekvenser

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Det första testet `(< 3 2)` utvärderas till falskt (`#f`).
- Det andra testet `(= 3 3)` utvärderas till sant (`#t`), så `"This will run"` returneras.
- Klausulen `else` körs inte eftersom en matchning redan har hittats.

Resultat: **"Detta kommer att köras"**

#### Exempel 2: Flera åtgärder med `begin`

När en följd involverar flera åtgärder, använd `begin` för att gruppera dem:

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

- Det första testet `(< 5 3)` utvärderas till falskt (`#f`).
- Det andra testet `(> 5 3)` utvärderas till sant (`#t`):
  - Den skriver ut `"Condition met"`.
  - Sedan beräknar den `(* 5 5)` och returnerar `25`.

Resultat: **Skriver ut "Kondition uppfyllt" och returnerar 25.**

#### Exempel 3: Använda ett `let` block i en följd

När du behöver introducera lokala variabler, använd ett `let`-block:

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

- Det första testet `(< 0 -1)` är falskt.
- Det andra testet `(> 0 -1)` är sant, så:
  - Ett `let`-block exekveras, vilket binder `y` till `20`.
  - Den skriver ut `"Positive condition met"`.
  - Sedan beräknar den `(+ y y)` och returnerar `40`.

Resultat: **Skriver ut "Positivt villkor uppfyllt" och returnerar 40,**

#### Exempel 4: Fallback med `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Inget av de två första testerna bedöms vara sant.
- Klausulen `else` exekveras och returnerar `"Fallback value"`.

Resultat: **"Reservvärde"**

### Sammanfattning

- Använd `cond` för att hantera flera förhållanden på ett tydligt och kortfattat sätt.
- Konsekvenser kan vara enstaka uttryck eller grupperade åtgärder med `begin`.
- Använd `let` i följd för att deklarera lokala variabler för beräkningar.
- Inkludera alltid en `else`-klausul som en reserv för att hantera oväntade fall.

Denna flexibilitet gör `cond` till ett kraftfullt och läsbart verktyg för att hantera komplex förgreningslogik.