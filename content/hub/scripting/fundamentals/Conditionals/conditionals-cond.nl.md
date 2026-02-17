---
title: "Cond"
type: docs
weight: 5
---
In Schema wordt de voorwaarde `cond` gebruikt voor het selecteren van een van de verschillende mogelijke codeblokken om uit te voeren, op basis van meerdere tests. Het is als een multi-filiaal `if`, waarbij elke tak op volgorde wordt gecontroleerd totdat er een match is gevonden.

### Syntaxis

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Elke test wordt geëvalueerd in de volgorde waarin deze is geschreven.
- Wanneer een test resulteert in true (`#t`), wordt het bijbehorende **consequent** uitgevoerd en stopt de `cond` expressie met het evalueren van verdere tests.
- De clausule `else` is optioneel en dient als reserve als geen van de tests waar is.

### Hoe het werkt

1. **Elke voorwaarde testen**:
   - `cond` evalueert de tests in de volgorde waarin ze worden vermeld.

2. **Voer het overeenkomende gevolg uit**:
   - Wanneer de eerste test die resulteert in waar (`#t`) wordt gevonden, wordt het **consequent** ervan uitgevoerd.
   - Als geen enkele test de uitkomst True oplevert en er een `else`-clausule is, wordt de **fallback-consequent** uitgevoerd.

### Voorbeelden

#### Voorbeeld 1: Gevolgen van een enkele expressie

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- De eerste test `(< 3 2)` resulteert in false (`#f`).
- De tweede test `(= 3 3)` resulteert in true (`#t`), dus `"This will run"` wordt geretourneerd.
- De clausule `else` wordt niet uitgevoerd omdat er al een overeenkomst is gevonden.

Resultaat: **"Dit wordt uitgevoerd"**

#### Voorbeeld 2: Meerdere acties met `begin`

Als een gevolg meerdere acties omvat, gebruik dan `begin` om ze te groeperen:

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

- De eerste test `(< 5 3)` resulteert in false (`#f`).
- De tweede test `(> 5 3)` resulteert in waar (`#t`):
  - Er wordt `"Condition met"` afgedrukt.
  - Vervolgens berekent het `(* 5 5)` en retourneert `25`.

Resultaat: **Afgedrukt "Aan voorwaarde voldaan" en retourneert 25.**

#### Voorbeeld 3: Een `let` blok als gevolg gebruiken

Wanneer u lokale variabelen moet invoeren, gebruikt u een `let` blok:

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

- De eerste test `(< 0 -1)` is onwaar.
- De tweede test `(> 0 -1)` is waar, dus:
  - Er wordt een `let` blok uitgevoerd, waarbij `y` wordt gekoppeld aan `20`.
  - Er wordt `"Positive condition met"` afgedrukt.
  - Vervolgens berekent het `(+ y y)` en retourneert `40`.

Resultaat: **Er wordt 'Aan positieve voorwaarde voldaan' afgedrukt en retourneert 40.**

#### Voorbeeld 4: Terugval met `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Geen van de eerste twee tests levert waar op.
- De clausule `else` wordt uitgevoerd en retourneert `"Fallback value"`.

Resultaat: **"Fallback-waarde"**

### Samenvatting

- Gebruik `cond` om meerdere voorwaarden op een duidelijke en beknopte manier af te handelen.
- Gevolgen kunnen afzonderlijke uitdrukkingen zijn of gegroepeerde acties met `begin`.
- Gebruik `let` in consequents om lokale variabelen voor berekeningen te declareren.
- Neem altijd een `else`-clausule op als reserve voor het afhandelen van onverwachte gevallen.

Deze flexibiliteit maakt `cond` een krachtig en leesbaar hulpmiddel voor het verwerken van complexe vertakkingslogica.