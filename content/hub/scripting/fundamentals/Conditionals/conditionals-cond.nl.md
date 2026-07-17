---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
In Scheme wordt het `cond`-conditionele gebruikt om een van meerdere mogelijke codeblokken te selecteren op basis van meerdere tests. Het is als een meerweg-`if`, waarbij elke tak in volgorde wordt gecontroleerd totdat er een match is.

### Syntaxis

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Elke test wordt geëvalueerd in de volgorde waarin ze zijn geschreven.
- Wanneer een test waar (`#t`) evalueert, wordt het bijbehorende **consequent** uitgevoerd en stopt de `cond`-expressie met het evalueren van verdere tests.
- De `else`-clausule is optioneel en dient als fallback als geen enkele test waar evalueert.

### Hoe het werkt

1. **Elke conditie testen:**
   - `cond` evalueert de tests in de opgegeven volgorde.

2. **Het bijbehorende consequent uitvoeren:**
   - Wanneer de eerste test die waar (`#t`) evalueert is gevonden, wordt het **consequent** uitgevoerd.
   - Als geen enkele test waar evalueert en er een `else`-clausule is, wordt het **fallback-consequent** uitgevoerd.

### Voorbeelden

#### Voorbeeld 1: consequenten met enkele expressie

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- De eerste test `(< 3 2)` evalueert onwaar (`#f`).
- De tweede test `(= 3 3)` evalueert waar (`#t`), dus `"This will run"` wordt geretourneerd.
- De `else`-clausule wordt niet uitgevoerd omdat er al een match is gevonden.

Resultaat: **"This will run"**

#### Voorbeeld 2: meerdere acties met `begin`

Wanneer een consequent meerdere acties omvat, gebruik `begin` om ze te groeperen:

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

- De eerste test `(< 5 3)` evalueert onwaar (`#f`).
- De tweede test `(> 5 3)` evalueert waar (`#t`):
  - Het drukt `"Condition met"` af.
  - Vervolgens berekent het `(* 5 5)` en retourneert `25`.

Resultaat: **Drukt "Condition met" af en retourneert 25.**

#### Voorbeeld 3: een `let`-blok in een consequent

Wanneer je lokale variabelen nodig hebt, gebruik een `let`-blok:

```scheme
(cond
  ;; Geval 1: Als 0 kleiner is dan -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Geval 2: als 0 groter is dan -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Standaardgeval: als geen van bovenstaande voorwaarden is voldaan
  (else
    (let ((z 0))
      z)))
```

- De eerste test `(< 0 -1)` is onwaar.
- De tweede test `(> 0 -1)` is waar, dus:
  - Een `let`-blok wordt uitgevoerd dat `y` bindt aan `20`.
  - Het drukt `"Positive condition met"` af.
  - Vervolgens berekent het `(+ y y)` en retourneert `40`.

Resultaat: **Drukt "Positive condition met" af en retourneert 40.**

#### Voorbeeld 4: fallback met `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Geen van de eerste twee tests evalueert waar.
- De `else`-clausule wordt uitgevoerd en retourneert `"Fallback value"`.

Resultaat: **"Fallback value"**

### Samenvatting

- Gebruik `cond` om meerdere condities op een heldere en beknopte manier af te handelen.
- Consequenten kunnen enkele expressies zijn of gegroepeerde acties met `begin`.
- Gebruik `let` in consequenten om lokale variabelen voor berekeningen te declareren.
- Neem altijd een `else`-clausule op als fallback voor onverwachte gevallen.

Deze flexibiliteit maakt `cond` tot een krachtig en leesbaar hulpmiddel voor complexe vertakkingslogica.
