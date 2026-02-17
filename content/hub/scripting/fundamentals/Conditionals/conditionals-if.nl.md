---
title: "als"
type: docs
weight: 4
---
In zijn eenvoudigste vorm evalueert de voorwaarde `if` in Scheme een test en voert op basis van het resultaat een van de twee mogelijke codeblokken uit. De eenvoudigste vorm ziet er als volgt uit:

```scheme
(if test-is-true
  do-this)
```

- Als de `test` evalueert naar true (`#t`), wordt het **codeblok in de consequentie** uitgevoerd. Het blok kan een waarde retourneren of andere acties uitvoeren, zoals het toewijzen van een variabele of het afdrukken van uitvoer.

### Voorbeeld

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- In dit geval is `test` `(< 0 1)` (controleren of 0 kleiner is dan 1).
- Aangezien de test resulteert in waar (`#t`), wordt het codeblok `(lumi-message "True!")` uitgevoerd, dat `"True!"` afdrukt.

### Een andere voorwaarde toevoegen: `if-else`

Wanneer u een voorwaardelijk `if` gebruikt met een alternatief codeblok (het geval `else`), ziet de structuur er als volgt uit:

```scheme
(if test
  do-this
  else-do-this)
```

- Als `test` resulteert in true (`#t`), wordt het **consequente** codeblok uitgevoerd.
- Als `test` onwaar is (`#f`), wordt het **alternatieve** codeblok uitgevoerd.

```scheme
(if test
  consequent
  alternative)
```

### Hoe het werkt

1. **Testexpressie**:
   - De `test` expressie wordt eerst geëvalueerd.

2. **Resultaat gebaseerd op test**:
   - Als `test` resulteert in true (`#t`), wordt het **consequente codeblok** uitgevoerd.
   - Als `test` onwaar is (`#f`), wordt het **alternatieve codeblok** uitgevoerd.

Zowel de codeblokken `consequent` als `alternative` kunnen elke geldige Scheme-bewerking uitvoeren, inclusief het retourneren van waarden, het wijzigen van variabelen of het uitvoeren van procedures.

### Voorbeelden

#### Voorbeeld 1: Een waarde retourneren

```scheme
(if (< 0 1)
  1
  0)
```

- Hier is `test` `(< 0 1)` (controleert of 0 kleiner is dan 1).
- Aangezien de test resulteert in waar (`#t`), wordt het **consequent** blok (`1`) uitgevoerd en wordt de waarde ervan geretourneerd.

Resultaat: **1**

#### Voorbeeld 2: Een beginblok evalueren

In gevallen waarin u meerdere acties moet uitvoeren wanneer de voorwaarde waar of onwaar is, kunt u `begin` of een `let` gebruiken om ze te groeperen.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- In dit voorbeeld is `test` `(= 0 1)` (er wordt gecontroleerd of 0 gelijk is aan 1).
- Omdat de test false oplevert (`#f`), wordt het **alternatieve** blok uitgevoerd:
  - Eerst wordt `"False condition met, calculating..."` afgedrukt.
  - Vervolgens berekent het `(* 3 4)` en retourneert `12`.

Resultaat: **Afgedrukt "Aan valse voorwaarde voldaan, berekening..." en retourneert 12.**

#### Voorbeeld 3: Een verhuurverklaring evalueren

Door een `let` te gebruiken, kunnen we lokale scopevariabelen binnen het codeblok declareren.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- In dit voorbeeld is `test` `(= 1 1)` (er wordt gecontroleerd of 1 gelijk is aan 1).
- Aangezien de test resulteert in waar (`#t`), wordt het **consequent** blok uitgevoerd:
  - Eerst wordt `"True condition met, calculating..."` afgedrukt.
  - Vervolgens berekent het `(* -1 10)` en retourneert `-10`.

Resultaat: **Er wordt afgedrukt "Aan ware voorwaarde voldaan, berekening..." en retourneert -10.**

### Samenvatting- De voorwaarde `if` is een krachtig hulpmiddel in Scheme voor het evalueren van tests en het uitvoeren van overeenkomstige codeblokken.
- Het kan zowel eenvoudige expressies als complexe codeblokken verwerken die waarden retourneren, variabelen wijzigen of bijwerkingen veroorzaken.
- Onthoud: als er geen expliciet `else` blok is, evalueert en voert `if` alleen het **gevolg** uit als de test waar is. Anders wordt het **alternatief** geëvalueerd en uitgevoerd.