---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
In zijn eenvoudigste vorm evalueert het `if`-conditionele in Scheme een test en voert het, op basis van het resultaat, een van twee mogelijke codeblokken uit. De eenvoudigste vorm ziet er zo uit:

```scheme
(if test-is-true
  do-this)
```

- Als de `test` waar (`#t`) evalueert, wordt het **consequent-blok** uitgevoerd. Het blok kan een waarde retourneren of andere acties uitvoeren, zoals een variabele toewijzen of output afdrukken.

### Voorbeeld

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- In dit geval is de `test` `(< 0 1)` (controle of 0 kleiner is dan 1).
- Omdat de test waar (`#t`) evalueert, wordt het codeblok `(lumi-message "True!")` uitgevoerd, dat `"True!"` afdrukt.

### Een else-voorwaarde toevoegen: `if-else`

Wanneer een `if`-conditionele een alternatief codeblok heeft (het `else`-geval), ziet de structuur er zo uit:

```scheme
(if test
  do-this
  else-do-this)
```

- Als de `test` waar (`#t`) evalueert, wordt het **consequent**-codeblok uitgevoerd.
- Als de `test` onwaar (`#f`) evalueert, wordt het **alternative**-codeblok uitgevoerd.

```scheme
(if test
  consequent
  alternative)
```

### Hoe het werkt

1. **Testexpressie:**
   - De `test`-expressie wordt eerst geëvalueerd.

2. **Resultaat op basis van de test:**
   - Als de `test` waar (`#t`) evalueert, wordt het **consequent-codeblok** uitgevoerd.
   - Als de `test` onwaar (`#f`) evalueert, wordt het **alternative-codeblok** uitgevoerd.

Zowel het `consequent`- als het `alternative`-codeblok kunnen elke geldige Scheme-bewerking uitvoeren, inclusief het retourneren van waarden, het wijzigen van variabelen of het uitvoeren van procedures.

### Voorbeelden

#### Voorbeeld 1: een waarde retourneren

```scheme
(if (< 0 1)
  1
  0)
```

- Hier is de `test` `(< 0 1)` (controle of 0 kleiner is dan 1).
- Omdat de test waar (`#t`) evalueert, wordt het **consequent**-blok (`1`) uitgevoerd en wordt de waarde ervan geretourneerd.

Resultaat: **1**

#### Voorbeeld 2: een begin-blok evalueren

Als je meerdere acties moet uitvoeren wanneer de conditie waar of onwaar is, kun je `begin` of `let` gebruiken om ze te groeperen.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- In dit voorbeeld is de `test` `(= 0 1)` (controle of 0 gelijk is aan 1).
- Omdat de test onwaar (`#f`) evalueert, wordt het **alternative**-blok uitgevoerd:
  - Eerst drukt het `"False condition met, calculating..."` af.
  - Vervolgens berekent het `(* 3 4)` en retourneert `12`.

Resultaat: **Drukt "False condition met, calculating..." af en retourneert 12.**

#### Voorbeeld 3: een let-expressie evalueren

Met `let` kun je lokale variabelen binnen het codeblok declareren.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- In dit voorbeeld is de `test` `(= 1 1)` (controle of 1 gelijk is aan 1).
- Omdat de test waar (`#t`) evalueert, wordt het **consequent**-blok uitgevoerd:
  - Eerst drukt het `"True condition met, calculating..."` af.
  - Vervolgens berekent het `(* -1 10)` en retourneert `-10`.

Resultaat: **Drukt "True condition met, calculating..." af en retourneert -10.**

### Samenvatting

- Het `if`-conditionele is een krachtig hulpmiddel in Scheme voor het evalueren van tests en het uitvoeren van bijbehorende codeblokken.
- Het kan zowel eenvoudige expressies als complexe codeblokken aan die waarden retourneren, variabelen wijzigen of neveneffecten uitvoeren.
- Onthoud: als er geen expliciet `else`-blok is, evalueert en voert `if` alleen het **consequent** uit als de test waar is; anders het **alternative**.
