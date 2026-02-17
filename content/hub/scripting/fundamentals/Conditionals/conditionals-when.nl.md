---
title: "wanneer"
type: docs
weight: 5
---
Hoewel `if` in Scheme elegant en veelzijdig is, kan het verwarrend worden als het wordt gebruikt zonder een expliciete `else`. Dit geldt met name als het de bedoeling is om één enkele codevertakking alleen uit te voeren als een voorwaarde waar is, zonder alternatieve actie voor het geval `false`. In dergelijke scenario's biedt de `when` constructie een duidelijker en beknopter alternatief.

De basisvorm van `when` ziet er als volgt uit:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Als de `test` evalueert naar true (`#t`), worden alle expressies in de hoofdtekst van de `when` constructie opeenvolgend uitgevoerd.
- Als `test` resulteert in false (`#f`), gebeurt er niets en worden er geen waarden geretourneerd.

### Voorbeeld

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Contrasterende `if` en `when`

Om het verschil tussen `if` en `when` beter te begrijpen, kunt u het volgende voorbeeld bekijken waarin beide samen worden gebruikt:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Uitleg:

1. **De `if` voorwaarde**:
   - De test `(= 0 1)` controleert of 0 gelijk is aan 1.
   - Omdat dit niet waar is (`#f`), wordt de `else` branch van `if` uitgevoerd.

2. **Het `when`-construct in de `else`-filiaal**:
   - De `when` test `(< 0 1)` controleert of 0 kleiner is dan 1.
   - Aangezien dit waar is (`#t`), worden alle expressies in de hoofdtekst van `when` opeenvolgend uitgevoerd:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Waarom hier `when` gebruiken?

- Het gebruik van `when` in plaats van een andere `if` vereenvoudigt de logica wanneer er geen behoefte is aan een expliciete `else` vertakking voor de voorwaarde.
- `when` maakt duidelijk dat alleen de echte branch relevant is, waardoor potentiële verwarring wordt verminderd.

### Samenvatting

- Gebruik `if` als je zowel een true als false branch nodig hebt.
- Gebruik `when` als er slechts één vertakking is voor het ware geval, vooral als er meerdere acties moeten worden uitgevoerd.
- Het combineren van `if` en `when` kan helpen complexere voorwaardelijke bepalingen duidelijk en beknopt te structureren.