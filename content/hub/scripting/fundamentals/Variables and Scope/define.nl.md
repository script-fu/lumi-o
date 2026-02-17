---
title: "definiëren"
type: docs
weight: 3
---
De instructie `define` in Scheme is een veelzijdige constructie die wordt gebruikt om globale of lokale bindingen te creëren. Het wordt meestal gebruikt om variabelen en functies te definiëren, waardoor ze herbruikbaar en toegankelijk worden binnen een script of binnen een specifiek bereik. Het begrijpen van `define` is cruciaal voor het schrijven van modulaire, herbruikbare en leesbare Scheme-programma's.

### Doel van `define`

Het `define`-construct dient meerdere doelen:
- **Variabelen definiëren**: wijst waarden toe aan variabelenamen, zodat ze beschikbaar zijn voor later gebruik.
- **Functies definiëren**: Creëert herbruikbare procedures die specifieke logica inkapselen.
- **Lokale definities**: bij gebruik binnen een functie creëert `define` lokale bindingen die geen invloed hebben op de globale naamruimte.

---

### Variabelen definiëren met `define`

Een basisgebruik van `define` is het maken van variabelen die constante of berekende waarden bevatten.

#### Syntaxis
```scheme
(define variable-name value)
```

#### Voorbeeld: een constante definiëren
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Resultaat**: `6.28318`

---

### Functies definiëren met `define`

U kunt `define` gebruiken om herbruikbare procedures te maken.

#### Syntaxis
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Voorbeeld: een eenvoudige functie definiëren
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Resultaat**: `16`

---

### Lokale definities met `define`

Bij gebruik binnen een functie creëert `define` lokale bindingen die alleen toegankelijk zijn binnen de omsluitende functie. Dit voorkomt vervuiling van de globale naamruimte en helpt bij het organiseren van uw code.

#### Voorbeeld: Lokale helperfuncties
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultaat**: `41` (Berekent \(2^2 + 3^3 + 4^2\))

---

### Belangrijkste kenmerken van `define`

1. **Wereldwijd of lokaal bereik**:
   - Bij gebruik op het hoogste niveau creëert `define` globale variabelen of functies.
   - Bij gebruik binnen een andere functie creëert `define` lokale bindingen.

2. **Herbruikbaarheid**:
   - Functies gedefinieerd met `define` kunnen meerdere keren in verschillende contexten worden hergebruikt.

3. **Verbeterde leesbaarheid**:
   - Het opsplitsen van logica in kleinere, goed benoemde functies verbetert de duidelijkheid en onderhoudbaarheid van uw code.

---

### Verschillen tussen `define` en `let`

| **Aspect** | **`define`** | **`let`** |
|-----------------------|----------------------------- -----------------|------------------------------------|
| **Doel** | Creëert globale of lokale bindingen voor variabelen of functies. | Creëert tijdelijke bindingen in een gelokaliseerd bereik. |
| **Reikwijdte** | Mondiaal wanneer op het hoogste niveau; lokaal wanneer binnen een andere functie. | Altijd lokaal voor het `let` blok.       |
| **Herbruikbaarheid** | Functies en variabelen kunnen op meerdere plaatsen worden hergebruikt. | Variabelen zijn tijdelijk gebonden voor één blok. |
| **Syntaxis** | Definieert expliciet variabelen of functies.       | Combineert variabelebinding met expressie-evaluatie. |