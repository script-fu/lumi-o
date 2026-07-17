---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
url: "hub/scripting/fundamentals/Iteration/map"
---
De functie `map` in Scheme wordt gebruikt om een procedure toe te passen op elk element van een lijst (of meerdere lijsten) en **een nieuwe lijst terug te geven** met de resultaten. Ideaal voor het transformeren van gegevens.

De eenvoudigste vorm van `map` ziet er zo uit:

```scheme
(map procedure list)
```

- **Functie:** Een functie die op elk element van de lijst wordt toegepast.
- **Lijst:** De lijst waarvan de elementen worden getransformeerd.

---

### Voorbeeld: elk element verdubbelen

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Hier wordt de functie `double` toegepast op elk element van de lijst `(1 2 3 4)`.
- Het resultaat is een nieuwe lijst waarin elk element is verdubbeld.

**Output**: `(2 4 6 8)`

---

### Hoe het werkt

1. **Maakt een nieuwe lijst:**
   - `map` past de opgegeven procedure toe op elk element van de lijst en verzamelt de resultaten in een nieuwe lijst.

2. **Transformeert gegevens:**
   - Het wordt vooral gebruikt voor datatransformaties in plaats van voor het uitvoeren van neveneffecten.

---

#### Voorbeeld: met meerdere lijsten

Als meerdere lijsten worden opgegeven, verwerkt `map` de overeenkomstige elementen uit elke lijst.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- De functie `sum` telt overeenkomstige elementen uit de twee lijsten op en retourneert de resultaten als een nieuwe lijst.

**Output**: `(5 7 9)`

---

### Samenvatting

- De functie `map` is een krachtig hulpmiddel om lijsten te transformeren door een procedure op elk element toe te passen.
- In tegenstelling tot `for-each` **produceert** `map` een nieuwe lijst met de resultaten van de toegepaste procedure.
- Het ondersteunt meerdere lijsten, waardoor elementgewijze bewerkingen over lijsten heen mogelijk zijn.

Met `map` kun je efficiënt getransformeerde versies van je gegevens maken terwijl de oorspronkelijke lijsten ongewijzigd blijven.
