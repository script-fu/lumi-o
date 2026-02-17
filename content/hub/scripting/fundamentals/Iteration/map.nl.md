---
title: "kaart"
type: docs
weight: 3
---
De functie `map` in Schema wordt gebruikt om een ​​procedure toe te passen op elk element van een lijst (of meerdere lijsten) en **een nieuwe lijst te retourneren** met de resultaten. Dit maakt het ideaal voor het transformeren van gegevens.

De eenvoudigste vorm van `map` ziet er als volgt uit:

```scheme
(map procedure list)
```

- **Procedure**: een functie die op elk element van de lijst moet worden toegepast.
- **Lijst**: de lijst waarvan de elementen worden getransformeerd.

---

### Voorbeeld: verdubbel elk element

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Hier wordt de functie `double` toegepast op elk element van de lijst `(1 2 3 4)`.
- Het resultaat is een nieuwe lijst waarbij elk element verdubbeld is.

**Uitvoer**: `(2 4 6 8)`

---

### Hoe het werkt

1. **Maakt een nieuwe lijst**:
   - `map` past de aangegeven procedure toe op elk element van de lijst en verzamelt de resultaten in een nieuwe lijst.

2. **Transformeert gegevens**:
   - Het wordt voornamelijk gebruikt voor gegevenstransformaties in plaats van voor het uitvoeren van bijwerkingen.

---

#### Voorbeeld: gebruik met meerdere lijsten

Als er meerdere lijsten beschikbaar zijn, verwerkt `map` overeenkomstige elementen uit elke lijst.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- De functie `sum` voegt overeenkomstige elementen uit de twee lijsten toe en retourneert de resultaten als een nieuwe lijst.

**Uitvoer**: `(5 7 9)`

---

### Samenvatting

- De functie `map` is een krachtig hulpmiddel voor het transformeren van lijsten door op elk element een procedure toe te passen.
- In tegenstelling tot `for-each`, produceert `map` **een nieuwe lijst** met de resultaten van het toepassen van de procedure.
- Het ondersteunt meerdere lijsten, waardoor elementgewijze bewerkingen mogelijk zijn.

Door `map` te gebruiken, kunt u op efficiënte wijze getransformeerde versies van uw gegevens maken, terwijl de oorspronkelijke lijsten ongewijzigd blijven.