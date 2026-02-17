---
title: "voor-elk"
type: docs
weight: 5
---
De functie `for-each` in Schema wordt gebruikt om een ​​procedure toe te passen op elk element van een lijst (of meerdere lijsten). In tegenstelling tot `map`, dat een nieuwe lijst met de resultaten retourneert, wordt `for-each` gebruikt vanwege de **bijeffecten**, zoals het afdrukken of bijwerken van variabelen.

De eenvoudigste vorm van `for-each` ziet er als volgt uit:

```scheme
(for-each procedure list)
```

- **Procedure**: een functie die op elk element van de lijst moet worden toegepast.
- **Lijst**: de lijst waarvan de elementen worden verwerkt.

---

### Voorbeeld: Druk een lijst af

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Hier wordt de functie `print-item` toegepast op elk element van de lijst `(1 2 3 4)`.
- Hierdoor wordt elk nummer opeenvolgend afgedrukt.

**Uitvoer**: `1 2 3 4`

---

### Hoe het werkt

1. **Herhaalt elk element**:
   - De opgegeven procedure wordt in volgorde voor elk element in de lijst uitgevoerd.

2. **Heeft bijwerkingen**:
   - Veel voorkomende bijwerkingen zijn afdrukken, loggen of wijzigen van externe variabelen. In tegenstelling tot `map`, retourneert `for-each` geen nieuwe lijst.

---

#### Voorbeeld: gebruik met meerdere lijsten

Als er meerdere lijsten beschikbaar zijn, verwerkt `for-each` overeenkomstige elementen uit elke lijst.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- De functie `sum-and-print` telt de corresponderende elementen uit de twee lijsten op en drukt de resultaten af.

**Uitvoer**: `5 7 9`

---

### Samenvatting

- De functie `for-each` is handig voor het uitvoeren van neveneffecten op elk element van een lijst.
- In tegenstelling tot `map` produceert `for-each` geen nieuwe lijst; het richt zich uitsluitend op de bijwerkingen van de procedure.
- Het kan meerdere lijsten tegelijkertijd verwerken, waarbij de procedure op overeenkomstige elementen wordt toegepast.

Door `for-each` te gebruiken, kunt u lijsten effectief verwerken wanneer het doel het uitvoeren van acties is in plaats van het transformeren van gegevens.