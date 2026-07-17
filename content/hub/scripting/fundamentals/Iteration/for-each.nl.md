---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
url: "hub/scripting/fundamentals/Iteration/for-each"
---
De functie `for-each` in Scheme wordt gebruikt om een procedure toe te passen op elk element van een lijst (of meerdere lijsten). In tegenstelling tot `map`, dat een nieuwe lijst met resultaten retourneert, wordt `for-each` gebruikt voor **neveneffecten**, zoals afdrukken of het bijwerken van variabelen.

De eenvoudigste vorm van `for-each` ziet er zo uit:

```scheme
(for-each procedure list)
```

- **Functie:** Een functie die op elk element van de lijst wordt toegepast.
- **Lijst:** De lijst waarvan de elementen worden verwerkt.

---

### Voorbeeld: een lijst afdrukken

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Hier wordt de functie `print-item` toegepast op elk element van de lijst `(1 2 3 4)`.
- Elk getal wordt achtereenvolgens afgedrukt.

**Output**: `1 2 3 4`

---

### Hoe het werkt

1. **Itereert over elk element:**
   - De opgegeven procedure wordt voor elk element in de lijst, in volgorde, uitgevoerd.

2. **Voert neveneffecten uit:**
   - Veelvoorkomende neveneffecten zijn afdrukken, loggen of het wijzigen van externe variabelen. In tegenstelling tot `map` retourneert `for-each` geen nieuwe lijst.

---

#### Voorbeeld: met meerdere lijsten

Als meerdere lijsten worden opgegeven, verwerkt `for-each` de overeenkomstige elementen uit elke lijst.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- De functie `sum-and-print` telt overeenkomstige elementen uit de twee lijsten op en drukt de resultaten af.

**Output**: `5 7 9`

---

### Samenvatting

- De functie `for-each` is nuttig voor het uitvoeren van neveneffecten op elk element van een lijst.
- In tegenstelling tot `map` produceert `for-each` geen nieuwe lijst — het richt zich uitsluitend op de neveneffecten van de procedure.
- Het kan meerdere lijsten tegelijk verwerken door de procedure toe te passen op overeenkomstige elementen.

Met `for-each` kun je lijsten effectief verwerken wanneer het doel acties uitvoeren is in plaats van gegevens transformeren.
