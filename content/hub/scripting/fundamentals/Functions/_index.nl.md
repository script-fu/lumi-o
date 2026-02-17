---
title: "Functies"
type: docs
weight: 7
---
Functies zijn een kernconcept in Scheme en bieden de middelen om logica in te kapselen, hergebruik van code mogelijk te maken en uw scripts effectief te structureren. Met functies kunt u modulaire, onderhoudbare scripts maken die een breed scala aan taken afhandelen, van basisbewerkingen tot geavanceerde workflows in Lumi.

Deze sectie dient als een inleiding tot de functies in Scheme en legt de basis voor het begrijpen van hun typen, definities en toepassingen. In de daaropvolgende secties wordt dieper ingegaan op specifieke functietypen en hun unieke mogelijkheden.

## Minimale syntaxis en expressies

Schemacode is gemaakt van **expressies**. Een expressie resulteert in een waarde. De syntaxis is uniform: haakjes vormen een aanroep, met de operator- of functienaam eerst.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Omdat alles een uitdrukking is, past de controlestroom op natuurlijke wijze in dezelfde stijl als functieaanroepen.

## Waarom functies ertoe doen

Functies spelen om verschillende redenen een cruciale rol in Scheme:

- **Herbruikbaarheid van code:** Vermijd herhaling door logica in herbruikbare componenten in te kapselen.
- **Modulariteit:** Breek complexe taken op in kleinere, beheersbare stukken.
- **Dynamisch gedrag:** Accepteer parameters om verschillende invoer te verwerken of aan te passen aan verschillende situaties.
- **Hogere abstractie:** Vereenvoudig de logica door te focussen op "wat" een functie doet in plaats van "hoe" deze het doet.

## Overzicht van functietypen

Scheme biedt een verscheidenheid aan functieconstructies, elk geschikt voor specifieke gebruiksscenario's:

1. **Benoemde functies**
   Dit zijn standaardfuncties die zijn gedefinieerd met `define`. Ze vormen de ruggengraat van de meeste scripts.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Anonieme functies**
   Ook bekend als **lambda-functies**. Dit zijn naamloze functies die inline zijn gedefinieerd voor eenmalig gebruik.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Functies van hogere orde**
   Functies die andere functies als argumenten gebruiken of functies als resultaten retourneren, waardoor krachtige abstracties zoals mapping, filteren en reduceren mogelijk zijn.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Algemene syntaxis voor functies

Functies in Schema hebben een eenvoudige en consistente syntaxis:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** De naam van de functie.
- **`parameter1, parameter2, ...`:** De argumenten die de functie nodig heeft.
- **`body-expression`:** De logica die wordt uitgevoerd wanneer de functie wordt aangeroepen.

Voorbeeld:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Bijwerkingen en mondiale toestand

In Lumi hebben veel nuttige procedures **bijeffecten**: ze wijzigen een afbeelding, wijzigen een tekenbestand, schrijven een bestand of geven uitvoer weer.

- Isoleer bijwerkingen in kleine, duidelijk benoemde procedures.
- Vermijd het veranderen van de mondiale context, tenzij dat nodig is.
- Wanneer u de context wijzigt (kleuren, penselen, enz.), verpakt u het werk met `lumi-context-push` en `lumi-context-pop` zodat de status van de gebruiker wordt hersteld.