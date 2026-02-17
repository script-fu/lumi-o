---
title: "Eenvoudige recursie"
type: docs
weight: 5
---
Recursie is een krachtig concept in Scheme, waarbij een functie zichzelf aanroept om kleinere deelproblemen van het oorspronkelijke probleem op te lossen. Een **eenvoudig recursie**-patroon omvat een basisscenario om de recursie te stoppen en een recursief geval om het probleem te verminderen.

De algemene structuur van een recursieve functie ziet er als volgt uit:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Basisvoorwaarde**: stopt de recursie.
- **Basisresultaat**: de waarde die wordt geretourneerd wanneer aan de basisvoorwaarde wordt voldaan.
- **Recursieve oproep**: een aanroep van de functie zelf met gewijzigde argumenten die de berekening dichter bij het basisscenario brengen.

---

### Voorbeeld: som van getallen (1 tot n)

Een eenvoudige recursieve functie om de som van getallen van 1 tot n te berekenen:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Hoe het werkt: afbreken en opnieuw in elkaar zetten

Recursie werkt door het oorspronkelijke probleem in kleinere stukjes op te splitsen. Elke aanroep van de functie verwerkt één stuk en geeft de rest door. Zodra het eenvoudigste geval is bereikt, worden de resultaten opnieuw samengesteld zodra de berekening is voltooid.

#### Stap-voor-stap tracering van som-naar-n 3

1. **Eerste oproep**: *sum-to-n 3*
   → *(+ 3 (som-naar-n 2))*

2. **Tweede oproep**: *som-tot-n 2*
   → *(+ 2 (som-tot-n 1))*

3. **Derde oproep**: *som-tot-n 1*
   → *(+ 1 (som-tot-n 0))*

4. **Basisscenario**: *som-tot-n 0*
   → *0*

---

#### Het eindresultaat opnieuw in elkaar zetten

Zodra het eenvoudigste geval is opgelost, is elke laag van de berekening voltooid:

1. *som-naar-n 0* geeft *0*
2. *som-tot-n 1* wordt *(+ 1 0) = 1*
3. *som-tot-n 2* wordt *(+ 2 1) = 3*
4. *som-naar-n 3* wordt *(+ 3 3) = 6*

---

### Voorbeeld: elk element van een lijst afdrukken

Hier is een eenvoudige recursieve functie om elk element in een lijst af te drukken:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Basisscenario:** Als de lijst leeg is (*null? lst*), stop dan de recursie.
- **Recursief geval:** Druk het eerste element af (*car lst*) en roep vervolgens de functie op in de rest van de lijst (*cdr lst*).

#### Voorbeeldgebruik

```scheme
(print-elements (list 1 2 3))
```

Uitgang:

- *"1"*
- *"2"*
- *"3"*

Resultaat: *"klaar"*

---

#### Hoe het werkt

1. De functie haalt het eerste element van de lijst op met *car* en verwerkt dit.
2. Vervolgens roept het zichzelf aan met de rest van de lijst (*cdr*).
3. Dit proces herhaalt zich totdat de lijst leeg is (*null? lst*).

---

### Samenvatting

- Eenvoudige recursie bestaat uit:
  1. **Basisscenario**: stopt de recursie.
  2. **Recursief geval**: verkleint het probleem richting het basisscenario.
- Elke recursieve aanroep zorgt ervoor dat de berekening voltooid wordt.
- Zodra het basisscenario is bereikt, worden de resultaten gecombineerd naarmate de recursie is voltooid.

Recursie weerspiegelt de structuur van het probleem en zorgt voor een duidelijke, logische stroom. Zorg altijd voor een basisscenario om oneindige recursie te voorkomen.