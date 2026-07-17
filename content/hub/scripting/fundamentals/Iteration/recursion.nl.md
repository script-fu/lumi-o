---
title: "Eenvoudige recursie"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
Recursie is een krachtig concept in Scheme, waarbij een functie zichzelf aanroept om kleinere deelproblemen van het oorspronkelijke probleem op te lossen. Een patroon van **eenvoudige recursie** omvat een basispad om de recursie te stoppen en een recursief pad om het probleem te verkleinen.

De algemene structuur van een recursieve functie ziet er zo uit:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Basisconditie:** Stopt de recursie.
- **Basisresultaat:** De waarde die wordt geretourneerd wanneer aan de basisvoorwaarde is voldaan.
- **Recursieve aanroep:** Een aanroep van de functie zelf met aangepaste argumenten die de berekening dichter bij het basispad brengen.

---

### Voorbeeld: som van getallen (1 tot n)

Een eenvoudige recursieve functie om de som van getallen van 1 tot n te berekenen:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Basispad: stoppen wanneer n 0 is
    0                          ; Basisresultaat: som is 0
    (+ n (sum-to-n (- n 1))))) ; Recursieve aanroep: tel huidige n op bij resultaat van kleiner subprobleem
```

---

#### Hoe het werkt: ontleden en herassembleren

Recursie werkt door het oorspronkelijke probleem op te splitsen in kleinere stukken. Elke aanroep van de functie behandelt één stuk en geeft de rest door. Zodra het eenvoudigste geval is bereikt, worden de resultaten herassembleerd naarmate de berekening voltooid wordt.

#### Stap-voor-stap trace van sum-to-n 3

1. **Eerste aanroep:** *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Tweede aanroep:** *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Derde aanroep:** *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Basispad:** *sum-to-n 0*
   → *0*

---

#### Het eindresultaat herassembleren

Zodra het eenvoudigste geval is opgelost, voltooit elke laag van de berekening:

1. *sum-to-n 0* geeft *0*
2. *sum-to-n 1* wordt *(+ 1 0) = 1*
3. *sum-to-n 2* wordt *(+ 2 1) = 3*
4. *sum-to-n 3* wordt *(+ 3 3) = 6*

---

### Voorbeeld: elk element van een lijst afdrukken

Hier is een eenvoudige recursieve functie om elk element in een lijst af te drukken:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print het eerste element
      (print-elements (cdr lst)))))             ; Verwerkt de rest van de lijst
```

- **Basispad:** Als de lijst leeg is (*null? lst*), stopt de recursie.
- **Recursief pad:** Druk het eerste element af (*car lst*), roep de functie dan aan op de rest van de lijst (*cdr lst*).

#### Voorbeeldgebruik

```scheme
(print-elements (list 1 2 3))
```

Output:

- *"1"*
- *"2"*
- *"3"*

Resultaat: *"done"*

---

#### Hoe het werkt

1. De functie haalt het eerste element van de lijst op met *car* en verwerkt het.
2. Vervolgens roept het zichzelf aan met de rest van de lijst (*cdr*).
3. Dit proces herhaalt zich totdat de lijst leeg is (*null? lst*).

---

### Samenvatting

- Eenvoudige recursie bestaat uit:
  1. **Basispad:** Stopt de recursie.
  2. **Recursief pad:** Verkleint het probleem richting het basispad.
- Elke recursieve aanroep brengt de berekening dichter bij voltooiing.
- Zodra het basispad is bereikt, worden de resultaten gecombineerd wanneer de recursie voltooid is.

Recursie weerspiegelt de structuur van het probleem en biedt een heldere, logische flow. Zorg altijd voor een basispad om oneindige recursie te voorkomen.
