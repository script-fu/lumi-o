---
title: "Enkel rekursion"
type: docs
weight: 5
---
Rekursion är ett kraftfullt koncept i Scheme, där en funktion kallar sig för att lösa mindre delproblem av det ursprungliga problemet. Ett **enkelt rekursionsmönster** innefattar ett basfall för att stoppa rekursionen och ett rekursivt fall för att minska problemet.

Den allmänna strukturen för en rekursiv funktion ser ut så här:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Basvillkor**: Stoppar rekursionen.
- **Basresultat**: Värdet som returneras när basvillkoret är uppfyllt.
- **Rekursivt anrop**: Ett anrop till själva funktionen med modifierade argument som flyttar beräkningen närmare basfallet.

---

### Exempel: Summan av tal (1 till n)

En enkel rekursiv funktion för att beräkna summan av tal från 1 till n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Hur det fungerar: Bryta ner och återmontera

Rekursion fungerar genom att bryta ner det ursprungliga problemet i mindre bitar. Varje anrop till funktionen hanterar en del och skickar resten vidare. När det enklaste fallet är nått, sätts resultaten ihop igen när beräkningen slutförs.

#### Steg-för-steg Spår av summa-till-n 3

1. **Initialt samtal**: *sum-till-n 3*
   → *(+ 3 (summa-till-n 2))*

2. **Andra samtal**: *summa-till-n 2*
   → *(+ 2 (summa-till-n 1))*

3. **Tredje samtal**: *summa-till-n 1*
   → *(+ 1 (summa-till-n 0))*

4. **Basfall**: *summa-till-n 0*
   → *0*

---

#### Återmontering av slutresultatet

När det enklaste fallet är löst slutförs varje lager i beräkningen:

1. *summa-till-n 0* ger *0*
2. *summa-till-n 1* blir *(+ 1 0) = 1*
3. *summa-till-n 2* blir *(+ 2 1) = 3*
4. *summa-till-n 3* blir *(+ 3 3) = 6*

---

### Exempel: Skriva ut varje element i en lista

Här är en enkel rekursiv funktion för att skriva ut varje element i en lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Base Case:** Om listan är tom (*null? lst*), stoppa rekursion.
- **Rekursivt fall:** Skriv ut det första elementet (*car lst*), anropa sedan funktionen på resten av listan (*cdr lst*).

#### Exempel på användning

```scheme
(print-elements (list 1 2 3))
```

Utdata:

- *"1"*
- *"2"*
- *"3"*

Resultat: *"klar"*

---

#### Hur det fungerar

1. Funktionen hämtar det första elementet i listan med *bil* och bearbetar det.
2. Den anropar sig sedan med resten av listan (*cdr*).
3. Denna process upprepas tills listan är tom (*null? lst*).

---

### Sammanfattning

- Enkel rekursion består av:
  1. **Grundfall**: Stoppar rekursionen.
  2. **Rekursivt fall**: Minskar problemet mot basfallet.
- Varje rekursivt samtal fortskrider beräkningen mot slutförande.
- När basfallet har uppnåtts, kombineras resultaten när rekursionen slutförs.

Rekursion speglar problemets struktur och ger ett tydligt, logiskt flöde. Se alltid till ett basfall för att undvika oändlig rekursion.