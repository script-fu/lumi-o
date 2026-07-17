---
title: "Enkel rekursion"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
Rekursion är ett kraftfullt begrepp i Scheme där en funktion anropar sig själv för att lösa mindre delproblem av det ursprungliga problemet. Ett mönster för **enkel rekursion** har ett basfall som stoppar rekursionen och ett rekursivt fall som minskar problemet.

Den allmänna strukturen för en rekursiv funktion ser ut så här:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Basvillkor:** stoppar rekursionen.
- **Basresultat:** värdet som returneras när basvillkoret uppfylls.
- **Rekursivt anrop:** ett anrop till funktionen själv med modifierade argument som för beräkningen närmare basfallet.

---

### Exempel: summa av tal (1 till n)

En enkel rekursiv funktion för att beräkna summan av talen från 1 till n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Basfall: stoppa när n är 0
    0                          ; Basresultat: summan är 0
    (+ n (sum-to-n (- n 1))))) ; Rekursivt anrop: addera n till resultatet av det mindre delproblemet
```

---

#### Så fungerar det: dela upp och sätta ihop

Rekursion fungerar genom att dela upp det ursprungliga problemet i mindre delar. Varje anrop hanterar en del och lämnar resten vidare. När det enklaste fallet nås sätts resultaten ihop igen när beräkningen slutförs.

#### Steg för steg: sum-to-n 3

1. **Initialt anrop:** *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Andra anropet:** *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Tredje anropet:** *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Basfall:** *sum-to-n 0*
   → *0*

---

#### Sätta ihop slutresultatet

När det enklaste fallet är löst slutförs varje lager av beräkningen:

1. *sum-to-n 0* ger *0*
2. *sum-to-n 1* blir *(+ 1 0) = 1*
3. *sum-to-n 2* blir *(+ 2 1) = 3*
4. *sum-to-n 3* blir *(+ 3 3) = 6*

---

### Exempel: skriv ut varje element i en lista

Här är en enkel rekursiv funktion som skriver ut varje element i en lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Skriv ut första elementet
      (print-elements (cdr lst)))))             ; Bearbeta resten av listan
```

- **Basfall:** om listan är tom (*null? lst*), stoppa rekursionen.
- **Rekursivt fall:** skriv ut första elementet (*car lst*), anropa sedan funktionen på resten av listan (*cdr lst*).

#### Exempelanvändning

```scheme
(print-elements (list 1 2 3))
```

Utdata:

- *"1"*
- *"2"*
- *"3"*

Resultat: *"done"*

---

#### Så fungerar det

1. Funktionen hämtar det första elementet i listan med *car* och bearbetar det.
2. Den anropar sedan sig själv med resten av listan (*cdr*).
3. Processen upprepas tills listan är tom (*null? lst*).

---

### Sammanfattning

- Enkel rekursion består av:
  1. **Basfall:** stoppar rekursionen.
  2. **Rekursivt fall:** minskar problemet mot basfallet.
- Varje rekursivt anrop för beräkningen närmare slutet.
- När basfallet nås kombineras resultaten när rekursionen slutförs.

Rekursion speglar problemets struktur och ger ett tydligt, logiskt flöde. Se alltid till att ha ett basfall för att undvika oändlig rekursion.
