---
title: "för varje"
type: docs
weight: 5
---
`for-each`-funktionen i Schema används för att tillämpa en procedur på varje element i en lista (eller flera listor). Till skillnad från `map`, som returnerar en ny lista med resultaten, används `for-each` för dess **bieffekter**, som att skriva ut eller uppdatera variabler.

Den enklaste formen av `for-each` ser ut så här:

```scheme
(for-each procedure list)
```

- **Procedur**: En funktion som ska tillämpas på varje element i listan.
- **Lista**: Listan vars element kommer att bearbetas.

---

### Exempel: Skriv ut en lista

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Här tillämpas funktionen `print-item` på varje element i listan `(1 2 3 4)`.
- Detta gör att varje nummer skrivs ut sekventiellt.

**Utdata**: `1 2 3 4`

---

### Hur det fungerar

1. **Itererar över varje element**:
   - Den angivna proceduren exekveras för varje element i listan, i ordning.

2. **Utför biverkningar**:
   - Vanliga biverkningar inkluderar utskrift, loggning eller modifiering av externa variabler. Till skillnad från `map` returnerar inte `for-each` en ny lista.

---

#### Exempel: Användning med flera listor

Om flera listor tillhandahålls, behandlar `for-each` motsvarande element från varje lista.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Funktionen `sum-and-print` summerar motsvarande element från de två listorna och skriver ut resultaten.

**Utdata**: `5 7 9`

---

### Sammanfattning

- `for-each`-funktionen är användbar för att utföra biverkningar på varje element i en lista.
- Till skillnad från `map` producerar `for-each` ingen ny lista – den fokuserar enbart på procedurens biverkningar.
- Den kan hantera flera listor samtidigt och tillämpa proceduren på motsvarande element.

Genom att använda `for-each` kan du effektivt bearbeta listor när målet är att utföra åtgärder snarare än att transformera data.