---
title: "karta"
type: docs
weight: 3
---
Funktionen `map` i Schema används för att tillämpa en procedur på varje element i en lista (eller flera listor) och **returnera en ny lista** som innehåller resultaten. Detta gör den idealisk för att transformera data.

Den enklaste formen av `map` ser ut så här:

```scheme
(map procedure list)
```

- **Procedur**: En funktion som ska tillämpas på varje element i listan.
- **Lista**: Listan vars element kommer att transformeras.

---

### Exempel: Dubbla varje element

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Här tillämpas funktionen `double` på varje element i listan `(1 2 3 4)`.
– Resultatet är en ny lista med varje element fördubblat.

**Utdata**: `(2 4 6 8)`

---

### Hur det fungerar

1. **Skapar en ny lista**:
   - `map` tillämpar den tillhandahållna proceduren på varje element i listan och samlar resultaten till en ny lista.

2. **Transformerar data**:
   – Det används främst för datatransformationer snarare än att utföra biverkningar.

---

#### Exempel: Användning med flera listor

Om flera listor tillhandahålls, behandlar `map` motsvarande element från varje lista.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Funktionen `sum` lägger till motsvarande element från de två listorna och returnerar resultaten som en ny lista.

**Utdata**: `(5 7 9)`

---

### Sammanfattning

- Funktionen `map` är ett kraftfullt verktyg för att transformera listor genom att tillämpa en procedur på varje element.
- Till skillnad från `for-each`, `map` **producerar en ny lista** som innehåller resultaten av tillämpningen av proceduren.
- Det stöder flera listor, vilket tillåter elementvisa operationer över dem.

Genom att använda `map` kan du effektivt skapa transformerade versioner av dina data samtidigt som de ursprungliga listorna behålls oförändrade.