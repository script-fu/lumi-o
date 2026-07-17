---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
url: "hub/scripting/fundamentals/Iteration/map"
---
Funktionen `map` i Scheme tillämpar en procedur på varje element i en lista (eller flera listor) och **returnerar en ny lista** med resultaten. Idealisk för datatransformation.

Den enklaste formen av `map` ser ut så här:

```scheme
(map procedure list)
```

- **Procedur:** funktionen som tillämpas på varje element i listan.
- **Lista:** listan vars element ska bearbetas.

---

### Exempel: fördubbla varje element

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Här tillämpas funktionen `double` på varje element i listan `(1 2 3 4)`.
- Det ger en ny lista där varje värde har fördubblats.

**Utdata**: `(2 4 6 8)`

---

### Så fungerar det

1. **Skapar en ny lista:**
   - `map` samlar resultaten från proceduren i en ny lista.

2. **Transformerar data:**
   - Till skillnad från `for-each` fokuserar `map` på datatransformation snarare än bieffekter.

---

#### Exempel: flera listor

Om flera listor anges bearbetar `map` motsvarande element från varje lista.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Funktionen `sum` adderar motsvarande element från de två listorna och returnerar resultaten som en ny lista.

**Utdata**: `(5 7 9)`

---

### Sammanfattning

- Funktionen `map` är ett kraftfullt verktyg för att transformera listor element för element.
- Till skillnad från `for-each` **producerar** `map` en ny lista.
- Den stöder flera listor och tillåter elementvisa operationer.

Med `map` kan du skapa transformerade versioner av data utan att ändra originalen.
