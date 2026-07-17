---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
url: "hub/scripting/fundamentals/Iteration/for-each"
---
Funktionen `for-each` i Scheme tillämpar en procedur på varje element i en lista (eller flera listor). Till skillnad från `map`, som returnerar en ny lista med resultat, används `for-each` för **bieffekter** — till exempel utskrift eller uppdatering av variabler.

Den enklaste formen av `for-each` ser ut så här:

```scheme
(for-each procedure list)
```

- **Procedur:** funktionen som tillämpas på varje element i listan.
- **Lista:** listan vars element ska bearbetas.

---

### Exempel: skriv ut en lista

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Här tillämpas funktionen `print-item` på varje element i listan `(1 2 3 4)`.
- Det gör att varje tal skrivs ut i ordning.

**Utdata**: `1 2 3 4`

---

### Så fungerar det

1. **Itererar över varje element:**
   - Den angivna proceduren körs för varje element i listan, i ordning.

2. **Utför bieffekter:**
   - Vanliga bieffekter är utskrift, loggning eller ändring av externa variabler. Till skillnad från `map` returnerar `for-each` ingen ny lista.

---

#### Exempel: flera listor

Om flera listor anges bearbetar `for-each` motsvarande element från varje lista.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Funktionen `sum-and-print` adderar motsvarande element från de två listorna och skriver ut resultaten.

**Utdata**: `5 7 9`

---

### Sammanfattning

- Funktionen `for-each` är användbar när målet är bieffekter på varje element i en lista.
- Till skillnad från `map` producerar `for-each` ingen ny lista — den fokuserar enbart på procedurens bieffekter.
- Den kan hantera flera listor samtidigt och tillämpa proceduren på motsvarande element.

Med `for-each` kan du bearbeta listor effektivt när målet är att utföra handlingar snarare än att transformera data.
