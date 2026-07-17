---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
I Scheme är `if` elegant och mångsidig, men utan explicit `else` kan den bli förvirrande — särskilt när avsikten är att köra en enda gren endast när villkoret är sant, utan någon åtgärd för falskt fall. I sådana situationer ger konstruktionen `when` ett tydligare och mer koncist alternativ.

Den grundläggande formen av `when` ser ut så här:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Om `test` utvärderas till sant (`#t`) körs alla uttryck i `when`-kroppen i ordning.
- Om `test` utvärderas till falskt (`#f`) händer inget och inga värden returneras.

### Exempel

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Jämförelse mellan `if` och `when`

För att förstå skillnaden tydligare, tänk på följande exempel där båda används tillsammans:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Förklaring:

1. **`if`-villkoret:**
   - Testet `(= 0 1)` kontrollerar om 0 är lika med 1.
   - Eftersom det är falskt (`#f`) körs `else`-grenen av `if`.

2. **`when` i else-grenen:**
   - Testet `(< 0 1)` kontrollerar om 0 är mindre än 1.
   - Eftersom det är sant (`#t`) körs alla uttryck i `when`-kroppen i ordning:
     - Först skrivs `"The 'when' condition is true!"` ut.
     - Sedan skrivs `"Executing multiple actions within 'when'."` ut.

#### Varför använda `when` här?

- `when` förenklar logiken när det inte behövs en explicit `else`-gren.
- `when` gör tydligt att endast den sanna grenen är relevant, vilket minskar risken för missförstånd.

### Sammanfattning

- Använd `if` när både sann och falsk gren behövs.
- Använd `when` när endast den sanna grenen är relevant, särskilt när flera åtgärder ska köras.
- Att kombinera `if` och `when` kan göra mer komplexa villkor tydligare och mer koncisa.
