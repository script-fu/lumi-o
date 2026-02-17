---
title: "när"
type: docs
weight: 5
---
I Scheme, medan `if` är elegant och mångsidig, kan det bli förvirrande när det används utan ett uttryckligt `else`. Detta är särskilt sant när avsikten är att exekvera en enda kodgren endast när ett villkor är sant, utan någon alternativ åtgärd för `false`-fallet. I sådana scenarier ger `when` konstruktionen ett tydligare och mer kortfattat alternativ.

Den grundläggande formen av `when` ser ut så här:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Om `test` evalueras till sant (`#t`), exekveras alla uttryck i kroppen av `when` konstruktionen sekventiellt.
- Om `test` utvärderas till falskt (`#f`), händer ingenting och inga värden returneras.

### Exempel

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Kontrasterande `if` och `when`

För att bättre förstå skillnaden mellan `if` och `when`, överväg följande exempel där båda används tillsammans:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Förklaring:

1. **`if` tillstånd**:
   - Testet `(= 0 1)` kontrollerar om 0 är lika med 1.
   - Eftersom detta är falskt (`#f`), exekveras `else`-grenen av `if`.

2. **`when`-konstruktionen i `else`-grenen**:
   - `when`-testet `(< 0 1)` kontrollerar om 0 är mindre än 1.
   - Eftersom detta är sant (`#t`), exekveras alla uttryck i kroppen av `when` sekventiellt:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Varför använda `when` här?

- Att använda `when` istället för en annan `if` förenklar logiken när det inte finns något behov av en explicit `else`-gren för villkoret.
- `when` gör det klart att endast den sanna grenen är relevant, vilket minskar potentiell förvirring.

### Sammanfattning

- Använd `if` när du behöver både en sann och falsk gren.
- Använd `when` när det bara finns en enda gren för det sanna fallet, speciellt när flera åtgärder måste utföras.
- Att kombinera `if` och `when` kan hjälpa till att strukturera mer komplexa villkor tydligt och koncist.