---
title: "om"
type: docs
weight: 4
---
I sin enklaste form utvärderar `if` villkoret i Schema ett test och, baserat på resultatet, exekverar ett av två möjliga kodblock. Den enklaste formen ser ut så här:

```scheme
(if test-is-true
  do-this)
```

- Om `test` evalueras till sant (`#t`), exekveras **kodblocket i efterföljande**. Blocket kan returnera ett värde eller utföra andra åtgärder, såsom att tilldela en variabel eller skriva ut.

### Exempel

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- I det här fallet är `test` `(< 0 1)` (kontrollerar om 0 är mindre än 1).
- Eftersom testet utvärderas till sant (`#t`), exekveras kodblocket `(lumi-message "True!")`, vilket skriver ut `"True!"`.

### Lägga till ett annat villkor: `if-else`

När du använder en `if` villkorlig med ett alternativt kodblock (`else`-fallet), ser strukturen ut så här:

```scheme
(if test
  do-this
  else-do-this)
```

- Om `test` evalueras till sant (`#t`), exekveras kodblocket **följande**.
- Om `test` evalueras till falskt (`#f`), exekveras det **alternativa** kodblocket.

```scheme
(if test
  consequent
  alternative)
```

### Hur det fungerar

1. **Testuttryck**:
   - `test`-uttrycket utvärderas först.

2. **Resultat baserat på test**:
   - Om `test` evalueras till sant (`#t`), exekveras **följande kodblock**.
   - Om `test` evalueras till falskt (`#f`), exekveras det **alternativa kodblocket**.

Både `consequent` och `alternative` kan utföra vilken giltig Scheme-operation som helst, inklusive att returnera värden, ändra variabler eller köra procedurer.

### Exempel

#### Exempel 1: Returnera ett värde

```scheme
(if (< 0 1)
  1
  0)
```

- Här är `test` `(< 0 1)` (kontrollerar om 0 är mindre än 1).
- Eftersom testet utvärderas till sant (`#t`), exekveras blocket **följande** (`1`) och dess värde returneras.

Resultat: **1**

#### Exempel 2: Utvärdera ett startblock

I fall där du behöver utföra flera åtgärder när villkoret är sant eller falskt, kan du använda `begin` eller en `let` för att gruppera dem.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- I det här exemplet är `test` `(= 0 1)` (kontrollerar om 0 är lika med 1).
- Eftersom testet utvärderas till falskt (`#f`), exekveras det **alternativa** blocket:
  - Först skriver den ut `"False condition met, calculating..."`.
  - Sedan beräknar den `(* 3 4)` och returnerar `12`.

Resultat: **Skriver ut "Falskt villkor uppfyllt, beräknar..." och returnerar 12.**

#### Exempel 3: Utvärdera ett utlåtande

Genom att använda en `let` kan vi deklarera lokala scope-variabler inom kodblocket.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- I det här exemplet är `test` `(= 1 1)` (kontrollerar om 1 är lika med 1).
- Eftersom testet utvärderas till sant (`#t`), exekveras **följande**-blocket:
  - Först skriver den ut `"True condition met, calculating..."`.
  - Sedan beräknar den `(* -1 10)` och returnerar `-10`.

Resultat: **Skriver ut "True condition uppfyllt, beräknar..." och returnerar -10.**

### Sammanfattning- `if` är ett kraftfullt verktyg i Scheme för att utvärdera tester och exekvera motsvarande kodblock.
– Den kan hantera både enkla uttryck och komplexa kodblock som returnerar värden, modifierar variabler eller utför biverkningar.
- Kom ihåg: Om det inte finns något explicit `else`-block, utvärderar och utför `if` endast **följande** om testet är sant. Annars utvärderar och exekverar den **alternativet**.