---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
I sin enklaste form utvärderar villkoret `if` i Scheme ett test och kör, beroende på resultatet, ett av två möjliga kodblock. Den enklaste formen ser ut så här:

```scheme
(if test-is-true
  do-this)
```

- Om `test` utvärderas till sant (`#t`) körs **kodblocket i consequent**. Blocket kan returnera ett värde eller utföra andra åtgärder, till exempel tilldela en variabel eller skriva ut.

### Exempel

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Här är `test` `(< 0 1)` (kontroll om 0 är mindre än 1).
- Eftersom testet utvärderas till sant (`#t`) körs `(lumi-message "True!")`, vilket skriver ut `"True!"`.

### Lägga till else: `if-else`

När `if` har ett alternativt kodblock (fallet `else`) ser strukturen ut så här:

```scheme
(if test
  do-this
  else-do-this)
```

- Om `test` utvärderas till sant (`#t`) körs **consequent**.
- Om `test` utvärderas till falskt (`#f`) körs **alternative**.

```scheme
(if test
  consequent
  alternative)
```

### Så fungerar det

1. **Testuttryck:**
   - `test` utvärderas först.

2. **Resultat baserat på test:**
   - Om `test` utvärderas till sant (`#t`) körs **consequent**.
   - Om `test` utvärderas till falskt (`#f`) körs **alternative**.

Både `consequent` och `alternative` kan utföra vilket giltigt Scheme-uttryck som helst, inklusive att returnera värden, ändra variabler eller köra procedurer.

### Exempel

#### Exempel 1: returnera ett värde

```scheme
(if (< 0 1)
  1
  0)
```

- Här är `test` `(< 0 1)`.
- Eftersom testet är sant (`#t`) körs **consequent** (`1`) och dess värde returneras.

Resultat: **1**

#### Exempel 2: utvärdera ett `begin`-block

När flera åtgärder behövs i ett villkor kan du gruppera dem med `begin` eller `let`.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Här är `test` `(= 0 1)`.
- Eftersom testet är falskt (`#f`) körs **alternative**:
  - Först skrivs `"False condition met, calculating..."` ut.
  - Sedan beräknas `(* 3 4)` och `12` returneras.

Resultat: **Skriver ut "False condition met, calculating..." och returnerar 12.**

#### Exempel 3: utvärdera ett `let`-uttryck

Med `let` kan du deklarera lokala variabler i kodblocket.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Här är `test` `(= 1 1)`.
- Eftersom testet är sant (`#t`) körs **consequent**:
  - Först skrivs `"True condition met, calculating..."` ut.
  - Sedan beräknas `(* -1 10)` och `-10` returneras.

Resultat: **Skriver ut "True condition met, calculating..." och returnerar -10.**

### Sammanfattning

- Villkoret `if` är ett kraftfullt verktyg i Scheme för att utvärdera test och köra motsvarande kodblock.
- Det hanterar både enkla uttryck och komplexa kodblock som returnerar värden, ändrar variabler eller utför bieffekter.
- Utan explicit `else` körs **consequent** endast om testet är sant; annars **alternative**.
