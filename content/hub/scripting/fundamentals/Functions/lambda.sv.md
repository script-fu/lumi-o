---
title: "Lambda funktioner"
type: docs
weight: 1
---
**Lambda-funktioner** i Scheme är anonyma funktioner, vilket betyder att de är funktioner utan namn. Dessa funktioner är definierade inline och används vanligtvis för korta, engångsoperationer. `lambda`-konstruktionen är ett kraftfullt verktyg för funktionell programmering, som låter dig skapa kortfattad och flexibel logik i farten.

Lambdafunktioner är särskilt användbara när:

– Du behöver en liten funktion för ett specifikt, tillfälligt syfte.
- Skicka funktioner som argument till funktioner av högre ordning som `map`, `filter` eller `fold`.
- Returnera funktioner från andra funktioner.

### Syntax för lambdafunktioner

Lambdafunktioner kan definieras på egen hand...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...eller anropas omedelbart:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Parametrarna som funktionen accepterar.
- **`body-expression`:** Logiken som exekveras när funktionen anropas.
- **Omedelbar anrop:** Den andra formen visar en lambda som omedelbart anropas med argument.

### Exempel på lambdafunktioner

#### Använda Lambda för enkla beräkningar

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Här:

- En lambdafunktion skapas för att lägga till två siffror (`x` och `y`).
- Funktionen anropas omedelbart med argumenten `3` och `5`.

#### Inline Lambda-funktioner

Följande exempel visar hur du använder `for-each` med både en namngiven funktion och en lambdafunktion:

**Använda en namngiven funktion:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Förklaring**:
  - `print-item` är en namngiven funktion som konverterar ett tal till en sträng (`number->string`) och skriver ut det med `lumi-message`.
  - `for-each` tillämpar `print-item` för varje element i listan `(1 2 3 4)`.

**Utdata**: 1 2 3 4

**Använda en lambdafunktion:**

Samma logik kan skrivas inline med en lambda-funktion, vilket undviker behovet av en separat namngiven funktion:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Förklaring**:
  - `(lambda (x) (lumi-message (number->string x)))` definierar en anonym funktion.
  - Denna funktion tillämpas på varje element i listan `(1 2 3 4)` av `for-each`.

**Utdata**: 1 2 3 4

#### Lambda fungerar som argument

Lambdafunktioner skickas ofta direkt till högre ordningsfunktioner som `map` eller `filter`.

#### Kvadratering av en lista med tal

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- Funktionen `lambda` kvadrerar varje element i listan.
- Funktionen `map` tillämpar `lambda` på varje element.

#### Lambda Fungerar som returvärden

Du kan returnera en lambdafunktion från en annan funktion för att skapa dynamiskt beteende.

#### Generera en adderfunktion

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` genererar en ny lambdafunktion som lägger till ett specifikt nummer (`n`).
- Den returnerade lambdan lagras i `add5`, vilket lägger till `5` till dess indata.

#### Använda Lambda med `let`

Lambdas används ofta med `let` för att skapa lokalt omfångade, tillfälliga funktioner.

#### Lokal Lambda för tillägg

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- `let` binder en lambdafunktion till namnet `add`.
- Lambdan används sedan som en normal funktion inom `let` räckvidden.

#### Kombinera lambda med högre ordningsfunktioner

Lambdas lyser när de kombineras med funktioner av högre ordning för att utföra komplexa datatransformationer.

#### Filtrera jämna tal

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- `lambda` kontrollerar om ett tal är jämnt.
- Funktionen `filter` använder lambda för att bara behålla jämna nummer från listan.

### Fördelar med Lambda-funktioner

- **Koncis:** Lambdas reducerar koden genom att ta bort behovet av att definiera separata namngivna funktioner.
- **Flexibilitet:** Du kan definiera och använda dem varhelst de behövs, vilket gör koden mer modulär.
- **Förbättrad läsbarhet:** För korta, specifika uppgifter gör lambdas tydliga avsikter utan att belamra koden med ytterligare namngivna funktioner.

### När ska lambdafunktionerna användas

Använd lambdafunktioner när:

– Logiken är kort och fristående.
- Funktionen behövs endast tillfälligt eller inom en specifik omfattning.
- Du arbetar med funktioner av högre ordning som `map`, `filter` eller `reduce`.

Undvik att använda lambdas för komplex flerradslogik, eftersom detta kan minska läsbarheten. För mer omfattande operationer, använd istället en namngiven funktion.

### Slutsats

Lambdafunktioner i Scheme ger ett kortfattat och kraftfullt sätt att definiera anonyma funktioner för specifika uppgifter. Deras flexibilitet och användarvänlighet gör dem till ett viktigt verktyg för alla Scheme-programmerare. Att förstå hur du använder `lambda` effektivt hjälper dig att skriva renare, mer modulära och effektiva skript.