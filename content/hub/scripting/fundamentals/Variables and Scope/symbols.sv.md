---
title: "Symboler"
type: docs
weight: 6
---
Symboler är en av kärndatatyperna i Scheme, som representerar unika, oföränderliga identifierare. De används främst som nycklar, markörer eller platshållare i program, vilket gör dem viktiga för att skriva ren och uttrycksfull kod.

En symbol i Scheme liknar en sträng men skiljer sig genom att symbolerna är **unika** och **atomära**. Detta innebär att två symboler med samma namn garanterat är samma objekt, vilket möjliggör snabba jämställdhetskontroller och effektiv användning i datastrukturer.

### Syntax

En symbol skrivs som en sekvens av tecken:

- Börjar med en bokstav, följt av bokstäver, siffror eller specialtecken som `-`, `+` eller `*`.
- Symboler är skiftlägeskänsliga som standard.

Exempel:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## Skapa symboler

Symboler skapas vanligtvis med operatorn **citat** (`'`), som säger till Scheme att behandla namnet som en symbol istället för att utvärdera det som en variabel eller funktion.

### Exempel

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

Du kan också skapa symboler programmatiskt med hjälp av proceduren `string->symbol`, som konverterar en sträng till en symbol.

```scheme
(string->symbol "dynamic-symbol")
```

**Resultat**: `'dynamic-symbol`


## Jämföra symboler

Eftersom symboler är unika kan du jämföra dem effektivt med `eq?`.

### Exempel

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

Detta gör symboler idealiska att använda som nycklar i datastrukturer eller markörer i din kod.

## Använda symboler

Symboler används ofta i Schema för:

1. **Nycklar i associationslistor:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **Identifierare i kod:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedurer för att arbeta med symboler

Schema tillhandahåller inbyggda procedurer för att arbeta med symboler:

| Tillvägagångssätt | Beskrivning |
|--------------------|--------------------------------------------------------------------------------|
| **`symbol?`** | Kontrollerar om ett objekt är en symbol.                                            |
| **`eq?`** | Jämför två symboler för identitet (snabb jämförelse).                       |
| **`symbol->string`** | Konverterar en symbol till en sträng (användbart för visning eller felsökning).          |
| **`string->symbol`** | Konverterar en sträng till en symbol (användbart för dynamiskt skapande av identifierare). |

### Exempel

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## Sammanfattning

Symboler är ett lättviktigt och effektivt sätt att representera identifierare, nycklar och markörer i Scheme. Deras oföränderlighet och snabba identitetskontroller gör dem idealiska för många programmeringsuppgifter. Att förstå hur man använder symboler effektivt kommer att förbättra din förmåga att skriva ren och uttrycksfull schemakod.