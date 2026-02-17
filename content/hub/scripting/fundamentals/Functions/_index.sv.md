---
title: "Funktioner"
type: docs
weight: 7
---
Funktioner är ett kärnkoncept i Scheme, som ger möjlighet att kapsla in logik, möjliggöra kodåteranvändning och strukturera dina skript effektivt. Med funktioner kan du skapa modulära, underhållsbara skript som hanterar ett brett utbud av uppgifter, från grundläggande operationer till avancerade arbetsflöden i Lumi.

Detta avsnitt fungerar som en introduktion till funktioner i Scheme och lägger grunden för att förstå deras typer, definitioner och användningar. Efterföljande avsnitt kommer att fördjupa sig i specifika funktionstyper och deras unika möjligheter.

## Minimal syntax och uttryck

Schemakoden är gjord av **uttryck**. Ett uttryck utvärderas till ett värde. Syntaxen är enhetlig: parenteser bildar ett anrop, med operatörens eller funktionsnamnet först.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Eftersom allt är ett uttryck passar kontrollflödet naturligt i samma stil som funktionsanrop.

## Varför funktioner är viktiga

Funktioner spelar en central roll i Scheme av flera anledningar:

- **Kodåteranvändbarhet:** Undvik upprepning genom att kapsla in logik i återanvändbara komponenter.
- **Modularitet:** Bryt ner komplexa uppgifter i mindre, hanterbara delar.
- **Dynamiskt beteende:** Acceptera parametrar för att hantera olika input eller anpassa sig till olika situationer.
- **Högre abstraktion:** Förenkla logiken genom att fokusera på "vad" en funktion gör snarare än "hur" den gör den.

## Översikt över funktionstyper

Schema erbjuder en mängd olika funktionskonstruktioner, var och en lämpad för specifika användningsfall:

1. **Namnställda funktioner**
   Dessa är standardfunktioner definierade med `define`. De utgör ryggraden i de flesta manus.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Anonyma funktioner**
   Även känd som **lambda-funktioner**, dessa är namnlösa funktioner som definieras inline för engångsbruk.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Högre ordningsfunktioner**
   Funktioner som tar andra funktioner som argument eller returnerar funktioner som resultat, vilket möjliggör kraftfulla abstraktioner som kartläggning, filtrering och reducering.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Allmän syntax för funktioner

Funktioner i Scheme har en enkel och konsekvent syntax:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Namnet på funktionen.
- **`parameter1, parameter2, ...`:** Argumenten som funktionen tar.
- **`body-expression`:** Logiken som exekveras när funktionen anropas.

Exempel:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Biverkningar och globalt tillstånd

I Lumi har många användbara procedurer **biverkningar**: de ändrar en bild, ändrar en ritbar, skriver en fil eller visar utdata.

- Isolera biverkningar i små, tydligt namngivna ingrepp.
- Undvik att ändra globalt sammanhang om du inte behöver.
- När du ändrar sammanhang (färger, penslar, etc), linda in arbetet med `lumi-context-push` och `lumi-context-pop` så att användarens tillstånd återställs.