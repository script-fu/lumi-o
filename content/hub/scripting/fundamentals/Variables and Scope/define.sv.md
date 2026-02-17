---
title: "definiera"
type: docs
weight: 3
---
`define`-satsen i Scheme är en mångsidig konstruktion som används för att skapa globala eller lokala bindningar. Det används oftast för att definiera variabler och funktioner, vilket gör dem återanvändbara och tillgängliga i ett skript eller inom ett specifikt omfång. Att förstå `define` är avgörande för att skriva modulära, återanvändbara och läsbara Scheme-program.

### Syftet med `define`

`define`-konstruktionen tjänar flera syften:
- **Definiera variabler**: Tilldelar värden till variabelnamn, vilket gör dem tillgängliga för senare användning.
- **Definiera funktioner**: Skapar återanvändbara procedurer som kapslar in specifik logik.
- **Lokala definitioner**: När den används inom en funktion skapar `define` lokala bindningar som inte påverkar det globala namnområdet.

---

### Definiera variabler med `define`

En grundläggande användning av `define` är att skapa variabler som håller konstanta eller beräknade värden.

#### Syntax
```scheme
(define variable-name value)
```

#### Exempel: Definiera en konstant
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Resultat**: `6.28318`

---

### Definiera funktioner med `define`

Du kan använda `define` för att skapa återanvändbara procedurer.

#### Syntax
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Exempel: Definiera en enkel funktion
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Resultat**: `16`

---

### Lokala definitioner med `define`

När den används i en funktion skapar `define` lokala bindningar som endast är tillgängliga inom den omslutande funktionen. Detta undviker att förorena det globala namnområdet och hjälper till att organisera din kod.

#### Exempel: Lokala hjälpfunktioner
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultat**: `41` (beräknar \(2^2 + 3^3 + 4^2\))

---

### Nyckelfunktioner hos `define`

1. **Globalt eller lokalt omfång**:
   - När den används på översta nivån skapar `define` globala variabler eller funktioner.
   - När den används i en annan funktion skapar `define` lokala bindningar.

2. **Återanvändbarhet**:
   - Funktioner definierade med `define` kan återanvändas flera gånger i olika sammanhang.

3. **Förbättrad läsbarhet**:
   - Att bryta logik i mindre, väl namngivna funktioner förbättrar din kods tydlighet och underhållbarhet.

---

### Skillnader mellan `define` och `let`

| **Aspekt** | **`define`** | **`let`** |
|------------------------|--------------------------------------------------------|----------------------------------------|
| **Syfte** | Skapar globala eller lokala bindningar för variabler eller funktioner. | Skapar tillfälliga bindningar i en lokal omfattning. |
| **Omfattning** | Global när man är på toppnivå; lokal när du är inne i en annan funktion. | Alltid lokalt för `let`-blocket.       |
| **Återanvändbarhet** | Funktioner och variabler kan återanvändas på flera ställen. | Variabler är bundna tillfälligt för ett enda block. |
| **Syntax** | Definierar explicit variabler eller funktioner.       | Kombinerar variabel bindning med uttrycksutvärdering. |