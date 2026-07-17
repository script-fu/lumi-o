---
title: "Vektorer"
type: "docs"
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57cae97347c4a9524567ebcc9eafbdf04228c2792c24e87784bf5f6255987d79
---
I Schema är en vektor en annan grundläggande datastruktur som används för att gruppera värden. Till skillnad från listor är vektorer indexerade samlingar av element med fast storlek, vilket ger snabbare slumpmässig åtkomst och uppdateringar. Varje element i en vektor kan vara av vilken typ som helst, inklusive en annan vektor. Vektorer representeras med # följt av parentes. `#(1 2 3)`

Även om vektorer och listor kan se likadana ut, tjänar de olika syften i schemaprogrammering:

- Listor används oftare för rekursiva operationer och dynamiska strukturer, eftersom deras länkade nodimplementering möjliggör effektiv manipulation av deras början och genomgång genom rekursiv nedbrytning.

- Vektorer, å andra sidan, är optimerade för scenarier där slumpmässig tillgång till element eller uppdateringar vid specifika index krävs, vilket gör dem mer lämpade för användningsfall som uppslagstabeller, konfigurationer med fast storlek eller prestandakritiska indexerade operationer.

I grund och botten är listor det naturliga valet för rekursiva algoritmer och dynamisk storlek data, medan vektorer lyser när fast storlek eller indexerade åtkomstmönster är av största vikt.

### Enkla vektorer

```scheme
(vector 1 2 3)
```

- Skapar en vektor med tre element: `1`, `2` och `3`.

Resultat: **`#(1 2 3)`**

#### Åtkomst till vektorelement

Element i en vektor nås med hjälp av proceduren `vector-ref`, den hämtar elementet vid ett specificerat index (med början från `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Hämtar elementet vid index 0
(vector-ref my-vector 1)  ; Hämtar elementet vid index 1
```

#### Iteration: Bearbetning av varje element i en vektor

Du kan iterera genom en vektor med en loop eller rekursion. Schema tillhandahåller `vector-length` för att bestämma storleken på en vektor. Här är en enkel slinga för att skriva ut varje element i en vektor:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Skriver ut elementet
        (loop (+ i 1)))                                    ; Bearbetar nästa index
      (lumi-message "done"))))                             ; Slut på loop
```

- **Basfall:** Om indexet `i` når vektorns längd, stoppa slingan.
- **Rekursivt skiftläge:** Skriv ut elementet på index `i`, öka sedan `i`.

#### Exempel på användning

```scheme
(print-elements (vector 1 2 3))
```

Resultat:

- `"1"`
- `"2"`
- `"3"`

Resultat: "klar"

### Blandade vektorer

Vektorer kan innehålla element av olika typer, inklusive strängar, booleaner, tal, andra vektorer eller till och med resultatet av uttryck:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Detta skapar en vektor med:
  - Ett nummer (`42`)
  - En sträng (`"hello"`)
  - En boolesk (`#t`)
  - En annan vektor (`#(1 2)`)
  - Resultatet av ett uttryck (`(+ 3 4)`, som utvärderas till `7`)

Resultat: **`#(42 "hello" #t #(1 2) 7)`**

### Konstruera vektorer

Vektorer skapas med `vector`, eller genom att använda `make-vector` för att skapa en vektor med en fast storlek med ett initialt värde.

```scheme
(make-vector 5 0)
```

Skapar en vektor med storlek `5` med alla element initialiserade till `0`.

Resultat: `#(0 0 0 0 0)`

### Uppdaterar vektorer

`vector-set!`-proceduren uppdaterar ett element i en vektor vid ett specificerat index.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sätter det andra elementet till 42
my-vector
```

Resultat: `#(1 42 3)`

### Söker efter vektorer

`vector?`-proceduren kontrollerar om ett givet värde är en vektor.

```scheme
(vector? (vector 1 2 3))  ; Kontrollerar om #(1 2 3) är en vektor
(vector? 42)              ; Kontrollerar om 42 är en vektor
```

Resultat:

- `(vector? (vector 1 2 3))` returnerar `#t` (sant)
- `(vector? 42)` returnerar `#f` (falskt)

### Vektorer och passerande referensbeteendeI Schema är vektorer föränderliga och skickas med referens. Detta innebär att när du skickar en vektor till en funktion kan funktionen modifiera den ursprungliga vektorn direkt. Alla ändringar som görs i vektorn inuti funktionen kommer att reflekteras utanför funktionen. Detta beteende är användbart för att effektivt dela och uppdatera data över flera funktioner, men det kräver också försiktighet för att undvika oavsiktliga biverkningar.

#### Exempel: Modifiera en vektor i en funktion

Här är ett exempel som visar hur vektorer skickas med referens och modifieras:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Uppdaterar vektorn vid angivet index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Ändrar det andra elementet till 99
my-vector                              ; Den ursprungliga vektorn är nu uppdaterad
```

Resultat: `#(10 99 30)`

#### Steg-för-steg-förklaring

1. **Skapa en vektor:** `my-vector` initieras med värdena `10`, `20` och `30`.
2. **Överför till en funktion:** `my-vector` skickas till `modify-vector` tillsammans med indexet och det nya värdet som ska uppdateras.
3. **Ändra i funktion:** `vector-set!`-proceduren uppdaterar värdet vid det angivna indexet direkt i den ursprungliga vektorn.
4. **Reflektera ändringar:** Eftersom vektorer skickas med referens, återspeglas ändringar som görs i funktionen i den ursprungliga vektorn.

#### Implikationer av Pass-by-Reference

- **Prestanda:** Att skicka vektorer genom referens är effektivt eftersom det undviker att kopiera stora strukturer.
- **Biverkningar:** Var försiktig när du delar vektorer mellan funktioner för att undvika oavsiktliga ändringar av delad data.

### Operationer på vektorer

Schema tillhandahåller flera inbyggda procedurer för att arbeta med vektorer, inklusive:

- `vector-length`: Returnerar antalet element i en vektor.
- `vector->list`: Konverterar en vektor till en lista.
- `list->vector`: Konverterar en lista till en vektor.

```scheme
(vector-length (vector 1 2 3))         ; Returnerar 3
(vector->list (vector 1 2 3))          ; Konverterar vektor till lista: (1 2 3)
(list->vector (list 1 2 3))            ; Konverterar lista till vektor: #(1 2 3)
```

Resultat:

- `(vector-length (vector 1 2 3))` returnerar `3`
- `(vector->list (vector 1 2 3))` returnerar `(1 2 3)`
- `(list->vector (list 1 2 3))` returnerar `#(1 2 3)`

### Kapslade vektorer

Vektorer i Scheme kan innehålla andra vektorer som element, vilket skapar en kapslad struktur.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Skapar en vektor med tre element, som vart och ett i sig är en vektor.

Resultat: **`#(#(1 2) #(3 4) #(5))`**

#### Åtkomst till kapslade data

För att komma åt element inom en kapslad vektor, använd `vector-ref` flera gånger för att navigera genom strukturen.

#### Exempel: Åtkomst till element

```scheme
(vector-ref nested-vector 0)              ; Hämtar det första elementet: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Hämtar det andra elementet i den första vektorn: 2
```

### Sammanfattning

- **Vektorer** i Scheme är indexerade datastrukturer med fast storlek.
- Använd `vector` för att skapa en vektor, `vector-ref` för att komma åt element och `vector-set!` för att uppdatera element.
- Inbyggda procedurer som `vector-length`, `vector->list` och `list->vector` möjliggör flexibla operationer.
- Kapslade vektorer möjliggör komplexa, hierarkiska datastrukturer.