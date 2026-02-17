---
title: "Omslag"
type: docs
weight: 4
---
Schemakommandon fungerar på en låg nivå, vilket innebär att även enkla uppgifter kan kräva flera steg. Men denna granularitet erbjuder flexibilitet, vi kan bunta ihop kommandon i små, återanvändbara funktioner som gör precis vad vi behöver. Omslag är inte ett svartvitt koncept; det kan sträcka sig från enkla alias för ofta använda kommandon till mer komplexa funktioner som hanterar hela arbetsflöden. Ibland är ett omslag bara en bekvämlighetsfunktion för att förbättra läsbarheten, medan det i andra fall utvecklas till ett fullfjädrat verktyg som kapslar in flera operationer.

### Varför Wrap-funktioner?

Det finns flera viktiga fördelar med omslagsfunktioner:

- **Förenklar repetitiva uppgifter** - Istället för att upprepa kommandon på låg nivå, slå in dem i en hjälpfunktion och återanvänd den.
- **Förbättrar läsbarheten** - Genom att ge våra inslagna funktioner tydliga, beskrivande namn blir vår kod lättare att förstå på ett ögonkast.
- **Inkapslar komplexitet** – Istället för att ta itu med långa, kryptiska listor med kommandon, djupt kapslade loopar eller komplexa meddelandesatser, kan vi dela upp dem i mindre, välstrukturerade hjälpfunktioner.
- **Förbättrar underhållsbarheten** – Om kärnfunktionaliteten för ett kommando ändras behöver vi bara uppdatera vår inkapslade funktion en gång, vilket isolerar våra plugin-program från detaljerna i dessa ändringar.
- **Uppmuntrar kodåteranvändning** - Varje hjälpare blir en del av ditt bibliotek, vilket gör framtida skript snabbare att skriva och felsöka.

När dina plugin-program växer hjälper omslag dig att hålla kärnlogiken läsbar och isolera återkommande detaljer.

En annan fördel med att radbryta funktioner är att integrera dem i en syntaxmarkör som Visual Studio Code. Detta förbättrar läsbarheten och navigeringen, vilket gör skripten tydligare. I en plug-in som använder anpassade funktioner, bekräftar alla grönmarkerade funktioner att den är korrekt refererad från vårt bibliotek.

Om du har ditt eget hjälpbibliotek, överväg att lägga till ditt projekts funktionsnamn till din editors syntaxmarkering. Det gör navigering och refaktorering snabbare.

Exempel:

### Random Seed

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Även om vi skulle kunna använda ***msrg-rand*** direkt i vår kod, förbättrar vi läsbarheten genom att linda in den i en funktion som kallas ***random-seed***. Genom att ge funktionen ett tydligt och beskrivande namn blir det lättare att förstå dess syfte med ett ögonkast.

Dessutom, genom att definiera ***random-seed*** som en fristående funktion kan vi använda den var som helst i våra plugin-program samtidigt som vi centraliserar implementeringen på en enda plats. Om vi ​​någonsin behöver ändra hur fröet genereras behöver vi bara uppdatera den här funktionen och lämna resten av vår kod orörd.

Om vi till exempel bestämmer oss för att byta till ***random*** istället:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

Funktionsnamnet förblir detsamma, vilket säkerställer att våra skript fortsätter att fungera utan ändringar. Detta tillvägagångssätt håller vår kod flexibel, underhållbar och lätt att läsa.

### JPEG exporterar

JPEG-exportfunktionen i Scheme kommer med många parametrar, vilket ger fin kontroll över hur bilder sparas. Men i de flesta fall bryr vi oss bara om ett fåtal nyckelinställningar, som filnamn och kvalitet. För att förenkla processen kan vi slå in funktionen.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

I denna omslagsfunktion är de flesta exportalternativen hårdkodade, vilket endast visar de parametrar som vi sannolikt kommer att justera: filnamn och kvalitet. Detta tillvägagångssätt förbättrar läsbarheten och gör det enklare att spara bilder.Dessutom, om Lumis exportör ändras i framtiden behöver vi bara uppdatera den här funktionen istället för att modifiera varje skript som exporterar en JPEG.

### Använda omslaget

För att exportera en JPEG i våra plugin-program inkluderar vi helt enkelt biblioteket och anropar vår anpassade funktion:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Detta håller vår kod ren, läsbar och anpassningsbar samtidigt som vi kan exportera JPEG-filer effektivt med minimal ansträngning.

### Bilbyte

***bil***-funktionen kan vara kryptisk och benägen för skriptfel. Det är lätt att av misstag applicera ***bil*** på en vektor eller ett objekt som inte finns på listan, vilket leder till oväntat beteende. För att göra vår kod mer robust och läsbar kan vi slå in den här funktionen i en säkrare funktion.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Den här funktionen hämtar säkert det första objektet i en lista eller vektor samtidigt som den ger användbara varningar när ogiltiga eller tomma ingångar påträffas. Genom att använda ***första objekt*** istället för ***bil*** minskar vi risken för oavsiktliga fel och förbättrar klarheten i våra skript.

#### Varför använda detta omslag?

- **Förhindrar skriptkrascher** - Undviker fel som orsakas av att ***bil*** tillämpas på icke-listor.
- **Stöder både listor och vektorer** - Utökar användbarheten utöver bara listor.
- **Ger meningsfulla varningar** - Hjälper till att felsöka oväntade inmatningsproblem.
- **Förbättrar läsbarheten** – Funktionsnamnet förmedlar tydligt dess syfte.

Genom att kapsla in denna logik i första objektet gör vi våra plug-ins mer robusta och lättare att underhålla. Naturligtvis beror detta på personliga preferenser, du kan vara helt bekväm med att använda bil-, caar-, cadr- och liknande Scheme-funktioner direkt.

### Slå in en inslagen funktion

Att radbryta en funktion som redan är packad kan ytterligare förbättra läsbarheten och underhållbarheten. Till exempel, när vi arbetar med koordinatpar som ***pixel-koordinater (lista 100 200)*** kan vi använda:

```scheme
(first-item pixel-coords)
```

för att hämta ***x***-koordinaten. Men även om det är funktionellt är det inte särskilt uttrycksfullt. Istället kan vi slå in ***första objekt*** i en mer lämplig definition för att göra vår avsikt tydligare.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Varför använda denna metod?

- **Förbättrar kodtydligheten** – Istället för att använda generiska liståtkomstfunktioner definierar vi uttryckligen funktioner som beskriver deras syfte.
- **Förbättrar underhållsbarheten** - Om vår koordinatrepresentation ändras (t.ex. genom att använda vektorer istället för listor), behöver vi bara uppdatera dessa små funktioner.
- **Uppmuntrar konsistens** – Genom att använda ***x-coord*** och ***y-coord*** blir skriptet lättare att läsa och förstå med ett ögonkast.

Nu, istället för att skriva i generiskt schema:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Vi kan skriva i _vårt_ Schema:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Genom att slå in lågnivåfunktioner i meningsfulla namn skapar vi ett mer intuitivt sätt att arbeta med data, vilket minskar förvirring och potentiella fel.

### Slutsats

Omslagsfunktioner är ett kraftfullt sätt att förenkla utvecklingen av schemat, vilket gör skript mer läsbara, underhållbara och robusta. Genom att kapsla in komplexitet och blottlägga nödvändiga detaljer skapar vi ett mer strukturerat tillvägagångssätt för att skriva plug-ins.

Viktiga tips från detta tillvägagångssätt:- **Förenklar repetitiva uppgifter** - Istället för att manuellt upprepa kommandon på låg nivå skapar vi återanvändbara funktioner.
- **Förbättrar kodläsbarheten** - Välnamnda omslag gör skript lättare att förstå.
- **Inkapslar komplexitet** - Lågnivådetaljer hanteras inuti omslaget, vilket håller huvudskriptet rent.
- **Förbättrar underhållsbarheten** – Om kärnfunktionaliteten ändras behöver vi bara uppdatera omslaget, inte alla skript som är beroende av det.
- **Uppmuntrar återanvändning och konsekvens** - Vårt personliga bibliotek med funktioner växer över tiden, vilket gör utvecklingen snabbare och mer effektiv.

Genom att konsekvent använda funktionsomslutning kan vi förändra hur vi skriver Scheme-plugin-program, vilket skapar en mer modulär och uttrycksfull skriptmiljö. Med dessa principer i åtanke kan vi fortsätta att förfina vårt tillvägagångssätt och utveckla en mer effektiv och skräddarsydd version av Scheme som möter våra specifika behov.

Nästa steg: identifiera upprepade block i dina skript och extrahera små hjälpare med tydliga namn.