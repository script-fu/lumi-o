---
title: "Omslag"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7b176d9b546b2566812e825fb2e10da5dd4e86f0e79be2c362a4775546110ac6
url: "hub/scripting/tutorials/Wrapping/wrapping"
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

### Slumpfrö

```scheme
;; Syfte: Returnerar ett slumpmässigt heltal för att seeda ett filter
(define (random-seed)
  (msrg-rand))
```

Även om vi skulle kunna använda ***msrg-rand*** direkt i vår kod, förbättrar vi läsbarheten genom att linda in den i en funktion som kallas ***random-seed***. Genom att ge funktionen ett tydligt och beskrivande namn blir det lättare att förstå dess syfte med ett ögonkast.

Dessutom, genom att definiera ***random-seed*** som en fristående funktion kan vi använda den var som helst i våra plugin-program samtidigt som vi centraliserar implementeringen på en enda plats. Om vi någonsin behöver ändra hur fröet genereras behöver vi bara uppdatera den här funktionen och lämna resten av vår kod orörd.

Om vi till exempel bestämmer oss för att byta till ***random*** istället:

```scheme
;; Syfte: Returnerar ett slumpmässigt heltal för att seeda ett filter
(define (random-seed)
  (random 1000))
```

Funktionsnamnet förblir detsamma, vilket säkerställer att våra skript fortsätter att fungera utan ändringar. Detta tillvägagångssätt håller vår kod flexibel, underhållbar och lätt att läsa.

### JPEG exporterar

JPEG-exportfunktionen i Scheme kommer med många parametrar, vilket ger fin kontroll över hur bilder sparas. Men i de flesta fall bryr vi oss bara om ett fåtal nyckelinställningar, som filnamn och kvalitet. För att förenkla processen kan vi slå in funktionen.

```scheme
;; Syfte: Sparar en bild som JPEG med angiven kvalitet
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Undvik jpg.jpg
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

För att exportera JPEG i våra tillägg inkluderar vi helt enkelt biblioteket och anropar vår anpassade funktion:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Det håller koden ren, läsbar och anpassningsbar samtidigt som vi kan exportera JPEG-filer effektivt med minimal ansträngning.

### Ersätta `car`

Funktionen ***car*** kan vara kryptisk och benägen att ge skriptfel. Det är lätt att av misstag använda ***car*** på en vektor eller ett icke-listobjekt, vilket leder till oväntat beteende. För att göra koden mer robust och läsbar kan vi omsluta funktionen i en säkrare funktion.

```scheme
;; Syfte: Returnerar det första objektet i en lista eller vektor.
;;          Varnar om indata är ogiltig eller tom.
(define (first-item collection)
  (cond
    ;; Hanterar icke-tomma listor
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Hanterar icke-tomma vektorer
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Ogiltig eller tom indata
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Funktionen hämtar säkert det första elementet i en lista eller vektor och ger tydliga varningar vid ogiltiga eller tomma indata. Genom att använda ***first-item*** i stället för ***car*** minskar vi risken för misstag och gör skripten tydligare.

#### Varför använda detta omslag?

- **Förhindrar skriptkrascher** – Undviker fel orsakade av ***car*** på icke-listor.
- **Stöder både listor och vektorer** - Utökar användbarheten utöver bara listor.
- **Ger tydliga varningar** – Hjälper till att felsöka oväntade indata.
- **Förbättrar läsbarheten** – Funktionsnamnet förklarar tydligt syftet.

Genom att kapsla in logiken i first-item gör vi våra tillägg mer robusta och enklare att underhålla. Naturligtvis beror detta på personliga preferenser, du kan vara helt bekväm med att använda bil-, caar-, cadr- och liknande Scheme-funktioner direkt.

### Slå in en inslagen funktion

Att omsluta en funktion som redan är omsluten kan ytterligare förbättra läsbarheten och underhållbarheten. När vi till exempel arbetar med koordinatpar som ***pixel-coords (list 100 200)*** kan vi använda:

```scheme
(first-item pixel-coords)
```

för att hämta ***x***-koordinaten. Det fungerar, men är inte särskilt uttrycksfullt. I stället kan vi omsluta ***first-item*** i en mer passande definition som gör vår avsikt tydligare.

```scheme
;; Syfte: Returnerar x-koordinaten, för läsbarhet
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Syfte: Returnerar y-koordinaten, för läsbarhet
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Varför använda detta tillvägagångssätt?

- **Förbättrar kodtydligheten** – I stället för generiska liståtkomstfunktioner definierar vi funktioner som beskriver sitt syfte.
- **Förbättrar underhållsbarheten** - Om vår koordinatrepresentation ändras (t.ex. genom att använda vektorer istället för listor), behöver vi bara uppdatera dessa små funktioner.
- **Uppmuntrar konsistens** – Genom att använda ***x-coord*** och ***y-coord*** blir skriptet lättare att läsa och förstå med ett ögonkast.

Nu, istället för att skriva i generiskt schema:

```scheme
(car pixel-coords) ;; Hämtar x-koordinaten
(cadr pixel-coords) ;; Hämtar y-koordinaten
```

Vi kan skriva i _vårt_ Scheme:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Genom att omsluta funktioner på låg nivå med meningsfulla namn får vi ett mer intuitivt sätt att arbeta med data och minskar risken för missförstånd och fel.

### Medföljande wrappers: Utility Stdlib

Lumi skickar en uppsättning färdiga omslag som laddas automatiskt vid start, så de är tillgängliga i alla plugin-program eller i Scheme Console utan något `(load ...)`-samtal. Dessa bibliotek (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm`, och `gegl.scm`, de är byggda på exakt samma namn som ovanstående exempel på: principen ovan: de är uppbyggda på exakt samma namn som ovan: operationer på låg nivå, dölj repetitiva plattor och tillhandahålla en enda plats att uppdatera om det underliggande kommandot ändras.Till exempel ger `images.scm` `image-get-open-list` som ett läsbart omslag runt det råa PDB-anropet, och `files.scm` avslöjar vägbyggande hjälpare som annars skulle kräva upprepade `string-append`-kedjor.

Du kan bläddra i alla exporterade namn, läsa dess docstring och se vilket bibliotek det kommer från i **[Utility Browser]({{< ref "/hub/scripting/reference/utility-browser" >}})** (Hjälp → Programmering → Utility Browser). Det är en praktisk demonstration av omslag i stor skala och en användbar källa till mönster att låna när du bygger ditt eget hjälpbibliotek.

### Slutsats

Omslagsfunktioner är ett kraftfullt sätt att förenkla utvecklingen av schemat, vilket gör skript mer läsbara, underhållbara och robusta. Genom att kapsla in komplexitet och blottlägga nödvändiga detaljer skapar vi ett mer strukturerat tillvägagångssätt för att skriva plug-ins.

Viktiga tips från detta tillvägagångssätt:

- **Förenklar repetitiva uppgifter** - Istället för att manuellt upprepa kommandon på låg nivå skapar vi återanvändbara funktioner.
- **Förbättrar kodläsbarheten** - Välnamnda omslag gör skript lättare att förstå.
- **Inkapslar komplexitet** - Lågnivådetaljer hanteras inuti omslaget, vilket håller huvudskriptet rent.
- **Förbättrar underhållsbarheten** – Om kärnfunktionaliteten ändras behöver vi bara uppdatera omslaget, inte alla skript som är beroende av det.
- **Uppmuntrar återanvändning och konsekvens** - Vårt personliga bibliotek med funktioner växer över tiden, vilket gör utvecklingen snabbare och mer effektiv.

Genom att konsekvent använda funktionsomslutning kan vi förändra hur vi skriver Scheme-plugin-program, vilket skapar en mer modulär och uttrycksfull skriptmiljö. Med dessa principer i åtanke kan vi fortsätta att förfina vårt tillvägagångssätt och utveckla en mer effektiv och skräddarsydd version av Scheme som möter våra specifika behov.

Nästa steg: identifiera upprepade block i dina skript och extrahera små hjälpare med tydliga namn.