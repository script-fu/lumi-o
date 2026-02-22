---
title: "Inpakken"
type: docs
weight: 4
---
Schemaopdrachten werken op een laag niveau, wat betekent dat zelfs eenvoudige taken meerdere stappen kunnen vereisen. Deze granulariteit biedt echter flexibiliteit: we kunnen opdrachten bundelen in kleine, herbruikbare functies die precies doen wat we nodig hebben. Inpakken is geen zwart-witconcept; het kan variëren van eenvoudige aliassen voor veelgebruikte opdrachten tot complexere functies die volledige workflows beheren. Soms is een wrapper slechts een gemaksfunctie om de leesbaarheid te verbeteren, terwijl deze in andere gevallen evolueert naar een volledig uitgerust hulpprogramma dat meerdere bewerkingen omvat.

### Waarom Wrap-functies?

Er zijn verschillende belangrijke voordelen verbonden aan het inpakken van functies:

- **Vereenvoudigt repetitieve taken** – In plaats van opdrachten op laag niveau te herhalen, kunt u ze in een helperfunctie verpakken en deze opnieuw gebruiken.
- **Verbetert de leesbaarheid** – Door onze verpakte functies duidelijke, beschrijvende namen te geven, is onze code in één oogopslag gemakkelijker te begrijpen.
- **Bevat complexiteit** – In plaats van te werken met lange, cryptische lijsten met opdrachten, diep geneste lussen of complexe berichtinstructies, kunnen we ze opsplitsen in kleinere, goed gestructureerde hulpfuncties.
- **Verbetert de onderhoudbaarheid** – Als de kernfunctionaliteit van een opdracht verandert, hoeven we onze ingepakte functie slechts één keer bij te werken, waardoor onze plug-ins worden geïsoleerd van de details van die wijzigingen.
- **Stimuleert hergebruik van code** – Elke helper wordt onderdeel van uw bibliotheek, waardoor toekomstige scripts sneller kunnen worden geschreven en gedebugd.

Naarmate uw plug-ins groeien, helpen wrappers u de kernlogica leesbaar te houden en repetitieve details te isoleren.

Een ander voordeel van inpakfuncties is de integratie ervan in een syntaxismarkeerstift zoals Visual Studio Code. Dit verbetert de leesbaarheid en navigatie, waardoor scripts duidelijker worden. In een plug-in die aangepaste functies gebruikt, bevestigt elke groen gemarkeerde functie dat er correct naar wordt verwezen vanuit onze bibliotheek.

Als u uw eigen helperbibliotheek onderhoudt, overweeg dan om de functienamen van uw project toe te voegen aan de syntaxisaccentuering van uw editor. Het maakt navigatie en refactoring sneller.

Voorbeelden:

### Willekeurig zaad

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Hoewel we ***msrg-rand*** rechtstreeks in onze code zouden kunnen gebruiken, verbetert het inpakken ervan in een functie met de naam ***random-seed*** de leesbaarheid. Door de functie een duidelijke en beschrijvende naam te geven, wordt het gemakkelijker om het doel ervan in één oogopslag te begrijpen.

Bovendien stelt het definiëren van ***random-seed*** als een zelfstandige functie ons in staat deze overal in onze plug-ins te gebruiken, terwijl de implementatie op één locatie wordt gecentraliseerd. Als we ooit de manier moeten veranderen waarop het zaad wordt gegenereerd, hoeven we alleen deze functie bij te werken, waarbij de rest van onze code onaangetast blijft.

Als we bijvoorbeeld besluiten om in plaats daarvan over te schakelen naar ***willekeurig***:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

De functienaam blijft hetzelfde, waardoor onze scripts zonder aanpassingen blijven werken. Deze aanpak houdt onze code flexibel, onderhoudbaar en gemakkelijk te lezen.

### JPEG-exporteren

De JPEG-exportfunctie in Scheme wordt geleverd met veel parameters, die nauwkeurige controle bieden over hoe afbeeldingen worden opgeslagen. In de meeste gevallen zijn we echter slechts geïnteresseerd in een paar belangrijke instellingen, zoals bestandsnaam en kwaliteit. Om het proces te vereenvoudigen, kunnen we de functie inpakken.

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

In deze wrapperfunctie zijn de meeste exportopties hardgecodeerd, waardoor alleen de parameters zichtbaar zijn die we waarschijnlijk zullen aanpassen: bestandsnaam en kwaliteit. Deze aanpak verbetert de leesbaarheid en maakt het opslaan van afbeeldingen eenvoudiger.Als de exporteur van Lumi in de toekomst verandert, hoeven we bovendien alleen deze ene functie bij te werken in plaats van elk script aan te passen dat een JPEG exporteert.

### De verpakking gebruiken

Om een JPEG in onze plug-ins te exporteren, voegen we eenvoudigweg de bibliotheek toe en roepen we onze aangepaste functie aan:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Hierdoor blijft onze code schoon, leesbaar en aanpasbaar, terwijl we JPEG's efficiënt en met minimale inspanning kunnen exporteren.

### Autovervanging

De ***car***-functie kan cryptisch zijn en gevoelig voor scriptfouten. Het is gemakkelijk om per ongeluk ***auto*** toe te passen op een vector of een item dat niet in de lijst voorkomt, wat tot onverwacht gedrag leidt. Om onze code robuuster en leesbaarder te maken, kunnen we deze functionaliteit in een veiligere functie verpakken.

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

Deze functie haalt veilig het eerste item van een lijst of vector op en geeft nuttige waarschuwingen wanneer ongeldige of lege invoer wordt aangetroffen. Door ***first-item*** te gebruiken in plaats van ***car*** verminderen we het risico op onbedoelde fouten en verbeteren we de duidelijkheid van onze scripts.

#### Waarom deze verpakking gebruiken?

- **Voorkomt scriptcrashes** – Voorkomt fouten veroorzaakt door het toepassen van ***car*** op niet-lijsten.
- **Ondersteunt zowel lijsten als vectoren** – Breidt de bruikbaarheid verder uit dan alleen lijsten.
- **Biedt betekenisvolle waarschuwingen** – Helpt onverwachte invoerproblemen op te lossen.
- **Verbetert de leesbaarheid** – De functienaam geeft duidelijk het doel ervan weer.

Door deze logica in first-item te verwerken, maken we onze plug-ins robuuster en gemakkelijker te onderhouden. Dit komt natuurlijk neer op persoonlijke voorkeur. Het kan zijn dat u zich helemaal op uw gemak voelt bij het rechtstreeks gebruiken van auto-, caar-, cadr- en soortgelijke Scheme-functies.

### Een verpakte functie inpakken

Het inpakken van een functie die al is ingepakt, kan de leesbaarheid en onderhoudbaarheid verder verbeteren. Als we bijvoorbeeld werken met coördinatenparen zoals ***pixel-coördinaten (lijst 100 200)***, kunnen we het volgende gebruiken:

```scheme
(first-item pixel-coords)
```

om de ***x***-coördinaat op te halen. Hoewel functioneel, is dit echter niet erg expressief. In plaats daarvan kunnen we ***eerste item*** in een geschiktere definitie verpakken om onze bedoeling duidelijker te maken.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Waarom deze aanpak gebruiken?

- **Verbetert de duidelijkheid van de code** – In plaats van algemene lijsttoegangsfuncties te gebruiken, definiëren we expliciet functies die hun doel beschrijven.
- **Verbetert de onderhoudbaarheid** – Als onze coördinatenweergave verandert (bijvoorbeeld door vectoren te gebruiken in plaats van lijsten), hoeven we alleen deze kleine functies bij te werken.
- **Stimuleert consistentie** – Door ***x-coord*** en ***y-coord*** te gebruiken, is het script in één oogopslag gemakkelijker te lezen en te begrijpen.

Nu, in plaats van in het algemene schema te schrijven:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

We kunnen in _ons_schema schrijven:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Door functies op laag niveau in betekenisvolle namen te verpakken, creëren we een meer intuïtieve manier om met gegevens te werken, waardoor verwarring en potentiële fouten worden verminderd.

### Verzonden wrappers: het hulpprogramma Stdlib

Lumi levert een set kant-en-klare wrappers die automatisch worden geladen bij het opstarten, zodat ze beschikbaar zijn in elke plug-in of in de Scheme Console zonder enige `(load ...)` aanroep. Deze bibliotheken (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` en `paths.scm`) zijn gebouwd op precies hetzelfde principe als de bovenstaande voorbeelden: ze geven duidelijke namen aan bewerkingen op laag niveau, verbergen repetitieve standaardtekst, en zorg voor één plek waar u kunt bijwerken als de onderliggende opdracht verandert.`images.scm` biedt bijvoorbeeld `image-get-open-list` als een leesbare wrapper rond de onbewerkte PDB-aanroep, en `files.scm` biedt hulp bij het bouwen van paden waarvoor anders herhaalde `string-append`-ketens nodig zouden zijn.

U kunt door elke geëxporteerde naam bladeren, de bijbehorende docstring lezen en zien uit welke bibliotheek deze afkomstig is in **[Utility Browser](@@LUMI_TOKEN_21@@)** (Help → Programmeren → Hulpprogrammabrowser). Het is een praktische demonstratie van inpakken op schaal, en een nuttige bron van patronen die u kunt lenen bij het bouwen van uw eigen helperbibliotheek.

### Conclusie

Wrapping-functies zijn een krachtige manier om de ontwikkeling van schema's te vereenvoudigen, waardoor scripts leesbaarder, onderhoudbaarder en robuuster worden. Door de complexiteit in te kapselen en alleen de noodzakelijke details bloot te leggen, creëren we een meer gestructureerde aanpak voor het schrijven van plug-ins.

Belangrijkste conclusies uit deze aanpak:

- **Vereenvoudigt repetitieve taken** – In plaats van handmatig opdrachten op laag niveau te herhalen, creëren we herbruikbare functies.
- **Verbetert de leesbaarheid van de code** – Wrappers met een goede naam maken scripts gemakkelijker te begrijpen.
- **Bevat complexiteit** – Details op laag niveau worden binnen de wrapper afgehandeld, waardoor het hoofdscript schoon blijft.
- **Verbetert de onderhoudbaarheid** – Als de kernfunctionaliteit verandert, hoeven we alleen de wrapper bij te werken, niet elk script dat ervan afhankelijk is.
- **Stimuleert hergebruik en consistentie** – Onze persoonlijke bibliotheek met functies groeit in de loop van de tijd, waardoor de ontwikkeling sneller en efficiënter wordt.

Door consistent gebruik te maken van function packing kunnen we de manier waarop we Scheme-plug-ins schrijven transformeren, waardoor een meer modulaire en expressieve scriptomgeving ontstaat. Met deze principes in gedachten kunnen we onze aanpak blijven verfijnen en een efficiëntere en op maat gemaakte versie van Scheme ontwikkelen die aan onze specifieke behoeften voldoet.

Volgende stappen: identificeer herhaalde blokken in uw scripts en extraheer kleine helpers met duidelijke namen.