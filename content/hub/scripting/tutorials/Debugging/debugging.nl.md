---
title: "Foutopsporing"
type: docs
weight: 5
---
Bij scripting is geen enkele functie onfeilbaar. Zelfs de meest betrouwbare opdrachten kunnen mislukken als ze worden geconfronteerd met onverwachte invoer of omstandigheden. Om ons hiertegen te beschermen, kunnen we een aangepast foutopsporingssysteem implementeren en defensieve programmeertechnieken toepassen. Door standaardfuncties te voorzien van mechanismen voor foutafhandeling en informatieve feedback te geven, kunnen we onze scripts robuuster maken en gemakkelijker problemen op te lossen.

Een belangrijk onderdeel van deze strategie is het gebruik van een globale debug-vlag om uitgebreide uitvoer te controleren, waardoor we gedetailleerde debug-informatie kunnen inschakelen wanneer dat nodig is, terwijl de uitvoer schoon blijft tijdens normale uitvoering.

## Globale foutopsporingsvlag

Een globale foutopsporingsvlag is een eenvoudige maar effectieve manier om het niveau van de informatie-uitvoer tijdens de uitvoering van scripts te controleren. Indien ingeschakeld, biedt het gedetailleerde foutopsporingsberichten die van onschatbare waarde kunnen zijn bij het opsporen van problemen. Indien uitgeschakeld, blijft de uitvoer beknopt voor productiegebruik.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

Standaard is foutopsporing uitgeschakeld. Om uitgebreide uitvoer tijdens de ontwikkeling mogelijk te maken, stelt u eenvoudigweg de vlag in op `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

We kunnen foutopsporing voor specifieke delen van de code ook tijdelijk in- of uitschakelen met behulp van helperfuncties.

### Lokaal foutopsporingsbeheer

Voor een nauwkeurigere controle kunnen we foutopsporing binnen specifieke delen van het script in- of uitschakelen met behulp van helperfuncties.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Hierdoor kunnen we de foutopsporing dynamisch beheren:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Foutopsporing in berichtensysteem

Om de debug-uitvoer in Scheme efficiënt af te handelen, gebruiken we een gestructureerde aanpak met meerdere helperfuncties. Deze functies zorgen ervoor dat foutopsporings- en waarschuwingsberichten duidelijk, leesbaar en onderhoudbaar zijn.

### Overzicht van het debug-berichtensysteem

Ons debug-berichtensysteem bestaat uit de volgende componenten:

1. `debug-message` – Geeft foutopsporingsberichten weer wanneer foutopsporing is ingeschakeld.
2. `serialize-item` – Converteert verschillende schemagegevenstypen naar een tekenreeksrepresentatie.
3. `concat` – Voegt meerdere items samen tot één enkele string.
4. `list->string` – Formatteert een lijst in een leesbare tekenreeks.
5. `message` – Geeft de uitvoer weer in de berichtenconsole van Lumi.
6. `warning-message` – Geeft waarschuwingsberichten weer wanneer waarschuwingen zijn ingeschakeld.

Elke functie speelt een rol bij het opmaken en weergeven van gestructureerde berichten.

---

### Functie voor foutopsporingsbericht

De functie `debug-message` is de kernmethode voor het weergeven van debug-uitvoer. Het zorgt ervoor dat berichten alleen worden weergegeven als foutopsporing is ingeschakeld.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- De voorwaarde `when debug` zorgt ervoor dat berichten alleen verschijnen als foutopsporing is ingeschakeld.
- Berichten worden voor de duidelijkheid voorafgegaan door `"> "`.
- De functie gebruikt `concat` om de berichtinhoud op te maken.
- Ten slotte roept het `message` aan om de uitvoer naar de berichtenconsole van Lumi te sturen.

Voorbeeldgebruik:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Als foutopsporing is ingeschakeld, kan de uitvoer het volgende zijn:

```scheme
> item: background-layer has tree position : 3
```

### Gegevens serialiseren voor foutopsporingsberichten

Berichten kunnen verschillende gegevenstypen bevatten, zoals lijsten, vectoren en getallen. Om ervoor te zorgen dat ze correct zijn opgemaakt, gebruiken we `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Voorbeeldgebruik:

```scheme
(serialize-item '(1 2 3))
```

Uitgang:

```scheme
list:
1
2
3
```

### Aaneenschakeling voor berichten

Om meerdere berichtcomponenten samen te voegen tot één enkele string, gebruiken we `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Voorbeeldgebruik:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Lijsten opmaken als tekenreeksen

De functie `list->string` converteert een lijst naar een opgemaakte tekenreeks.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### WaarschuwingsberichtenDe functie `warning-message` werkt op dezelfde manier als `debug-message`, maar geeft waarschuwingen weer, zelfs als foutopsporing is uitgeschakeld.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Zorgt ervoor dat berichten alleen worden weergegeven als waarschuwingen zijn ingeschakeld (de vlag `warning` is ingesteld in `common.scm` als `#t`).
- Roept `concat` op om de berichtinhoud op te maken.
- Gebruikt `message` om uitvoer naar Lumi te sturen.

## Verbetering van standaardfuncties

Zodra er een foutopsporingssysteem aanwezig is, kunnen we onze bibliotheek met functies uitbreiden door gedetailleerde berichten op te nemen. Dit geeft inzicht in itemstatussen, variabelewaarden en functieaanroepen.

Een veelvoorkomend voorbeeld is `item-is-valid?`, waarbij `lumi-item-id-is-valid` wordt verpakt om `#t` of `#f` te retourneren. Als `#f` wordt geretourneerd, kunnen we een `warning-message` activeren in de aanroepcode. Als de invoer geen getal is, kunnen we een waarschuwing geven in de functie.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Praktisch gebruik

Bij het ontwikkelen van Scheme-plug-ins vermindert het op deze manier inpakken van functies de tijd voor foutopsporing aanzienlijk en zorgt voor robuuste, onderhoudbare code. Met ons foutopsporingssysteem kunnen we met één druk op de knop een gestructureerde foutopsporingsstroom genereren in de foutconsole.

In deze debug-stroom worden functieaanroepen gemarkeerd met een asterisk (*), waardoor het eenvoudiger wordt de uitvoering van scripts te volgen en fouten op te sporen, vooral bij complexe plug-ins. Deze zichtbaarheid helpt ons de stroom van activiteiten te begrijpen en onverwacht gedrag efficiënt te diagnosticeren.

Een wrapper voor onze berichtfunctie om een `*` te gebruiken

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Voorbeeld van `call` in de praktijk:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Voorbeeld van een debug-stream terwijl een plug-in wordt uitgevoerd:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Dit gestructureerde logboek biedt een duidelijke tijdlijn van functieaanroepen en gegevenswijzigingen, waardoor foutopsporing en prestatieanalyse aanzienlijk eenvoudiger worden.

## Conclusie

Door een gestructureerd foutopsporingssysteem te implementeren, creëren we veiligere, beter onderhoudbare scripts die realtime inzicht bieden in de uitvoering ervan.

### Belangrijkste afhaalrestaurants

- **Beheer de breedsprakigheid** – Gebruik een globale foutopsporingsvlag om de uitvoerniveaus te beheren.
- **Geef duidelijke feedback** – Verpak standaardfuncties met informatieve foutopsporingsberichten.
- **Verbeter de robuustheid** – Ga op een elegante manier om met onverwachte invoer om fouten te voorkomen.
- **Vereenvoudig het oplossen van problemen** – Gestructureerde foutopsporingsberichten maken het gemakkelijker om problemen te diagnosticeren en op te lossen.

Met deze aanpak 'leggen onze scripts zichzelf effectief uit' terwijl ze gegevens verwerken, waardoor frustratie wordt verminderd en de efficiëntie van de workflow wordt verbeterd. Debuggen wordt een proactief hulpmiddel in plaats van een reactief karwei, waardoor ons scriptproces soepeler en lonender wordt.