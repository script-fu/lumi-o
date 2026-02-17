---
title: "Felsökning"
type: docs
weight: 5
---
I scripting är ingen funktion ofelbar. Även de mest tillförlitliga kommandona kan misslyckas när de ställs inför oväntade inmatningar eller förhållanden. För att skydda oss mot detta kan vi implementera ett anpassat felsökningssystem och använda defensiva programmeringstekniker. Genom att slå in standardfunktioner med felhanteringsmekanismer och ge informativ feedback kan vi göra våra skript mer robusta och lättare att felsöka.

En viktig del av denna strategi är att använda en global felsökningsflagga för att kontrollera utförlig utdata, vilket gör att vi kan aktivera detaljerad felsökningsinformation när det behövs samtidigt som utdata hålls rent under normal körning.

## Global felsökningsflagga

En global felsökningsflagga är ett enkelt men effektivt sätt att kontrollera nivån på informationsutmatning under skriptkörning. När den är aktiverad ger den detaljerade felsökningsmeddelanden som kan vara ovärderliga för att spåra problem. När den är inaktiverad håller den utgången kortfattad för produktionsanvändning.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

Som standard är felsökning avstängd. För att aktivera utförlig utdata under utveckling, ställ helt enkelt in flaggan till `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

Vi kan också tillfälligt aktivera eller inaktivera felsökning för specifika avsnitt av kod med hjälp av hjälpfunktioner.

### Lokal felsökningskontroll

För bättre kontroll kan vi aktivera eller inaktivera felsökning inom specifika delar av skriptet med hjälp av hjälpfunktioner.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Detta gör att vi kan styra felsökningen dynamiskt:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Felsök meddelandesystem

För att effektivt hantera felsökningsutdata i Scheme använder vi ett strukturerat tillvägagångssätt som involverar flera hjälpfunktioner. Dessa funktioner säkerställer att felsöknings- och varningsmeddelanden är tydliga, läsbara och underhållsbara.

### Översikt över Debug Messaging System

Vårt felsökningssystem består av följande komponenter:

1. `debug-message` – Visar felsökningsmeddelanden när felsökning är aktiverad.
2. `serialize-item` – Konverterar olika schemadatatyper till en strängrepresentation.
3. `concat` – Sammanfogar flera objekt till en enda sträng.
4. `list->string` – Formaterar en lista till en läsbar sträng.
5. `message` – Visar utdata i Lumis meddelandekonsol.
6. `warning-message` – Visar varningsmeddelanden när varningar är aktiverade.

Varje funktion spelar en roll vid formatering och visning av strukturerade meddelanden.

---

### Felsökningsmeddelandefunktion

`debug-message`-funktionen är kärnmetoden för att visa felsökningsutdata. Det säkerställer att meddelanden endast visas när felsökning är aktiverad.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug`-villkoret säkerställer att meddelanden endast visas när felsökning är aktiverad.
- Meddelanden har prefixet `"> "` för tydlighetens skull.
- Funktionen använder `concat` för att formatera meddelandeinnehåll.
- Slutligen anropar den `message` för att skicka utdata till Lumis meddelandekonsol.

Exempel på användning:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Med felsökning aktiverad kan utdata vara:

```scheme
> item: background-layer has tree position : 3
```

### Serialisera data för felsökningsmeddelanden

Meddelanden kan innehålla olika datatyper som listor, vektorer och siffror. För att säkerställa att de är korrekt formaterade använder vi `serialize-item`.

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

Exempel på användning:

```scheme
(serialize-item '(1 2 3))
```

Utdata:

```scheme
list:
1
2
3
```

### Sammansättning för meddelanden

För att slå samman flera meddelandekomponenter till en enda sträng använder vi `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Exempel på användning:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formatera listor som strängar

Funktionen `list->string` konverterar en lista till en formaterad sträng.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Varningsmeddelanden`warning-message`-funktionen fungerar på samma sätt som `debug-message`, men den visar varningar även när felsökning är inaktiverad.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Säkerställer att meddelanden endast visas när varningar är aktiverade (`warning`-flaggan är inställd i `common.scm` som `#t`).
- Ringer `concat` för att formatera meddelandeinnehåll.
- Använder `message` för att skicka utdata till Lumi.

## Förbättring av standardfunktioner

När ett felsökningssystem väl är på plats kan vi förbättra vårt bibliotek med funktioner genom att inkludera detaljerade meddelanden. Detta ger insikt i objekttillstånd, variabelvärden och funktionsanrop.

Ett vanligt exempel är `item-is-valid?`, som omsluter `lumi-item-id-is-valid` för att returnera `#t` eller `#f`. Om `#f` returneras kan vi trigga ett `warning-message` i anropskoden, om inmatningen inte är ett nummer kan vi ge en varning i funktionen.

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

## Praktisk användning

När du utvecklar Scheme-plugin-program, minskar inpackningsfunktioner på detta sätt avsevärt felsökningstiden och säkerställer robust, underhållbar kod. Med vårt felsökningssystem på plats kan vi generera en strukturerad felsökningsström i felkonsolen med en knapptryckning.

I den här felsökningsströmmen är funktionsanrop markerade med en asterisk (*), vilket gör det lättare att spåra skriptkörning och lokalisera fel, särskilt i komplexa plugin-program. Denna synlighet hjälper oss att förstå flödet av operationer och diagnostisera oväntade beteenden effektivt.

Ett omslag för vår meddelandefunktion för att använda en `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Exempel på att `call` används i praktiken:

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

Exempel på en felsökningsström när ett plugin-program körs:

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

Denna strukturerade logg ger en tydlig tidslinje för funktionsanrop och dataändringar, vilket gör felsökning och prestandaanalys betydligt enklare.

## Slutsats

Genom att implementera ett strukturerat felsökningssystem skapar vi säkrare, mer underhållbara skript som ger realtidsinsikter om hur de körs.

### Nyckelalternativ

- **Kontrollera detaljnivå** - Använd en global felsökningsflagga för att hantera utdatanivåer.
- **Ge tydlig feedback** - Omslut standardfunktioner med informativa felsökningsmeddelanden.
- **Förbättra robustheten** - Hantera oväntade inmatningar elegant för att förhindra fel.
- **Förenkla felsökning** - Strukturerade felsökningsmeddelanden gör det lättare att diagnostisera och åtgärda problem.

Med detta tillvägagångssätt "förklarar våra skript sig själva" effektivt när de bearbetar data, vilket minskar frustration och förbättrar arbetsflödets effektivitet. Felsökning blir ett proaktivt verktyg snarare än en reaktiv syssla, vilket gör vår skriptprocess både smidigare och mer givande.