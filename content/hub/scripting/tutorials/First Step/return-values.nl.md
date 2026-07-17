---
title: "Waarden retourneren"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 80a8f61c6fc7f6b86167f7489f61558b49f3d1d2b7e1e5236406cbca31ff611e
---
Retourwaarden zijn belangrijk omdat u hiermee de stroom kunt controleren zonder extra status. In Schema wordt de laatst geëvalueerde expressie de retourwaarde.

Deze pagina gebruikt de validatiehulpmiddelen uit het berichtenvoorbeeld om te laten zien hoe expliciete retourwaarden het samenstellen van code eenvoudiger maken.

### Wat is een retourwaarde?

In Schema wordt de retourwaarde van een functie bepaald door de laatste expressie die de functie evalueert. Dit betekent dat wat de laatste regel code in de functie ook evalueert, zal worden geretourneerd als resultaat van de functie. Als er expliciet geen waarde wordt geretourneerd, retourneert de functie `#f` (false) of `undefined`.

Laten we de validatiefunctie opnieuw bekijken (is-valid-string?)

```scheme
;; Doel: Controleert dat het bericht een niet-lege tekenreeks is
(define (is-valid-string? message)
  ;; Controleren of het bericht een niet-lege tekenreeks is
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Als het bericht ongeldig is, wordt er in deze functie een fout gegenereerd. Als het bericht echter geldig is, wordt er geen expliciete retourwaarde gegeven en retourneert de functie standaard `#f`.

### Retourwaarden expliciet maken

Dit kunnen we verbeteren door de retourwaarde explicieter te maken. We kunnen bijvoorbeeld `#t` (waar) retourneren als het bericht geldig is:

```scheme
;; Doel: Controleert dat het bericht naar een geldige uitvoer wordt gestuurd
(define (is-valid-output-display? output)
  ;; Controleren of de uitvoer een van de verwachte weergavebestemmingen is
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

In deze versie retourneert de functie `#t` wanneer het bericht geldig is, wat een duidelijk resultaat oplevert. Hierdoor kan de functie flexibeler worden gebruikt in andere contexten waar een Booleaans resultaat nodig is.

### Retourwaarden effectief gebruiken

Door te beslissen wat onze functies retourneren, kunnen we ze voorspelbaarder en nuttiger maken. Het retourneren van waarden zoals `#t`, `#f`, of een specifiek resultaat geeft ons meer controle over hoe de functie samenwerkt met de rest van de code. U kunt de retourwaarde bijvoorbeeld gebruiken om verdere beslissingen te nemen in de aanroepende functie of deze als argument doorgeven aan een andere functie.

Hier is een eenvoudig voorbeeld van het gebruik van een retourwaarde om de logicastroom te controleren:

```scheme
;; Doel: Stuurt een bericht naar de juiste uitvoerbestemming
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

In dit geval vertrouwt (send-message) op de geretourneerde waarde van (is-valid-output-display?) om te beslissen of er doorgegaan moet worden.
De voorwaardelijke instructie `cond` wordt overgeslagen als de eerste test mislukt. Merk ook op hoe het op een redelijk natuurlijke manier leest: is het een geldige uitvoerweergave?

## If-instructielogica in schema

Vóór het gerefactoreerde bibliotheekvoorbeeld volgt hier een kort overzicht van de voorwaardelijke logica. Schema gebruikt `if` om tussen twee paden te kiezen.

Hier is een eenvoudige vorm van een `if`-verklaring:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Deze structuur controleert de voorwaarde en als de voorwaarde waar is, wordt de eerste actie uitgevoerd. Als de voorwaarde onwaar is, wordt de tweede actie uitgevoerd.

In gevallen waarin u meerdere acties moet uitvoeren terwijl de voorwaarde waar of onwaar is, kunt u `begin` gebruiken om ze te groeperen:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Hierdoor kunt u complexere situaties aan, waarbij meerdere expressies of instructies moeten worden uitgevoerd, afhankelijk van de uitkomst van de voorwaardelijke test.

Oké, hier is de bibliotheekcode met ingebedde retourwaarden die worden gebruikt om het uitvoeringsproces te besturen.

### Geherstructureerd met retourwaarden

```scheme
;; Doel: Stuurt een bericht naar de statusbalk, retourneert #t bij succes
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar het dialoogvenster, retourneert #t bij succes
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de Error Console, retourneert #t bij succes
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de terminal, retourneert #t bij succes
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de juiste uitvoer, retourneert #t bij succes
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Doel: Controleert dat het bericht een niet-lege tekenreeks is, retourneert #t indien geldig
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Doel: Controleert dat de uitvoer een geldige bestemming is, retourneert #t indien geldig
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusie

Retourwaarden vormen een fundamenteel onderdeel van het flexibel en herbruikbaar maken van functies. Door zorgvuldig te beslissen wat elke functie moet retourneren, kunnen we ervoor zorgen dat onze functies goed met elkaar samenwerken en nuttige informatie aan de rest van de code verstrekken. Of het nu gaat om het retourneren van `#t` of `#f`, of iets specifiekers, retourwaarden bieden ons een manier om de stroom van onze programma's te controleren en verschillende uitkomsten te verwerken.