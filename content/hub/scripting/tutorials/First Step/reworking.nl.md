---
title: "Herwerken"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 324662811965905bad18a135ac343a3eb8120da180149b19bc212a6af61a4bb7
---
Met deze stap wordt een subtiel gedrag in het berichtenvoorbeeld opgelost.

We gaven de tekenreeks "Hallo wereld!\n" door als bericht. De "\n" is een speciaal soort teken, een "escape"-teken. Het vertelt de uitvoerafdruk om een ​​nieuwe regel te beginnen. In Scheme zal het ook een bericht dat naar de statusbalk wordt verzonden, dwingen om als een GUI-box te verschijnen.

De helper `send-to-gui` verzendt berichten naar een Lumi-dialoogvenster.

Werk de inhoud en bestemmingen van het bericht bij, zodat het voorbeeld zich consistent gedraagt.

Het escape-teken verwijderen en de functies uitbreiden:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Vervang magische getallen door de constanten van Lumi (bijvoorbeeld `MESSAGE-BOX` en `ERROR-CONSOLE`).

Splits de validatie vervolgens in twee functies, zodat deze vanaf meerdere oproeplocaties kan worden hergebruikt.

- (is-geldige-string?) Om te controleren of een string een string is en geen lege string, binnen een send-to*-functie.
- (is-geldige-uitvoerweergave?) Om te controleren of een bepaalde uitvoerbestemming geldig is, in de verzendberichtfunctie.

Herwerk de bibliotheek:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Voeg een regeleinde toe om een berichtvak te forceren
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Doel: Stuurt een bericht naar de juiste uitvoerbestemming
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Doel: Controleert dat het bericht een niet-lege tekenreeks is
(define (is-valid-string? message)
  ;; Controleren of het bericht een niet-lege tekenreeks is
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Doel: Controleert dat het bericht naar een geldige uitvoer wordt gestuurd
(define (is-valid-output-display? output)
  ;; Controleren of de uitvoer een van de verwachte weergavebestemmingen is
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusie

Door onze berichtenbibliotheek te herwerken, hebben we deze robuuster en betrouwbaarder gemaakt. We hebben het verborgen probleem met het nieuweregelteken opgelost, constanten geïntroduceerd voor betere duidelijkheid en de functionaliteit uitgebreid door ondersteuning toe te voegen voor de statusbalk en de uitvoer van dialoogvensters. Bovendien zorgt het scheiden van de validatielogica in kleinere, gerichte functies ervoor dat onze code in de toekomst gemakkelijker te onderhouden en uit te breiden is.

Deze herwerking laat zien hoe kleine veranderingen de algehele structuur en functionaliteit van onze bibliotheek kunnen verbeteren, waardoor de weg wordt vrijgemaakt voor meer flexibiliteit en herbruikbaarheid naarmate ons project groeit.