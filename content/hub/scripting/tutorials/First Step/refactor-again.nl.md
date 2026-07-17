---
title: "Opnieuw refactoreren"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
url: "hub/scripting/tutorials/First Step/refactor-again"
---
Naarmate de helperbibliotheek groeit, wordt het moeilijker om in één oogopslag te volgen. Refactoreer opnieuw om elke functie klein en voor één doel te houden.

### Complexiteit doorbreken

Om de functie eenvoudiger te volgen en te onderhouden, kunt u deze opsplitsen in kleinere, gerichte functies. Begin met het scheiden van validatie en berichtroutering.

### Maak een validatiefunctie

We kunnen het deel van de functie dat de argumenten `message` en `output` valideert, naar een aparte functie verplaatsen. Op deze manier hoeft de kernfunctie `send-message` zich geen zorgen te maken over validatie, waardoor deze gemakkelijker te volgen is.

```scheme
(define (validate-message message output)
  ;; Controleren of het bericht een niet-lege tekenreeks is
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Controleren of de uitvoer een van de verwachte bestemmingen is
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Vereenvoudig het verzenden van berichten

Nu de validatie naar een aparte functie is verplaatst, kan de functie `send-message` zich richten op alleen het verzenden van het bericht. Het zal veel eenvoudiger zijn, omdat het alleen de specifieke taak afhandelt om het bericht naar de juiste bestemming te leiden.

```scheme
(define (send-message message output)
  ;; Roep de validatiefunctie aan voordat u doorgaat
  (validate-message message output)

  (cond
    ;; Verzenden naar de berichtenconsole
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Verzenden naar het GUI-dialoogvenster
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Verzenden naar het terminalvenster
    ((eq? output 'terminal)
       (display message)))

  ;; Herstel de standaard berichtenhandler naar de berichtenconsole
  (lumi-message-set-handler 2))
```

### Verder uitgesplitst: scheid elke uitvoerhandler

Elk type berichtuitvoer (GUI, berichtenconsole, terminal) kan naar een eigen functie worden verplaatst. Dit maakt het eenvoudiger om te testen, aan te passen en mogelijk uit te breiden in de toekomst.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Stuur naar de juiste uitvoer
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Herstel de standaard berichtenhandler naar de berichtenconsole
  (lumi-message-set-handler 2))
```

### Validatie hergebruiken in elke verzendfunctie

Omdat validatie een belangrijk onderdeel is om ervoor te zorgen dat zowel het bericht als de uitvoer correct zijn, is het logisch dat elke `send-*` functie zijn eigen validatie uitvoert. Dit zorgt ervoor dat, ongeacht welke uitgang wordt aangeroepen, we altijd eerst de ingangen controleren.

```scheme
(define (send-to-gui message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'terminal)
  (display message))
```

Zie dat we de validatie van de functie voor het verzenden van berichten hebben verwijderd en de verantwoordelijkheid hebben verschoven naar elke individuele uitvoerfunctie. Deze wijziging zorgt ervoor dat elke bestemming (GUI, berichtenconsole, terminal) zijn eigen validatie afhandelt, waardoor de functie voor het verzenden van berichten wordt gestroomlijnd en de validatielogica dichter bij de plaats blijft waar deze nodig is.

Deze aanpak kan de functie voor het verzenden van berichten vereenvoudigen, waardoor deze een _dispatcher_ wordt, terwijl ervoor wordt gezorgd dat elke functie voor verzenden naar* het bericht correct valideert voordat het wordt verwerkt.

Door de validatie naar elke send-to-*-functie te verplaatsen, hebben we ze herbruikbaar gemaakt als zelfstandige functies. Dit betekent dat we elk van de send-to-gui-, send-to-error-console- of send-to-terminal-functies rechtstreeks kunnen aanroepen zonder afhankelijk te zijn van de send-message dispatcher-functie. Elk van deze functies verwerkt nu volledig zijn eigen logica en kan onafhankelijk worden gebruikt in andere delen van de code of in andere plug-ins, waardoor uw code modulair en flexibeler wordt.

## Voordelen van refactoring

- **Duidelijke scheiding van zorgen**: elke functie heeft nu slechts één verantwoordelijkheid, waardoor de code gemakkelijker te begrijpen is.
- **Uitbreidbaarheid**: het toevoegen van nieuwe uitvoertypen is eenvoudig. U definieert eenvoudigweg een nieuwe functie zoals `send-to-file` of `send-to-logger`, en voegt vervolgens een case toe aan de `cond`-instructie.
- **Herbruikbaarheid**: elk van deze functies voor uitvoerverwerking kan elders in uw project worden hergebruikt of worden gedeeld met meerdere plug-ins.
- **Consistentie**: door de validatiefunctie in elke `send-to-*` functie opnieuw te gebruiken, zorgt u ervoor dat alle uitvoer correct wordt gevalideerd, waardoor de code robuuster wordt.

Een opnieuw opgebouwde bibliotheekversie:

```scheme
;; Doel: Stuurt een bericht naar het GUI-dialoogvenster
(define (send-to-gui message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Doel: Stuurt een bericht naar de berichtenconsole
(define (send-to-error-console message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Doel: Stuurt een bericht naar het terminalvenster
(define (send-to-terminal message)
  ;; Valideer het bericht voordat u doorgaat
  (validate-message message 'terminal)
  (display message))

;; Doel: Stuurt een bericht naar de juiste uitvoerbestemming
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Herstel de standaard berichtenhandler naar de berichtenconsole
  (lumi-message-set-handler 2))

;; Doel: Controleert dat het bericht een niet-lege tekenreeks is en dat de uitvoer geldig is
(define (validate-message message output)
  ;; Controleren of het bericht een niet-lege tekenreeks is
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Controleren of de uitvoer een van de verwachte bestemmingen is
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Is dat alles wat we kunnen doen? Nee! Er is nog meer te doen, lees verder.