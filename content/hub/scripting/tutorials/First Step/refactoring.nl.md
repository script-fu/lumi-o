---
title: "Refactoring"
type: docs
weight: 2
---
Zodra een functie werkt, kunnen we een stapje terug doen en nadenken over hoe we onze code het beste kunnen structureren. Het doel is om onze plug-in zo duidelijk, begrijpelijk en onderhoudbaar mogelijk te maken. Dit proces van het verbeteren en verfijnen van de structuur van bestaande code zonder het gedrag ervan te veranderen, staat bekend als refactoring.

Hier is nogmaals de initiële functie:

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

De functienaam is de naam van de functie en parameter is wat de functie als invoer accepteert. De body is het codeblok dat wordt uitgevoerd wanneer de functie wordt aangeroepen.

Abstracte vorm:

```scheme
(define (function-name parameter)
  body)
```

### Codeherhaling

Verwijder herhaling vroegtijdig. `(lumi-message "Hello world!\n")` wordt twee keer herhaald en de berichtenreeks wordt drie keer herhaald. Een variabele lost de herhaalde string op.

### Variabelen

In Scheme heeft een variabele een "scope", waar deze bekend over is, en die scope wordt ingesteld met behulp van een `let` statement. De variabele is gebonden aan een waarde in het bindende deel, en de variabele heeft bereik in de let-tekst. De variabele is alleen bekend binnen het let-blok en is daarbuiten niet toegankelijk.

```scheme
(let ((variable value))
  body)
```

Introductie van een variabele genaamd "bericht":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

In ons voorbeeld hebben we een variabele gebruikt genaamd "message" gebonden aan een string "Hallo wereld!\n". Hierdoor kunnen we de berichtinhoud één keer wijzigen in plaats van drie keer, waardoor de kans op fouten kleiner wordt en de code flexibeler wordt.

### Functies extraheren

Bij functioneel programmeren is het herstructureren van code om herbruikbare logica in afzonderlijke functies te extraheren een gangbare praktijk. Door dit te doen wordt de **hoofdfunctie** veel eenvoudiger en meer gericht op het hogere doel, terwijl de **geëxtraheerde functie** complexer lijkt omdat deze de gedetailleerde logica afhandelt. Dit is opzettelijk en sluit aan bij de kernprincipes van functioneel programmeren, zoals modulariteit, scheiding van zorgen en leesbaarheid. Hier is de gerefactoreerde
Hallo wereld! na extractie.

De logica extraheren:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Symbolen
In het bovenstaande voorbeeld wordt een gegevenstype gebruikt dat een symbool wordt genoemd, zoals 'gui. Symbolen worden als parameters doorgegeven aan de functie voor het verzenden van berichten en kunnen worden gebruikt om eenvoudige voorwaardelijke beslissingen te nemen. Net als symbolische sleutels zijn het unieke identificatiegegevens. Ga voor meer informatie over symbolen naar [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Vereenvoudiging van de hoofdfunctie

In de originele functie (scheme-hello-world) was alle logica voor het verzenden van berichten naar verschillende uitgangen (GUI, Error Console, Terminal) gemengd in de hoofdfunctie. Na refactoring concentreert de hoofdfunctie zich eenvoudigweg op **wat er moet gebeuren**, waarbij het bericht naar verschillende bestemmingen wordt verzonden.

De gerefactoreerde hoofdfunctie is eenvoudiger:

- Het vermeldt duidelijk het doel: hetzelfde bericht naar meerdere uitgangen sturen.
- Het vermijdt dat de hoofdlogica onoverzichtelijk wordt met repetitieve code, zoals het instellen van berichtbehandelaars voor verschillende uitgangen.
- Het is gemakkelijker om in één oogopslag te lezen en te begrijpen.

### De complexiteit van de geëxtraheerde functie

In de functie **(verzendbericht)** bevindt zich daarentegen de gedetailleerde logica. Het verwerkt nu de variaties in gedrag voor elke uitvoer (GUI, Error Console, Terminal). De functie is iets complexer dan voorheen, maar is nu **gecentraliseerd** en **geïsoleerd**.

## Dit in verband brengen met functioneel programmeren

Bij functioneel programmeren worden functies gezien als **eersteklasburgers**, wat betekent dat ze kunnen worden hergebruikt, doorgegeven en gecombineerd om complexer gedrag te vormen. Het doel is om:- **Ontleed problemen** in kleinere, onafhankelijke stukken.
- **Isoleer complexiteit** in kleinere functies die specifieke taken afhandelen, zoals `send-message`.
- **Houd functies op een hoger niveau eenvoudig**, zodat ze zich kunnen concentreren op het orkestreren van de gegevensstroom en acties, zonder dat ze de details hoeven te weten over hoe elke taak wordt uitgevoerd.
- **Scheiding van zorgen**: de functie zorgt ervoor hoe het bericht wordt verzonden op basis van het uitvoertype, waardoor deze logica wordt geïsoleerd van de hoofdfunctie.
- **Modulariteit**: door alle logica voor het verzenden van berichten op één plek af te handelen, kunnen we eenvoudig wijzigingen aanbrengen (zoals het toevoegen van nieuwe uitvoeropties) zonder de hoofdfunctie te wijzigen.
- **Herbruikbaarheid**: de functie `send-message` is herbruikbaar, wat betekent dat als we een bericht naar meerdere uitgangen elders in onze code moeten sturen, we deze functie eenvoudigweg kunnen aanroepen in plaats van soortgelijke logica te herschrijven.

Door refactoring wordt de hoofdfunctie in dit voorbeeld een **declaratieve** verklaring van wat er gebeurt ("stuur een bericht naar drie plaatsen"), terwijl de complexiteit van hoe deze berichten moeten worden verzonden, wordt weggenomen in de functie `send-message`.