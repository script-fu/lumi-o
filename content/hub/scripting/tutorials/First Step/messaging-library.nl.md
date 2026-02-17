---
title: "Berichtenbibliotheek"
type: docs
weight: 6
---
Wat begon als een enkele functie om berichten te verzenden, is in de loop van de tijd uitgegroeid tot een verzameling gerelateerde functies. Deze functies vormen nu de basis van een **Berichtenbibliotheek**, ontworpen om uitvoer naar verschillende bestemmingen te verwerken, zoals de GUI, Error Console en Terminal.

### Waarom een ​​berichtenbibliotheek?

Naarmate onze behoeften groeien, vereist het verwerken van berichten via meerdere uitgangen een meer modulaire en uitbreidbare aanpak. In plaats van dat één enkele functie alles doet, hebben we het proces opgedeeld in herbruikbare componenten, waardoor er meer flexibiliteit ontstaat. Deze bibliotheek kan nu worden gebruikt als een berichtentool voor algemene doeleinden waarvan andere plug-ins of functies kunnen lenen.

### Wat doet de berichtenbibliotheek?

De Berichtenbibliotheek bevat momenteel de volgende functies:

- **send-to-gui**: verzendt berichten naar het Lumi GUI-dialoogvenster.
- **send-to-error-console**: verzendt berichten naar de Lumi Error Console.
- **verzenden naar terminal**: verzendt berichten naar het terminalvenster.
- **send-message**: een verzendfunctie die berichten naar de juiste uitgang stuurt.
- **validate-message**: Zorgt ervoor dat het bericht en de uitvoer geldig zijn voordat het wordt verzonden.

### De bibliotheek uitbreiden

De **Berichtenbibliotheek** kan eenvoudig worden uitgebreid om extra uitgangen te ondersteunen. Bijvoorbeeld:

- **verzenden naar bestand**: berichten opslaan in een logbestand.
- **send-to-logger**: Integreer met een extern logsysteem.
- **verzenden naar notificatie**: berichten weergeven als systeemmeldingen.

Door hetzelfde patroon van modulair ontwerp en herbruikbare functies te volgen, kan deze bibliotheek uitgroeien tot een alomvattend hulpmiddel voor het verwerken van allerlei soorten berichttaken.

## Voordelen van een berichtenbibliotheek

- **Herbruikbaarheid**: de functies kunnen worden hergebruikt in verschillende plug-ins of projecten.
- **Modulariteit**: elke functie voert één specifieke taak uit, waardoor de code gemakkelijker te onderhouden en uit te breiden is.
- **Consistentie**: het gebruik van dezelfde validatie- en berichtverwerkingsfuncties zorgt voor consistent gedrag in de hele applicatie.

De **Berichtenbibliotheek** is het begin van een breder raamwerk dat de manier waarop berichten in uw project worden beheerd, kan vereenvoudigen. Naarmate de bibliotheek groeit, kunnen nieuwe plug-ins er eenvoudig gebruik van maken om berichten te verzenden waar ze ook heen moeten.

We kunnen de bestandsstructuur aanpassen:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

En vergeet niet om `load` aan te passen in de hoofdplug-in:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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