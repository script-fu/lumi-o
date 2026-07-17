---
title: "Berichtenbibliotheek"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bfe459b9f717201d646bde29196fd66e6c3b19b3e9dbdb3338e0c853153e1c05
---
Wat begon als een enkele functie om berichten te verzenden, is in de loop van de tijd uitgegroeid tot een verzameling gerelateerde functies. These functions now form the foundation of a **Messaging Library**, designed to handle output to different destinations, such as the GUI, Message console, and OS terminal.

### Waarom een ​​berichtenbibliotheek?

Naarmate onze behoeften groeien, vereist het verwerken van berichten via meerdere uitgangen een meer modulaire en uitbreidbare aanpak. In plaats van dat één enkele functie alles doet, hebben we het proces opgedeeld in herbruikbare componenten, waardoor er meer flexibiliteit ontstaat. This library can now be used as a general-purpose messaging tool that other plug-ins or functions can borrow from.

### Wat doet de berichtenbibliotheek?

De Berichtenbibliotheek bevat momenteel de volgende functies:

- **send-to-gui**: verzendt berichten naar het Lumi GUI-dialoogvenster.
- **send-to-error-console**: Sends messages to the Lumi Message console.
- **verzenden naar terminal**: verzendt berichten naar het terminalvenster.
- **send-message**: een verzendfunctie die berichten naar de juiste uitgang stuurt.
- **validate-message**: Zorgt ervoor dat het bericht en de uitvoer geldig zijn voordat het wordt verzonden.

### De bibliotheek uitbreiden

De **Berichtenbibliotheek** kan eenvoudig worden uitgebreid om extra uitgangen te ondersteunen. Bijvoorbeeld:

- **verzenden naar bestand**: berichten opslaan in een logbestand.
- **send-to-logger**: Integreer met een extern logsysteem.
- **verzenden naar notificatie**: berichten weergeven als systeemmeldingen.

By following the same pattern of modular design and reusable functions, this library can grow into a comprehensive tool for handling all kinds of messaging tasks.

## Voordelen van een berichtenbibliotheek

- **Herbruikbaarheid**: de functies kunnen worden hergebruikt in verschillende plug-ins of projecten.
- **Modulariteit**: elke functie voert één specifieke taak uit, waardoor de code gemakkelijker te onderhouden en uit te breiden is.
- **Consistency**: Using the same validation and message-handling functions ensures consistent behavior across the application.

The **Messaging Library** is the beginning of a broader framework that could simplify how messages are managed in your project. Naarmate de bibliotheek groeit, kunnen nieuwe plug-ins er eenvoudig gebruik van maken om berichten te verzenden waar ze ook heen moeten.

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
#!/usr/bin/env lumi-scheme-interpreter-0.1

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