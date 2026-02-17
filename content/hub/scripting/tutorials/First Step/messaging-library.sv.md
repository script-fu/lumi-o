---
title: "Meddelandebibliotek"
type: docs
weight: 6
---
Med tiden har det som började som en enda funktion för att skicka meddelanden utvecklats till en samling relaterade funktioner. Dessa funktioner utgör nu grunden för ett **Meddelandebibliotek**, designat för att hantera utdata till olika destinationer, såsom GUI, Error Console och Terminal.

### Varför ett meddelandebibliotek?

När våra behov växer kräver hantering av meddelanden över flera utgångar ett mer modulärt och utbyggbart tillvägagångssätt. Istället för att en enda funktion gör allt, har vi delat upp processen i återanvändbara komponenter, vilket möjliggör större flexibilitet. Detta bibliotek kan nu användas som ett allmänt meddelandeverktyg som andra plugin-program eller funktioner kan låna från.

### Vad gör meddelandebiblioteket?

Meddelandebiblioteket innehåller för närvarande följande funktioner:

- **send-to-gui**: Skickar meddelanden till Lumi GUI-dialogrutan.
- **send-to-error-console**: Skickar meddelanden till Lumi Error Console.
- **sänd-till-terminal**: Skickar meddelanden till terminalfönstret.
- **sänd-meddelande**: En avsändarfunktion som dirigerar meddelanden till lämplig utgång.
- **validera-meddelande**: Säkerställer att meddelandet och utdata är giltiga innan det skickas.

### Utökar biblioteket

**Meddelandebiblioteket** kan enkelt utökas för att stödja ytterligare utgångar. Till exempel:

- **skicka-till-fil**: Spara meddelanden till en loggfil.
- **sänd-till-logger**: Integrera med ett externt loggningssystem.
- **skicka till avisering**: Visa meddelanden som systemaviseringar.

Genom att följa samma mönster av modulär design och återanvändbara funktioner kan detta bibliotek växa till ett omfattande verktyg för att hantera alla typer av meddelandeuppgifter.

## Fördelar med ett meddelandebibliotek

- **Återanvändbarhet**: Funktionerna kan återanvändas över olika plugin-program eller projekt.
- **Modularitet**: Varje funktion hanterar en specifik uppgift, vilket gör koden lättare att underhålla och utöka.
- **Konsistens**: Att använda samma validerings- och meddelandehanteringsfunktioner säkerställer konsekvent beteende i hela applikationen.

**Meddelandebiblioteket** är början på ett bredare ramverk som kan förenkla hur meddelanden hanteras i ditt projekt. När biblioteket växer kan nya plugin-program enkelt utnyttja det för att skicka meddelanden vart de än behöver gå.

Vi kan justera filstrukturen:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Och kom ihåg att justera `load` i huvudplugin:

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