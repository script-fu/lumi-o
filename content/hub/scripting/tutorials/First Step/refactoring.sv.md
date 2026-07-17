---
title: "Refaktorering"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bc83f55511f34e6f099f8de8c6af3bba5e459974aa4bff6265ae70d679517646
---
När vi har en funktion som fungerar kan vi ta ett steg tillbaka och fundera på hur vi bäst strukturerar vår kod. Målet är att göra vår plug-in så tydlig, begriplig och underhållbar som möjligt. Denna process att förbättra och förfina strukturen för befintlig kod utan att ändra dess beteende kallas för refactoring.

Här är den första funktionen igen:

```scheme
(define (scheme-hello-world)
  ;; Ställ in meddelandehanteraren så att meddelandet skickas till en GUI-dialogruta
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Ställ in meddelandehanteraren så att meddelandet skickas till Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Skicka meddelandet till terminal, OS-fönstret som startade Lumi
  (display "Hello world!\n"))
```

Funktionsnamnet är namnet på funktionen, och parametern är vad funktionen accepterar som indata. Body är det kodblock som körs när funktionen anropas.

Abstrakt form:

```scheme
(define (function-name parameter)
  body)
```

### Kodupprepning

Ta bort repetition tidigt. `(lumi-message "Hello world!\n")` upprepas två gånger, och meddelandesträngen upprepas tre gånger. En variabel löser den upprepade strängen.

### VariablerI Schema har en variabel ett "omfattning", där det är känt om det, och det omfånget ställs in med hjälp av en `let`-sats. Variabeln är bunden till ett värde i bindningsdelen, och variabeln har scope i let-kroppen. Variabeln är bara känd inuti let-blocket och kan inte nås utanför det.

```scheme
(let ((variable value))
  body)
```

Vi introducerar en variabel som heter "meddelande":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Ställ in meddelandehanteraren så att meddelandet skickas till en GUI-dialogruta
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Ställ in meddelandehanteraren så att meddelandet skickas till Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Skicka meddelandet till terminal, OS-fönstret som startade Lumi
    (display message)))
```

I vårt exempel har vi använt en variabel som heter "meddelande" bunden till en sträng "Hello world!\n". Detta gör att vi kan ändra meddelandeinnehållet en gång istället för tre gånger, vilket minskar risken för fel och gör koden mer flexibel.

### Extrahera funktionerInom funktionell programmering är det vanligt att omfaktorisera kod för att extrahera återanvändbar logik till separata funktioner. Genom att göra detta blir **huvudfunktionen** mycket enklare och mer fokuserad på sitt högnivåmål, medan den **extraherade funktionen** verkar mer komplex eftersom den hanterar den detaljerade logiken. Detta är avsiktligt och överensstämmer med kärnprinciperna för funktionell programmering, som modularitet, separation av problem och läsbarhet. Här är den refaktorerade
Hej världen! efter extraktion.

Extrahera logiken:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; Huvudfunktion
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Funktion för att hantera meddelandeutdata till olika destinationer
(define (send-message message output)
  (cond
    ;; Skicka till Error Console
    ((eq? output 'error-console)
       ;; Ställ in hanteraren till Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Skicka till GUI-dialogrutan
    ((eq? output 'gui)
       ;; Ställ in hanteraren till GUI-dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Skicka till terminalfönstret
    ((eq? output 'terminal)
       ;; Terminal-utdata hanteras med display
       (display message)))

  ;; Återställ standardmeddelandehanteraren till Error Console
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

#### SymbolerI exemplet ovan används en datatyp som kallas en symbol, till exempel 'gui. Symboler skickas som parametrar till skicka-meddelande-funktionen och kan användas för att fatta enkla villkorade beslut. Liksom symboliska nycklar är de unika identifierare. För mer information om symboler, besök [den här sidan.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Förenkla huvudfunktionen

I den ursprungliga (schema-hello-world)-funktionen blandades all logik för att skicka meddelanden till olika utgångar (GUI, Error Console, Terminal) in i huvudfunktionen. Efter omfaktorering fokuserar huvudfunktionen helt enkelt på **vad som behöver göras**, och skickar meddelandet till olika destinationer.

Den refaktorerade huvudfunktionen är enklare:

- Den anger tydligt dess syfte: skicka samma meddelande till flera utgångar.- Det undviker att belamra huvudlogiken med upprepad kod som att ställa in meddelandehanterare för olika utgångar.
– Det är lättare att läsa och förstå på ett ögonblick.

### Den extraherade funktionens komplexitet

Däremot är **(sänd-meddelande)-funktionen** där den detaljerade logiken finns. Den hanterar nu variationerna i beteende för varje utgång (GUI, Error Console, Terminal). Funktionen är lite mer komplex än tidigare, men den är nu **centraliserad** och **isolerad**.

## Relaterar detta till funktionell programmering

I funktionell programmering ses funktioner som **förstklassiga medborgare**, vilket innebär att de kan återanvändas, skickas runt och kombineras för att bilda ett mer komplext beteende. Målet är att:

- **Dekomponera problem** i mindre, oberoende bitar.- **Isolera komplexitet** till mindre funktioner som hanterar specifika uppgifter, som `send-message`.
- **Håll överordnade funktioner enkla** så att de kan fokusera på att orkestrera flödet av data och åtgärder, utan att behöva känna till detaljerna om hur varje uppgift utförs.
- **Separation of concerns**: Funktionen tar hand om hur meddelandet skickas baserat på utdatatypen, vilket isolerar denna logik från huvudfunktionen.
- **Modularitet**: Genom att hantera all meddelandesändningslogik på ett ställe kan vi enkelt göra ändringar (som att lägga till nya utdataalternativ) utan att ändra huvudfunktionen.- **Återanvändbarhet**: `send-message`-funktionen är återanvändbar, vilket innebär att om vi behöver skicka ett meddelande till flera utgångar någon annanstans i vår kod, kan vi helt enkelt anropa denna funktion istället för att skriva om liknande logik.

Genom omfaktorisering blir huvudfunktionen i detta exempel ett **deklarativt** uttalande om vad som händer ("skicka ett meddelande till tre ställen"), medan komplexiteten i hur man skickar dessa meddelanden abstraheras bort i funktionen `send-message`.