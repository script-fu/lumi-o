---
title: "Refaktor igen"
type: docs
weight: 5
---
När hjälpbiblioteket växer blir det svårare att följa med en blick. Refaktorera igen för att hålla varje funktion liten och enkel.

### Bryter ner komplexitet

För att göra funktionen lättare att följa och underhålla, dela upp den i mindre, fokuserade funktioner. Börja med att skilja validering från meddelandedirigering.

### Skapa en valideringsfunktion

Vi kan ta den del av funktionen som validerar argumenten `message` och `output` och flytta den till en separat funktion. På så sätt behöver kärnfunktionen `send-message` inte oroa sig för validering, vilket gör den lättare att följa.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Förenkla meddelandesändningen

Nu när valideringen har flyttats till en separat funktion kan `send-message`-funktionen fokusera på att bara skicka meddelandet. Det blir mycket enklare, eftersom det bara hanterar den specifika uppgiften att dirigera meddelandet till rätt destination.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Nedbrytning ytterligare: Separera varje utdatahanterare

Varje typ av meddelandeutgång (GUI, Error Console, Terminal) kan flyttas till sin egen funktion. Detta möjliggör enklare testning, modifiering och potentiell förlängning i framtiden.

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
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Återanvända validering i varje sändningsfunktion

Eftersom validering är en viktig del för att säkerställa att både meddelandet och utdata är korrekta, är det vettigt att varje `send-*`-funktion utför sin egen validering. Detta säkerställer att oavsett vilken utgång som anropas så kontrollerar vi alltid ingångarna först.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Se att vi har tagit bort valideringen från skicka-meddelande-funktionen och flyttat ansvaret till varje enskild utdatafunktion. Denna ändring säkerställer att varje destination (GUI, Error Console, Terminal) hanterar sin egen validering, effektiviserar funktionen för att skicka meddelanden och håller valideringslogiken närmare där den behövs.

Detta tillvägagångssätt kan förenkla sänd-meddelande-funktionen, vilket gör den till en _dispatcher_, samtidigt som man säkerställer att varje sänd-till-*-funktion validerar meddelandet korrekt innan bearbetning.

Genom att flytta valideringen till varje skicka-till-*-funktion har vi gjort dem återanvändbara som fristående funktioner. Detta betyder att vi kan anropa vilken som helst av skicka-till-gui-, skicka-till-fel-konsolen eller skicka-till-terminal-funktionerna direkt utan att förlita oss på skicka-meddelande-dispatcher-funktionen. Var och en av dessa funktioner hanterar nu helt sin egen logik och kan användas oberoende i andra delar av koden eller i andra plug-ins, vilket gör din kod mer modulär och flexibel.

## Fördelar med Refactoring

- **Klar åtskillnad av bekymmer**: Varje funktion hanterar nu bara ett ansvar, vilket gör koden lättare att förstå.
- **Utökbarhet**: Det är enkelt att lägga till nya utdatatyper. Du definierar helt enkelt en ny funktion som `send-to-file` eller `send-to-logger` och lägger sedan till ett fall i `cond`-satsen.
- **Återanvändbarhet**: Var och en av dessa utdatahanteringsfunktioner kan återanvändas någon annanstans i ditt projekt eller delas mellan flera plugin-program.
- **Konsistens**: Genom att återanvända valideringsfunktionen i varje `send-to-*`-funktion säkerställer du att alla utgångar är korrekt validerade, vilket gör koden mer robust.

En refaktorerad biblioteksversion:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Är det allt vi kan göra? Inga! det finns mer att göra, läs vidare.