---
title: "Omarbetning"
type: docs
weight: 7
---
Det här steget fixar ett subtilt beteende i meddelandeexemplet.

Vi skickade strängen "Hello world!\n" som meddelande. "\n" är en speciell typ av tecken, ett "flykt"-tecken. Den säger åt utskriften att starta en ny rad. I Schema kommer det också att tvinga ett meddelande som skickas till statusfältet att dyka upp som en GUI-ruta.

Hjälparen `send-to-gui` skickar meddelanden till en Lumi-dialogruta.

Uppdatera meddelandets innehåll och destinationer så att exemplet beter sig konsekvent.

Ta bort escape-tecknet och utöka funktionerna:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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

Ersätt magiska tal med konstanterna från Lumi (till exempel `MESSAGE-BOX` och `ERROR-CONSOLE`).

Dela sedan upp valideringen i två funktioner så att den kan återanvändas från flera samtalsplatser.

- (är-giltig-sträng?) Att kontrollera att en sträng är en sträng och inte en tom sträng, inom en skicka-till*-funktion.
- (is-valid-output-display?) För att kontrollera att en given utdatadestination är giltig, i skicka-meddelande-funktionen.

Omarbeta biblioteket:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
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

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Slutsats

Genom att omarbeta vårt meddelandebibliotek har vi gjort det mer robust och pålitligt. Vi fixade det dolda problemet med nyradstecken, introducerade konstanter för bättre tydlighet och utökade funktionaliteten genom att lägga till stöd för statusfältet och dialogrutans utdata. Att dessutom separera valideringslogiken i mindre, fokuserade funktioner säkerställer att vår kod är lättare att underhålla och utöka i framtiden.

Denna omarbetning visar hur små förändringar kan förbättra den övergripande strukturen och funktionaliteten i vårt bibliotek, vilket banar väg för mer flexibilitet och återanvändbarhet när vårt projekt växer.