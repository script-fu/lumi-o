---
title: "Godkännande"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
url: "hub/scripting/tutorials/First Step/validation"
---
När du bygger robusta plugin-program är det viktigt att se till att våra funktioner hanterar fel elegant och fungerar som förväntat, även i fall av missbruk eller oväntade indata. Validering hjälper till att skydda funktionens integritet och förhindra krascher eller oavsiktligt beteende.

Låt oss titta på hur vi kan förbättra `send-message`-funktionen genom att lägga till valideringskontroller för att säkerställa att den hanterar indata korrekt.

### Validera ingångar

Innan vi skickar ett meddelande bör vi se till att `output`-argumentet som skickas till `send-message`-funktionen är giltigt. Vi kan lägga till en kontroll för att bekräfta att utdatadestinationen är ett av de förväntade värdena (gui, felkonsol eller terminal).

Exempel:

```scheme
(define (send-message message output)
  ;; Validerar utdataargumentet
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Skicka till meddelandekonsol
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Skicka till GUI-dialogrutan
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Skicka till terminalfönstret
      ((eq? output 'terminal)
         (display message))))

  ;; Återställ standardmeddelandehanteraren till meddelandekonsol
  (lumi-message-set-handler 2))
```

I det här exemplet använder vi `member` för att kontrollera om argumentet `output` är giltigt. Om inte, väcker funktionen ett fel med ett tydligt meddelande, vilket förhindrar att ogiltiga värden orsakar problem.

### Hantera tomma meddelanden

Det är också användbart att se till att argumentet `message` är giltigt. Till exempel, om en tom sträng eller #f (falskt) skickas som meddelande, bör funktionen hantera detta på ett elegant sätt.

Exempel på hantering av ett tomt meddelande:

```scheme
(define (send-message message output)
  ;; Kontrollera om meddelandet är tomt
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Detta tillvägagångssätt säkerställer att funktionen alltid får giltig input, vilket förbättrar dess tillförlitlighet och förhindrar oväntat beteende.

### Exempel på kombinerad validering

```scheme
;; Funktion för att hantera meddelandeutdata till olika destinationer
(define (send-message message output)

  ;; Validera meddelande- och utdataargument
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Skicka till meddelandekonsol
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Skicka till GUI-dialogrutan
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Skicka till terminalfönstret
        ((eq? output 'terminal)
           (display message)))))

  ;; Återställ standardmeddelandehanteraren till meddelandekonsol
  (lumi-message-set-handler 2))
```

I denna version:
- Funktionen kontrollerar först om `message` är tomt eller ogiltigt. Om meddelandet är giltigt går det vidare till att kontrollera om `output` är ett av de accepterade värdena (`gui`, `error-console` eller `terminal`).
- Om båda kontrollerna godkänns skickas meddelandet till lämplig utgång. Annars visas ett felmeddelande med en tydlig förklaring.
- En ytterligare kontroll görs för att säkerställa att meddelandet också är en sträng.

Denna kombinerade valideringsfunktion håller koden renare och säkerställer att båda ingångarna valideras innan någon åtgärd vidtas, vilket gör funktionen mer robust. Observera att vi också bygger in ett felsökningssystem för meddelanden. När
koden misslyckas, vi får en anledning, en anledning till att vi skrev själva.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```