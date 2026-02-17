---
title: "Slutliga tankar"
type: docs
weight: 10
---
Du har nu ett plug-in för fungerande procedur och ett litet hjälpbibliotek. Den här serien introducerade kärnmönstren du kommer att använda i de flesta Lumi-skript:

- Funktioner: Byggstenarna i våra plugin-program.
- Refactoring: Förbättrar kodstrukturen samtidigt som funktionaliteten bibehålls.
- Kodbibliotek: Centralisera återanvändbara funktioner för att hålla vår kod ren och modulär.
- Valideringstekniker: Se till att indata är giltiga innan vi kör vår kärnlogik.

Du såg också grunderna i att använda Git för att spåra ändringar och hålla en ren projektstruktur. Det arbetsflödet gör det lättare att iterera utan att förlora fungerande versioner.

Här är den slutliga versionen av vår huvudsakliga plugin-kod:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Bibliotekskod:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Slutsats

Genom att omstrukturera meddelandehjälparna till ett litet bibliotek förblir plugin-programmet fokuserat på avsikt och biblioteket innehåller implementeringsdetaljerna. Validering och konsekvent meddelandedirigering gör att misslyckanden är förutsägbara.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Nästa steg:

- Flytta återanvändbara hjälpare till en dedikerad biblioteksfil.
- Håll plugin-program små och nämn procedurer för vad de gör.
- Lägg till validering vid gränser (ingångar, filsökvägar, menyalternativ).

Behåll slutresultatet som två filer i din plugin-repo:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`