---
title: "Slutliga tankar"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5233667e27065df0a6bc940209f767b9f9e32876d41fa3d09428737b535906e9
---
Du har nu ett plug-in för fungerande procedur och ett litet hjälpbibliotek. Den här serien introducerade kärnmönstren du kommer att använda i de flesta Lumi-skript:

- Funktioner: Byggstenarna i våra plugin-program.
- Refactoring: Förbättrar kodstrukturen samtidigt som funktionaliteten bibehålls.
- Kodbibliotek: Centralisera återanvändbara funktioner för att hålla vår kod ren och modulär.
- Valideringstekniker: Se till att indata är giltiga innan vi kör vår kärnlogik.

Du såg också grunderna i att använda Git för att spåra ändringar och hålla en ren projektstruktur. Det arbetsflödet gör det lättare att iterera utan att förlora fungerande versioner.

Här är den slutliga versionen av vår huvudsakliga plugin-kod:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
;; Syfte: Skickar ett meddelande till statusfältet, returnerar #t vid lyckat resultat
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till dialogrutan, returnerar #t vid lyckat resultat
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till Error Console, returnerar #t vid lyckat resultat
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till terminal, returnerar #t vid lyckat resultat
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till rätt utdata, returnerar #t vid lyckat resultat
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Syfte: Validerar att meddelandet är en icke-tom sträng, returnerar #t om giltigt
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Syfte: Validerar att utdata är en giltig destination, returnerar #t om giltig
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