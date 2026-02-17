---
title: "Considerazioni finali"
type: docs
weight: 10
---
Ora hai un plug-in per la procedura funzionante e una piccola libreria di supporto. Questa serie ha introdotto i modelli principali che utilizzerai nella maggior parte degli script Lumi:

- Funzioni: gli elementi costitutivi dei nostri plug-in.
- Refactoring: miglioramento della struttura del codice mantenendo la funzionalità.
- Librerie di codici: centralizzazione delle funzioni riutilizzabili per mantenere il nostro codice pulito e modulare.
- Tecniche di convalida: garantire che gli input siano validi prima di eseguire la nostra logica di base.

Hai anche visto le nozioni di base sull'utilizzo di Git per tenere traccia delle modifiche e mantenere una struttura di progetto pulita. Questo flusso di lavoro semplifica l'iterazione senza perdere le versioni funzionanti.

Ecco la versione finale del nostro codice plug-in principale:

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

Codice biblioteca:

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

## Conclusione

Effettuando il refactoring degli helper di messaggistica in una piccola libreria, il plug-in rimane focalizzato sull'intento e la libreria contiene i dettagli di implementazione. La convalida e il routing coerente dei messaggi mantengono prevedibili gli errori.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Passaggi successivi:

- Sposta gli helper riutilizzabili in un file di libreria dedicato.
- Mantenere i plug-in piccoli e nominare le procedure per ciò che fanno.
- Aggiungi convalida ai confini (input, percorsi di file, opzioni di menu).

Mantieni il risultato finale come due file nel repository dei plug-in:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`