---
title: "Rielaborazione"
type: docs
weight: 7
---
Questo passaggio risolve un comportamento sottile nell'esempio di messaggistica.

Stavamo passando la stringa "Hello world!\n" come messaggio. Il "\n" è un tipo speciale di carattere, un carattere di "fuga". Dice alla stampa dell'output di iniziare una nuova riga. In Scheme, forzerà anche la visualizzazione di un messaggio inviato alla barra di stato come finestra della GUI.

L'helper `send-to-gui` invia messaggi a una finestra di dialogo Lumi.

Aggiorna il contenuto e le destinazioni del messaggio in modo che l'esempio si comporti in modo coerente.

Rimozione del carattere di escape ed estensione delle funzioni:
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

Sostituisci i numeri magici con le costanti fornite da Lumi (ad esempio, `MESSAGE-BOX` e `ERROR-CONSOLE`).

Quindi suddividi la convalida in due funzioni in modo che possa essere riutilizzata da più siti di chiamata.

- (is-valid-string?) Per verificare che una stringa sia una stringa e non una stringa vuota, all'interno di una funzione send-to*.
- (is-valid-output-display?) Per verificare che una determinata destinazione di output sia valida, nella funzione send-message.

Rielaborare la libreria:

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

## Conclusione

Rielaborando la nostra libreria di messaggistica, l'abbiamo resa più solida e affidabile. Abbiamo risolto il problema nascosto con il carattere di nuova riga, introdotto costanti per una maggiore chiarezza e ampliato la funzionalità aggiungendo il supporto per la barra di stato e gli output delle finestre di dialogo. Inoltre, separare la logica di convalida in funzioni più piccole e mirate garantisce che il nostro codice sia più facile da mantenere ed estendere in futuro.

Questa rielaborazione dimostra come piccoli cambiamenti possano migliorare la struttura e la funzionalità complessive della nostra libreria, aprendo la strada a una maggiore flessibilità e riusabilità man mano che il nostro progetto cresce.