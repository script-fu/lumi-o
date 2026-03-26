---
title: "Refactoring di nuovo"
type: docs
weight: 5
---
As the helper library grows, it becomes harder to follow at a glance. Refactor again to keep each function small and single-purpose.

### Abbattere la complessità

To make the function easier to follow and maintain, break it down into smaller, focused functions. Start by separating validation from message routing.

### Crea una funzione di convalida

We can take the part of the function that validates the `message` and `output` arguments and move it into a separate function. This way, the core `send-message` function doesn’t need to worry about validation, making it easier to follow.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Semplifica l'invio dei messaggi

Now that the validation has been moved to a separate function, the `send-message` function can focus on just sending the message. It will be much simpler, as it only handles the specific task of directing the message to the correct destination.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Message console
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

  ;; Restore the default message handler to the Message console
  (lumi-message-set-handler 2))
```

### Breaking Down Further: Separate Each Output Handler

Each type of message output (GUI, Message console, Terminal) can be moved into its own function. This allows for easier testing, modification, and potential extension in the future.

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

  ;; Restore the default message handler to the Message console
  (lumi-message-set-handler 2))
```

### Riutilizzo della convalida in ciascuna funzione di invio

Poiché la convalida è una parte importante per garantire che sia il messaggio che l'output siano corretti, è opportuno che ciascuna funzione `send-*` esegua la propria convalida. This ensures that no matter which output is called, we always check the inputs first.

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

See that we've removed the validation from the send-message function and shifted the responsibility to each individual output function. Questa modifica garantisce che ciascuna destinazione (GUI, console messaggi, terminale) gestisca la propria convalida, semplificando la funzione di invio del messaggio e mantenendo la logica di convalida più vicina a dove è necessaria.

Questo approccio può semplificare la funzione send-message, rendendola un _dispatcher_, garantendo al contempo che ciascuna funzione send-to-* convalidi correttamente il messaggio prima dell'elaborazione.

By moving the validation into each send-to-* function, we’ve made them reusable as standalone functions. Ciò significa che possiamo chiamare direttamente qualsiasi funzione send-to-gui, send-to-error-console o send-to-terminal senza fare affidamento sulla funzione send-message dispatcher. Ognuna di queste funzioni ora gestisce completamente la propria logica e può essere utilizzata indipendentemente in altre parti del codice o in altri plug-in, rendendo il codice più modulare e flessibile.

## Vantaggi del refactoring

- **Clear Separation of Concerns**: Each function now handles only one responsibility, making the code easier to understand.
- **Extensibility**: Adding new output types is straightforward. You simply define a new function like `send-to-file` or `send-to-logger`, and then add a case in the `cond` statement.
- **Reusability**: Each of these output handling functions can be reused elsewhere in your project or shared among multiple plug-ins.
- **Coerenza**: riutilizzando la funzione di convalida in ciascuna funzione `send-to-*`, ti assicuri che tutti gli output siano correttamente convalidati, rendendo il codice più robusto.

Una versione della libreria rifattorizzata:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Message console
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

  ;; Restore the default message handler to the Message console
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

È tutto quello che possiamo fare? NO! c'è altro da fare, continua a leggere.