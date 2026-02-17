---
title: "Refactoring di nuovo"
type: docs
weight: 5
---
Man mano che la libreria helper cresce, diventa più difficile seguirla a colpo d'occhio. Effettuare nuovamente il refactoring per mantenere ciascuna funzione piccola e monouso.

### Abbattere la complessità

Per rendere la funzione più facile da seguire e gestire, suddividila in funzioni più piccole e mirate. Inizia separando la convalida dal routing dei messaggi.

### Crea una funzione di convalida

Possiamo prendere la parte della funzione che convalida gli argomenti `message` e `output` e spostarla in una funzione separata. In questo modo, la funzione principale `send-message` non deve preoccuparsi della convalida, rendendola più facile da seguire.

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

Ora che la convalida è stata spostata in una funzione separata, la funzione `send-message` può concentrarsi solo sull'invio del messaggio. Sarà molto più semplice, poiché si occuperà solo del compito specifico di indirizzare il messaggio alla destinazione corretta.

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

### Ulteriore analisi: separare ciascun gestore di output

Ogni tipo di output del messaggio (GUI, Console errori, Terminale) può essere spostato nella propria funzione. Ciò consente test, modifiche e potenziali estensioni più semplici in futuro.

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

### Riutilizzo della convalida in ciascuna funzione di invio

Poiché la convalida è una parte importante per garantire che sia il messaggio che l'output siano corretti, è opportuno che ciascuna funzione `send-*` esegua la propria convalida. Ciò garantisce che, indipendentemente dall'output chiamato, controlliamo sempre prima gli input.

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

Nota che abbiamo rimosso la convalida dalla funzione di invio del messaggio e spostato la responsabilità su ogni singola funzione di output. Questa modifica garantisce che ciascuna destinazione (GUI, Console errori, Terminale) gestisca la propria convalida, semplificando la funzione di invio del messaggio e mantenendo la logica di convalida più vicina a dove è necessaria.

Questo approccio può semplificare la funzione send-message, rendendola un _dispatcher_, garantendo al contempo che ciascuna funzione send-to-* convalidi correttamente il messaggio prima dell'elaborazione.

Spostando la convalida in ciascuna funzione send-to-*, le abbiamo rese riutilizzabili come funzioni autonome. Ciò significa che possiamo chiamare direttamente qualsiasi funzione send-to-gui, send-to-error-console o send-to-terminal senza fare affidamento sulla funzione send-message dispatcher. Ognuna di queste funzioni ora gestisce completamente la propria logica e può essere utilizzata indipendentemente in altre parti del codice o in altri plug-in, rendendo il codice più modulare e flessibile.

## Vantaggi del refactoring

- **Chiara separazione delle preoccupazioni**: ogni funzione ora gestisce solo una responsabilità, rendendo il codice più facile da comprendere.
- **Estensibilità**: aggiungere nuovi tipi di output è semplice. È sufficiente definire una nuova funzione come `send-to-file` o `send-to-logger`, quindi aggiungere un caso nell'istruzione `cond`.
- **Riutilizzabilità**: ciascuna di queste funzioni di gestione dell'output può essere riutilizzata altrove nel progetto o condivisa tra più plug-in.
- **Coerenza**: riutilizzando la funzione di convalida in ciascuna funzione `send-to-*`, ti assicuri che tutti gli output siano correttamente convalidati, rendendo il codice più robusto.

Una versione della libreria rifattorizzata:

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

È tutto quello che possiamo fare? NO! c'è altro da fare, continua a leggere.