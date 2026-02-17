---
title: "Validazione"
type: docs
weight: 4
---
Quando creiamo plug-in robusti, è importante garantire che le nostre funzioni gestiscano gli errori in modo corretto e funzionino come previsto, anche in caso di uso improprio o input imprevisti. La convalida aiuta a proteggere l'integrità della funzione e a prevenire arresti anomali o comportamenti non intenzionali.

Diamo un'occhiata a come possiamo migliorare la funzione `send-message` aggiungendo controlli di convalida per garantire che gestisca correttamente gli input.

### Convalida gli input

Prima di inviare un messaggio, dobbiamo assicurarci che l'argomento `output` passato alla funzione `send-message` sia valido. Possiamo aggiungere un controllo per confermare che la destinazione dell'output sia uno dei valori attesi (gui, console di errore o terminale).

Esempio:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In questo esempio, utilizziamo `member` per verificare se l'argomento `output` è valido. In caso contrario, la funzione genera un errore con un messaggio chiaro, impedendo che valori non validi causino problemi.

### Gestisci i messaggi vuoti

È anche utile assicurarsi che l'argomento `message` sia valido. Ad esempio, se come messaggio viene passata una stringa vuota o #f (falso), la funzione dovrebbe gestirlo in modo corretto.

Esempio di gestione di un messaggio vuoto:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
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

Questo approccio garantisce che la funzione riceva sempre input validi, migliorandone l'affidabilità e prevenendo comportamenti imprevisti.

### Esempio di convalida combinata

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In questa versione:
- La funzione controlla prima se `message` è vuoto o non valido. Se il messaggio è valido, si passa alla verifica se `output` è uno dei valori accettati (`gui`, `error-console` o `terminal`).
- Se entrambi i controlli vengono superati, il messaggio viene inviato all'output appropriato. In caso contrario, verrà generato un messaggio di errore con una spiegazione chiara.
- Viene effettuato un ulteriore controllo per garantire che anche il messaggio sia una stringa.

Questa funzione di convalida combinata mantiene il codice più pulito e garantisce che entrambi gli input vengano convalidati prima che venga intrapresa qualsiasi azione, rendendo la funzione più robusta. Nota: stiamo anche creando un sistema di messaggistica di debug. Quando il
il codice fallisce, otteniamo un motivo, un motivo che abbiamo scritto noi stessi.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```