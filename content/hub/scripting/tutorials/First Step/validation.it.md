---
title: "Validazione"
type: docs
weight: 4
---
Quando creiamo plug-in robusti, è importante garantire che le nostre funzioni gestiscano gli errori in modo corretto e funzionino come previsto, anche in caso di uso improprio o input imprevisti. Validation helps protect the integrity of the function and prevent crashes or unintended behavior.

Let’s look at how we can improve the `send-message` function by adding validation checks to ensure it handles inputs correctly.

### Convalida gli input

Before sending a message, we should ensure the `output` argument passed to the `send-message` function is valid. We can add a check to confirm that the output destination is one of the expected values (gui, error-console, or terminal).

Esempio:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Message console
  (lumi-message-set-handler 2))
```

In this example, we use `member` to check if the `output` argument is valid. If not, the function raises an error with a clear message, preventing invalid values from causing issues.

### Gestisci i messaggi vuoti

It’s also useful to ensure that the `message` argument is valid. For example, if an empty string or #f (false) is passed as the message, the function should handle this gracefully.

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

This approach ensures that the function always receives valid input, improving its reliability and preventing unexpected behavior.

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
           (display message)))))

  ;; Restore the default message handler to the Message console
  (lumi-message-set-handler 2))
```

In questa versione:
- The function checks if the `message` is empty or invalid first. Se il messaggio è valido, si passa alla verifica se `output` è uno dei valori accettati (`gui`, `error-console` o `terminal`).
- If both checks pass, the message is sent to the appropriate output. Otherwise, an error message is raised with a clear explanation.
- An additional check is made to ensure the message is also a string.

Questa funzione di convalida combinata mantiene il codice più pulito e garantisce che entrambi gli input vengano convalidati prima che venga intrapresa qualsiasi azione, rendendo la funzione più robusta. Notice, we are also building in a debug messaging system. Quando il
code fails, we get a reason, a reason we wrote ourselves.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```