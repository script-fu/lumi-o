---
title: "Refactoring"
type: docs
weight: 2
---
Una volta che abbiamo una funzione funzionante, possiamo fare un passo indietro e pensare a come strutturare al meglio il nostro codice. L'obiettivo è rendere il nostro plug-in il più chiaro, comprensibile e gestibile possibile. Questo processo di miglioramento e perfezionamento della struttura del codice esistente senza modificarne il comportamento è noto come refactoring.

Ecco di nuovo la funzione iniziale:

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

Il nome-funzione è il nome della funzione e il parametro è ciò che la funzione accetta come input. Il corpo è il blocco di codice che viene eseguito quando viene chiamata la funzione.

Forma astratta:

```scheme
(define (function-name parameter)
  body)
```

### Ripetizione del codice

Rimuovere la ripetizione in anticipo. `(lumi-message "Hello world!\n")` viene ripetuto due volte e la stringa del messaggio viene ripetuta tre volte. Una variabile risolve la stringa ripetuta.

### Variabili

In Scheme una variabile ha un "ambito", di cui è nota, e tale ambito viene impostato utilizzando un'istruzione `let`. La variabile è associata a un valore nella parte vincolante e la variabile ha ambito nel corpo let. La variabile è nota solo all'interno del blocco let e non è possibile accedervi al di fuori di esso.

```scheme
(let ((variable value))
  body)
```

Introducendo una variabile chiamata "messaggio":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

Nel nostro esempio abbiamo utilizzato una variabile chiamata "messaggio" legata ad una stringa "Ciao mondo!\n". Questo ci permette di modificare il contenuto del messaggio una volta anziché tre, riducendo la possibilità di errori e rendendo il codice più flessibile.

### Funzioni di estrazione

Nella programmazione funzionale, il refactoring del codice per estrarre la logica riutilizzabile in funzioni separate è una pratica comune. In questo modo, la **funzione principale** diventa molto più semplice e più focalizzata sul suo obiettivo di alto livello, mentre la **funzione estratta** appare più complessa perché gestisce la logica dettagliata. Ciò è intenzionale e in linea con i principi fondamentali della programmazione funzionale, come modularità, separazione delle preoccupazioni e leggibilità. Ecco il refactoring
Ciao mondo! dopo l'estrazione.

Estrarre la logica:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Simboli
Nell'esempio sopra viene utilizzato un tipo di dati chiamato simbolo, come "gui". I simboli vengono passati come parametri alla funzione di invio del messaggio e possono essere utilizzati per prendere semplici decisioni condizionali. Come le chiavi simboliche, sono identificatori univoci. Per ulteriori informazioni sui simboli, visitare [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Semplificazione della funzione principale

Nella funzione originale (scheme-hello-world), tutta la logica per l'invio di messaggi a diversi output (GUI, Console errori, Terminale) era mescolata nella funzione principale. Dopo il refactoring, la funzione principale si concentra semplicemente su **cosa è necessario fare**, inviando il messaggio a destinazioni diverse.

La funzione main refactoring è più semplice:

- Dichiara chiaramente il suo scopo: inviare lo stesso messaggio a più uscite.
- Evita di ingombrare la logica principale con codice ripetitivo come l'impostazione di gestori di messaggi per output diversi.
- È più facile da leggere e comprendere a colpo d'occhio.

### La complessità della funzione estratta

Al contrario, la funzione **(invia messaggio)** è dove risiede la logica dettagliata. Ora gestisce le variazioni di comportamento per ciascun output (GUI, Console errori, Terminale). La funzione è un po' più complessa di prima, ma ora è **centralizzata** e **isolata**.

## Relativo a ciò con la programmazione funzionale

Nella programmazione funzionale, le funzioni sono viste come **cittadini di prima classe**, nel senso che possono essere riutilizzate, scambiate e combinate per formare comportamenti più complessi. L'obiettivo è:- **Scomponi i problemi** in parti più piccole e indipendenti.
- **Isolare la complessità** in funzioni più piccole che gestiscono attività specifiche, come `send-message`.
- **Mantieni semplici le funzioni di livello superiore** in modo che possano concentrarsi sull'orchestrazione del flusso di dati e azioni, senza dover conoscere i dettagli di come viene eseguita ciascuna attività.
- **Separazione degli interessi**: la funzione si occupa di come viene inviato il messaggio in base al tipo di output, isolando questa logica dalla funzione principale.
- **Modularità**: gestendo tutta la logica di invio dei messaggi in un unico posto, possiamo facilmente apportare modifiche (come aggiungere nuove opzioni di output) senza alterare la funzione principale.
- **Riutilizzabilità**: la funzione `send-message` è riutilizzabile, il che significa che se dobbiamo inviare un messaggio a più output altrove nel nostro codice, possiamo semplicemente chiamare questa funzione anziché riscrivere una logica simile.

Mediante il refactoring, la funzione principale in questo esempio diventa una dichiarazione **dichiarativa** di ciò che sta accadendo ("invia un messaggio a tre posti"), mentre la complessità di come inviare tali messaggi viene astratta nella funzione `send-message`.