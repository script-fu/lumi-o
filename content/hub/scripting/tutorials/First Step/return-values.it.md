---
title: "Valori restituiti"
type: docs
weight: 8
---
I valori restituiti sono importanti perché consentono di controllare il flusso senza uno stato aggiuntivo. In Scheme, l'ultima espressione valutata diventa il valore restituito.

Questa pagina utilizza gli helper di convalida dell'esempio di messaggistica per mostrare come i valori restituiti espliciti semplificano la composizione del codice.

### Cos'è un valore restituito?

In Scheme, il valore restituito di una funzione è determinato dall'ultima espressione valutata dalla funzione. Ciò significa che qualunque valore restituisca l'ultima riga di codice nella funzione verrà restituito come risultato della funzione. Se non viene restituito alcun valore in modo esplicito, la funzione restituisce `#f` (falso) o `undefined`.

Rivisitiamo la funzione di convalida, (is-valid-string?)

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

In questa funzione, se il messaggio non è valido, viene generato un errore. Tuttavia, se il messaggio è valido, non viene fornito alcun valore restituito esplicito e la funzione restituisce `#f` per impostazione predefinita.

### Rendere espliciti i valori restituiti

Possiamo migliorare questo aspetto rendendo il valore restituito più esplicito. Ad esempio, potremmo restituire `#t` (true) se il messaggio è valido:

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

In questa versione, la funzione restituirà `#t` quando il messaggio è valido, fornendo un risultato chiaro. Ciò consente alla funzione di essere utilizzata in modo più flessibile in altri contesti in cui è necessario un risultato booleano.

### Utilizzo efficace dei valori restituiti

Decidendo cosa restituiscono le nostre funzioni, possiamo renderle più prevedibili e utili. Restituire valori come `#t`, `#f` o un risultato specifico ci dà un maggiore controllo su come la funzione interagisce con il resto del codice. Ad esempio, puoi utilizzare il valore restituito per prendere ulteriori decisioni nella funzione chiamante o passarlo come argomento a un'altra funzione.

Ecco un semplice esempio di utilizzo di un valore restituito per controllare il flusso della logica:

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

In questo caso, (send-message) si basa sul valore restituito di (is-valid-output-display?) per decidere se continuare.
L'istruzione condizionale `cond` verrà saltata se il primo test fallisce. Inoltre, nota come si legge in modo abbastanza naturale, se la visualizzazione dell'output è valida?

## Se la logica dell'istruzione nello schema

Prima dell'esempio della libreria rifattorizzata, ecco una rapida rassegna della logica condizionale. Lo schema utilizza `if` per scegliere tra due percorsi.

Ecco una forma semplice di un'istruzione `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Questa struttura controlla la condizione e, se la condizione è vera, esegue la prima azione. Se la condizione è falsa, esegue la seconda azione.

Nei casi in cui è necessario eseguire più azioni quando la condizione è vera o falsa, è possibile utilizzare `begin` per raggrupparle insieme:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Ciò consente di gestire situazioni più complesse, in cui è necessario eseguire più espressioni o istruzioni a seconda del risultato del test condizionale.

Ok, ecco il codice della libreria con i valori restituiti incorporati e utilizzati per controllare il processo di esecuzione.

### Refactoring con valori restituiti

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

I valori restituiti sono una parte fondamentale per rendere le funzioni flessibili e riutilizzabili. Decidendo attentamente cosa deve restituire ciascuna funzione, possiamo garantire che le nostre funzioni interagiscano bene tra loro e forniscano informazioni utili al resto del codice. Che si tratti di restituire `#t` o `#f`, o qualcosa di più specifico, i valori restituiti ci danno un modo per controllare il flusso dei nostri programmi e gestire vari risultati.