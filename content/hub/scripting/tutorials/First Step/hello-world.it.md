---
title: "Ciao mondo!"
type: docs
weight: 1
---
Questo tutorial illustra la struttura minima di un plug-in Scheme. Alcune righe sono “boilerplate”: sono necessarie a Lumi per caricare il file, anche se non le capisci ancora del tutto.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

Ad alto livello dovrai:

1. Definire una funzione
2. Registrarlo in modo che appaia nel Database delle Procedure
3. (Facoltativo) Aggiungi una voce di menu
4. Installare il file in una cartella di plug-in

### Definire una funzione

Una funzione, nota anche come _procedura_, è un pezzo di codice con un nome e uno scopo, prende un input e produce un output.

**Ingresso** > **_Funzione_** > **Uscita**

### Registra la funzione

La registrazione è l'atto di inserire il nome della funzione in un elenco in modo che Lumi ne sia a conoscenza.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Collegamento al Menù

Questo dice a Lumi dove trovare la tua funzione nel suo sistema di menu.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Verrà visualizzato il menu "Funky" nella barra del menu principale. Modificare il percorso per inserire il plug-in altrove. Il percorso `<Image>/Funky` indica che il plug-in verrà visualizzato nella categoria del menu **Immagine**. È possibile modificare `<Image>` in `<Tools>`, `<Filters>`, ecc., a seconda di dove si desidera che appaia il plug-in.

### Commenti

In Scheme, il linguaggio base di Scheme, i commenti vengono generalmente fatti precedendo un'utile riga di testo con `;;`. L'utilizzo dei commenti dipenderà dalla tua fluidità come programmatore: se codifichi occasionalmente, più commenti ti saranno utili. Se codifichi continuamente, il codice sarà facile da leggere come lo sarebbe il commento. Inoltre, quando si programma in modo funzionale, il codice tende a diventare sufficientemente descrittivo da poter essere letto come uno script.

### Sintassi

Il codice tende ad avere piccole regole su come posizionare gli elementi in una riga, in modo da poter leggere facilmente la riga. Ad esempio, una frase può contenere uno spazio dopo una virgola o un punto. Aiuta la leggibilità.

Il codice può organizzare le cose in modo simile, il che a prima vista può sembrare strano:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Codice di esempio

Ecco l'esempio completo. La maggior parte delle procedure Lumi hanno il prefisso `lumi-`. Ad esempio, `lumi-message` stampa una stringa nel gestore messaggi configurato.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


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

### Installa il plug-in

1. Vai su **Lumi -> Modifica -> Preferenze -> Cartelle -> Plug-in**.
2. Aggiungere la cartella dei plug-in [repo](/hub/scripting/tools/git) all'elenco.
3. Crea una cartella per il plug-in e salva il codice di esempio sopra come `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Fare clic con il pulsante destro del mouse sul file `hello-world.scm`.
5. Vai su **Proprietà -> Autorizzazioni -> Consenti l'esecuzione del file come programma**.
6. Riavvia Lumi.

### Prova il plug-in

Il plug-in dovrebbe ora apparire nel menu "Funky" nella finestra principale di Lumi. Fare clic e dovrebbe essere visualizzato il messaggio "Hello world!" messaggio. Prova a modificare il codice, ad esempio cambiando il testo del messaggio, e salva il file. Quando esegui nuovamente il plug-in, le modifiche verranno applicate senza riavviare Lumi.

Prova a sperimentare modificando il percorso del menu. Ad esempio, `"<Image>/File"` lo inserirà nel menu File e `"<Image>/File/Funky"` creerà una nuova sezione nel menu File. Questo è un ottimo modo per personalizzare la posizione in cui appare il plug-in e per organizzare i tuoi strumenti.