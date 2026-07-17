---
title: "Libreria di messaggistica"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bfe459b9f717201d646bde29196fd66e6c3b19b3e9dbdb3338e0c853153e1c05
---
Nel corso del tempo, quella che era iniziata come un'unica funzione per inviare messaggi si è evoluta in un insieme di funzioni correlate. These functions now form the foundation of a **Messaging Library**, designed to handle output to different destinations, such as the GUI, Message console, and OS terminal.

### Perché una libreria di messaggistica?

Man mano che le nostre esigenze crescono, la gestione dei messaggi su più output richiede un approccio più modulare ed estensibile. Invece di una singola funzione che fa tutto, abbiamo suddiviso il processo in componenti riutilizzabili, consentendo una maggiore flessibilità. Questa libreria può ora essere utilizzata come strumento di messaggistica generico da cui possono prendere in prestito altri plug-in o funzioni.

### What Does the Messaging Library Do?

La Libreria Messaggistica attualmente include le seguenti funzioni:

- **send-to-gui**: Sends messages to the Lumi GUI dialog box.
- **send-to-error-console**: invia messaggi alla console Lumi Message.
- **send-to-terminal**: invia messaggi alla finestra del terminale.
- **send-message**: una funzione dispatcher che indirizza i messaggi all'output appropriato.
- **validate-message**: garantisce che il messaggio e l'output siano validi prima dell'invio.

### Espansione della Biblioteca

La **Libreria di messaggistica** può essere facilmente estesa per supportare output aggiuntivi. Ad esempio:

- **send-to-file**: Save messages to a log file.
- **send-to-logger**: integrazione con un sistema di registrazione esterno.
- **invio a notifica**: visualizza i messaggi come notifiche di sistema.

By following the same pattern of modular design and reusable functions, this library can grow into a comprehensive tool for handling all kinds of messaging tasks.

## Vantaggi di una libreria di messaggistica

- **Reusability**: The functions can be reused across different plug-ins or projects.
- **Modularity**: Each function handles one specific task, making the code easier to maintain and extend.
- **Consistency**: Using the same validation and message-handling functions ensures consistent behavior across the application.

La **Libreria dei messaggi** è l'inizio di un quadro più ampio che potrebbe semplificare il modo in cui i messaggi vengono gestiti nel tuo progetto. Man mano che la libreria cresce, nuovi plug-in possono facilmente attingere ad essa per inviare messaggi ovunque debbano andare.

We can adjust the file structure:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

E ricorda di regolare `load` nel plug-in principale:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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