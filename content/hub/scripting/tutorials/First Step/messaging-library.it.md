---
title: "Libreria di messaggistica"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
url: "hub/scripting/tutorials/First Step/messaging-library"
---
Nel corso del tempo, quella che era iniziata come un'unica funzione per inviare messaggi si è evoluta in un insieme di funzioni correlate. Queste funzioni costituiscono ora la base di una **Libreria di messaggistica**, progettata per gestire l'output verso destinazioni diverse, come la GUI, la console Message e il terminale del sistema operativo.

### Perché una libreria di messaggistica?

Man mano che le nostre esigenze crescono, la gestione dei messaggi su più output richiede un approccio più modulare ed estensibile. Invece di una singola funzione che fa tutto, abbiamo suddiviso il processo in componenti riutilizzabili, consentendo una maggiore flessibilità. Questa libreria può ora essere utilizzata come strumento di messaggistica generico da cui possono prendere in prestito altri plug-in o funzioni.

### Cosa fa la libreria di messaggistica?

La Libreria Messaggistica attualmente include le seguenti funzioni:

- **send-to-gui**: invia messaggi alla finestra di dialogo GUI di Lumi.
- **send-to-error-console**: invia messaggi alla console Lumi Message.
- **send-to-terminal**: invia messaggi alla finestra del terminale.
- **send-message**: una funzione dispatcher che indirizza i messaggi all'output appropriato.
- **validate-message**: garantisce che il messaggio e l'output siano validi prima dell'invio.

### Espansione della Biblioteca

La **Libreria di messaggistica** può essere facilmente estesa per supportare output aggiuntivi. Ad esempio:

- **send-to-file**: salva i messaggi in un file di log.
- **send-to-logger**: integrazione con un sistema di registrazione esterno.
- **invio a notifica**: visualizza i messaggi come notifiche di sistema.

Seguendo lo stesso modello di progettazione modulare e funzioni riutilizzabili, questa libreria può crescere fino a diventare uno strumento completo per gestire ogni tipo di attività di messaggistica.

## Vantaggi di una libreria di messaggistica

- **Riutilizzabilità**: le funzioni possono essere riutilizzate in plug-in o progetti diversi.
- **Modularità**: ogni funzione gestisce un compito specifico, rendendo il codice più facile da mantenere ed estendere.
- **Coerenza**: usare le stesse funzioni di validazione e gestione dei messaggi garantisce un comportamento coerente nell'applicazione.

La **Libreria dei messaggi** è l'inizio di un quadro più ampio che potrebbe semplificare il modo in cui i messaggi vengono gestiti nel tuo progetto. Man mano che la libreria cresce, nuovi plug-in possono facilmente attingere ad essa per inviare messaggi ovunque debbano andare.

Possiamo adattare la struttura dei file:

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