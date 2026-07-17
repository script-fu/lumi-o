---
title: "Caricamento"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
url: "hub/scripting/tutorials/First Step/loading"
---
Non appena una funzione di supporto cresce, spostala in un piccolo file di libreria. Ciò mantiene il plug-in focalizzato e rende l'helper riutilizzabile su più plug-in.

### Crea una funzione di libreria

Possiamo prendere la funzione di invio del messaggio e creare un nuovo file con quello come contenuto. Salva il file nella cartella del repository, non nella parte dei plugin, magari vicino al livello più alto;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: questa è la directory principale in cui memorizzare il codice Scheme.
  - **library/**: qui risiedono le funzioni condivise come `send-message.scm`.
  - **plug-in/**: qui sono memorizzati i tuoi plug-in individuali.
    - **hello-world/**: Cartella per lo specifico plug-in «Hello World!».
      - **hello-world.scm**: File di script del plug-in.

Esempio di una funzione di libreria send-message.scm

```scheme
;; Funzione per gestire l'output dei messaggi verso varie destinazioni
(define (send-message message output)
  (cond
    ;; Invia alla console dei messaggi
    ((eq? output 'error-console)
       ;; Impostare il gestore su console dei messaggi
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Invia alla finestra di dialogo GUI
    ((eq? output 'gui)
       ;; Impostare il gestore sulla finestra di dialogo GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Invia alla finestra del terminale
    ((eq? output 'terminal)
       ;; L'output del terminal è gestito con display
       (display message)))

  ;; Ripristinare il gestore dei messaggi predefinito alla console dei messaggi
  (lumi-message-set-handler 2))
```

### Carica la funzione di libreria

Possiamo caricare quella funzione di libreria con il comando Scheme `load`;

Caricamento di un file di libreria:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

Ehi! Ora abbiamo qualcosa di più semplice e più breve da leggere, che si descrive da solo senza commenti. Questa è la conclusione soddisfacente del refactoring.