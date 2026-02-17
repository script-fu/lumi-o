---
title: "Laden"
type: docs
weight: 3
---
Sobald eine Hilfsfunktion wächst, verschieben Sie sie in eine kleine Bibliotheksdatei. Dadurch bleibt das Plug-In fokussiert und der Helfer kann über mehrere Plug-Ins hinweg wiederverwendet werden.

### Erstellen Sie eine Bibliotheksfunktion

Wir können die Funktion „Nachricht senden“ verwenden und eine neue Datei mit diesem Inhalt erstellen. Speichern Sie die Datei in Ihrem Repo-Ordner, nicht im Plugins-Teil, vielleicht in der Nähe der obersten Ebene;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Dies ist Ihr Hauptverzeichnis zum Speichern Ihres Scheme-Codes.
  - **Bibliothek/**: Hier leben gemeinsame Funktionen wie `send-message.scm`.
  - **Plug-Ins/**: Hier werden Ihre individuellen Plug-Ins gespeichert.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Beispiel einer Bibliotheksfunktion send-message.scm

```scheme
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
```

### Laden Sie die Bibliotheksfunktion

Wir können diese Bibliotheksfunktion mit dem Befehl Scheme `load` laden;

Laden einer Bibliotheksdatei:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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

Hey! Wir haben jetzt etwas Einfacheres und Kürzeres zum Lesen, das sich quasi von selbst ohne Kommentare beschreibt. Dies ist die zufriedenstellende Schlussfolgerung des Refactorings.