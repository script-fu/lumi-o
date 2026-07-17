---
title: "Laden"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
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
;; Funktion zur Ausgabe von Nachrichten an verschiedene Ziele
(define (send-message message output)
  (cond
    ;; An die Message Console senden
    ((eq? output 'error-console)
       ;; Handler auf Message console setzen
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; An das GUI-Dialogfeld senden
    ((eq? output 'gui)
       ;; Handler auf GUI-Dialog setzen
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; An das Terminalfenster senden
    ((eq? output 'terminal)
       ;; Terminal-Ausgabe wird mit display verarbeitet
       (display message)))

  ;; Standard-Nachrichtenhandler auf die Message console zurücksetzen
  (lumi-message-set-handler 2))
```

### Laden Sie die Bibliotheksfunktion

Wir können diese Bibliotheksfunktion mit dem Befehl Scheme `load` laden;

Laden einer Bibliotheksdatei:

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

Hey! Wir haben jetzt etwas Einfacheres und Kürzeres zum Lesen, das sich quasi von selbst ohne Kommentare beschreibt. Dies ist die zufriedenstellende Schlussfolgerung des Refactorings.