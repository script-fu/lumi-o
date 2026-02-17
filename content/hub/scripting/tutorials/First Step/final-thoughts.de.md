---
title: "Letzte Gedanken"
type: docs
weight: 10
---
Sie verfügen nun über ein Arbeitsprozedur-Plug-in und eine kleine Hilfsbibliothek. In dieser Serie wurden die Kernmuster vorgestellt, die Sie in den meisten Lumi-Skripten verwenden werden:

- Funktionen: Die Bausteine unserer Plug-Ins.
- Refactoring: Verbesserung der Codestruktur bei gleichzeitiger Beibehaltung der Funktionalität.
- Codebibliotheken: Zentralisierung wiederverwendbarer Funktionen, um unseren Code sauber und modular zu halten.
- Validierungstechniken: Sicherstellen, dass Eingaben gültig sind, bevor unsere Kernlogik ausgeführt wird.

Sie haben auch die Grundlagen der Verwendung von Git kennengelernt, um Änderungen zu verfolgen und eine saubere Projektstruktur beizubehalten. Dieser Workflow erleichtert die Iteration, ohne dass Arbeitsversionen verloren gehen.

Hier ist die endgültige Version unseres Haupt-Plug-in-Codes:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Bibliothekscode:

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

## Fazit

Durch die Umgestaltung der Messaging-Helfer in eine kleine Bibliothek bleibt das Plug-in auf die Absicht konzentriert und die Bibliothek enthält die Implementierungsdetails. Durch Validierung und konsistentes Nachrichtenrouting bleiben Fehler vorhersehbar.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Nächste Schritte:

- Verschieben Sie wiederverwendbare Helfer in eine dedizierte Bibliotheksdatei.
- Halten Sie Plug-ins klein und benennen Sie die Prozeduren entsprechend ihrer Funktion.
- Validierung an Grenzen hinzufügen (Eingaben, Dateipfade, Menüoptionen).

Behalten Sie das Endergebnis als zwei Dateien in Ihrem Plug-In-Repository:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`