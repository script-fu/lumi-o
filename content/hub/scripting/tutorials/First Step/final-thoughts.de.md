---
title: "Letzte Gedanken"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
Sie verfügen nun über ein Arbeitsprozedur-Plug-in und eine kleine Hilfsbibliothek. In dieser Serie wurden die Kernmuster vorgestellt, die Sie in den meisten Lumi-Skripten verwenden werden:

- Funktionen: Die Bausteine unserer Plug-Ins.
- Refactoring: Verbesserung der Codestruktur bei gleichzeitiger Beibehaltung der Funktionalität.
- Codebibliotheken: Zentralisierung wiederverwendbarer Funktionen, um unseren Code sauber und modular zu halten.
- Validierungstechniken: Sicherstellen, dass Eingaben gültig sind, bevor unsere Kernlogik ausgeführt wird.

Sie haben auch die Grundlagen der Verwendung von Git kennengelernt, um Änderungen zu verfolgen und eine saubere Projektstruktur beizubehalten. Dieser Workflow erleichtert die Iteration, ohne dass Arbeitsversionen verloren gehen.

Hier ist die endgültige Version unseres Haupt-Plug-in-Codes:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
;; Zweck: Sendet eine Nachricht an die Statusleiste, gibt #t bei Erfolg zurück
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Zweck: Sendet eine Nachricht an das Dialogfeld, gibt #t bei Erfolg zurück
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Zweck: Sendet eine Nachricht an die Fehlerkonsole, gibt #t bei Erfolg zurück
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Zweck: Sendet eine Nachricht an das terminal, gibt #t bei Erfolg zurück
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Zweck: Leitet eine Nachricht an die passende Ausgabe weiter, gibt #t bei Erfolg zurück
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Zweck: Prüft, dass die Nachricht eine nicht leere Zeichenkette ist, gibt #t bei Gültigkeit zurück
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Zweck: Prüft, dass die Ausgabe ein gültiges Ziel ist, gibt #t bei Gültigkeit zurück
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