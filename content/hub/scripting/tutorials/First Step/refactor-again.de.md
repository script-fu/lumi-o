---
title: "Erneut umgestalten"
type: docs
weight: 5
---
Je größer die Hilfsbibliothek wird, desto schwieriger wird es, sie auf einen Blick zu verfolgen. Erneut umgestalten, um jede Funktion klein und zweckgebunden zu halten.

### Komplexität aufschlüsseln

Um die Funktion einfacher verfolgen und warten zu können, unterteilen Sie sie in kleinere, fokussierte Funktionen. Trennen Sie zunächst die Validierung vom Nachrichtenrouting.

### Erstellen Sie eine Validierungsfunktion

Wir können den Teil der Funktion, der die Argumente `message` und `output` validiert, nehmen und ihn in eine separate Funktion verschieben. Auf diese Weise muss sich die Kernfunktion `send-message` nicht um die Validierung kümmern, was die Befolgung erleichtert.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Vereinfachen Sie das Versenden von Nachrichten

Nachdem die Validierung nun in eine separate Funktion verschoben wurde, kann sich die Funktion `send-message` auf das reine Senden der Nachricht konzentrieren. Es wird viel einfacher sein, da es nur die spezifische Aufgabe übernimmt, die Nachricht an das richtige Ziel weiterzuleiten.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Weitere Aufschlüsselung: Trennen Sie jeden Ausgabehandler

Jede Art der Meldungsausgabe (GUI, Fehlerkonsole, Terminal) kann in eine eigene Funktion verschoben werden. Dies ermöglicht einfachere Tests, Änderungen und mögliche Erweiterungen in der Zukunft.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Wiederverwendung der Validierung in jeder Sendefunktion

Da die Validierung ein wichtiger Teil der Sicherstellung ist, dass sowohl die Nachricht als auch die Ausgabe korrekt sind, ist es sinnvoll, dass jede `send-*` Funktion ihre eigene Validierung durchführt. Dies stellt sicher, dass wir unabhängig davon, welche Ausgabe aufgerufen wird, immer zuerst die Eingaben überprüfen.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Beachten Sie, dass wir die Validierung aus der Funktion zum Senden von Nachrichten entfernt und die Verantwortung auf jede einzelne Ausgabefunktion verlagert haben. Diese Änderung stellt sicher, dass jedes Ziel (GUI, Fehlerkonsole, Terminal) seine eigene Validierung durchführt, wodurch die Funktion zum Senden von Nachrichten rationalisiert wird und die Validierungslogik näher an der Stelle bleibt, an der sie benötigt wird.

Dieser Ansatz kann die Funktion „Nachricht senden“ vereinfachen und sie zu einem _Dispatcher_ machen, während gleichzeitig sichergestellt wird, dass jede Funktion „Senden an*“ die Nachricht vor der Verarbeitung korrekt validiert.

Durch die Verlagerung der Validierung in jede send-to-*-Funktion haben wir sie als eigenständige Funktionen wiederverwendbar gemacht. Das bedeutet, dass wir jede der Funktionen „Send-to-GUI“, „Send-to-Error-Console“ oder „Send-to-Terminal“ direkt aufrufen können, ohne auf die Funktion „Send-Message Dispatcher“ angewiesen zu sein. Jede dieser Funktionen verwaltet nun vollständig ihre eigene Logik und kann unabhängig in anderen Teilen des Codes oder in anderen Plug-Ins verwendet werden, wodurch Ihr Code modularer und flexibler wird.

## Vorteile des Refactorings

- **Klare Trennung der Belange**: Jede Funktion übernimmt jetzt nur noch eine Verantwortung, wodurch der Code leichter verständlich wird.
- **Erweiterbarkeit**: Das Hinzufügen neuer Ausgabetypen ist unkompliziert. Sie definieren einfach eine neue Funktion wie `send-to-file` oder `send-to-logger` und fügen dann einen Fall in der `cond`-Anweisung hinzu.
- **Wiederverwendbarkeit**: Jede dieser Ausgabeverarbeitungsfunktionen kann an anderer Stelle in Ihrem Projekt wiederverwendet oder von mehreren Plug-Ins gemeinsam genutzt werden.
- **Konsistenz**: Durch die Wiederverwendung der Validierungsfunktion in jeder `send-to-*` Funktion stellen Sie sicher, dass alle Ausgaben ordnungsgemäß validiert werden, wodurch der Code robuster wird.

Eine überarbeitete Bibliotheksversion:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Ist das alles, was wir tun können? NEIN! Es gibt noch viel zu tun, bitte lesen Sie weiter.