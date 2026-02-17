---
title: "Überarbeitung"
type: docs
weight: 7
---
Dieser Schritt behebt ein subtiles Verhalten im Messaging-Beispiel.

Als Nachricht haben wir die Zeichenfolge „Hello world!\n“ übergeben. Das „\n“ ist eine besondere Art von Zeichen, ein „Escape“-Zeichen. Es weist den Ausgabedruck an, eine neue Zeile zu beginnen. In Scheme wird außerdem erzwungen, dass eine an die Statusleiste gesendete Nachricht als GUI-Box angezeigt wird.

Der Helfer `send-to-gui` sendet Nachrichten an ein Lumi-Dialogfeld.

Aktualisieren Sie die Nachrichteninhalte und -ziele, damit sich das Beispiel konsistent verhält.

Entfernen des Escape-Zeichens und Erweitern der Funktionen:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

Ersetzen Sie magische Zahlen durch die von Lumi bereitgestellten Konstanten (z. B. `MESSAGE-BOX` und `ERROR-CONSOLE`).

Teilen Sie dann die Validierung in zwei Funktionen auf, damit sie von mehreren Aufrufseiten aus wiederverwendet werden kann.

- (ist-gültiger-String?) Um zu überprüfen, ob ein String ein String und kein leerer String ist, innerhalb einer send-to*-Funktion.
- (is-valid-output-display?) Um zu überprüfen, ob ein bestimmtes Ausgabeziel in der Funktion „Nachricht senden“ gültig ist.

Überarbeiten Sie die Bibliothek:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Fazit

Durch die Überarbeitung unserer Messaging-Bibliothek haben wir sie robuster und zuverlässiger gemacht. Wir haben das versteckte Problem mit dem Zeilenumbruchzeichen behoben, zur besseren Übersichtlichkeit Konstanten eingeführt und die Funktionalität um Unterstützung für die Statusleiste und die Dialogfeldausgaben erweitert. Darüber hinaus stellt die Aufteilung der Validierungslogik in kleinere, fokussierte Funktionen sicher, dass unser Code in Zukunft einfacher zu warten und zu erweitern ist.

Diese Überarbeitung zeigt, wie kleine Änderungen die Gesamtstruktur und Funktionalität unserer Bibliothek verbessern und den Weg für mehr Flexibilität und Wiederverwendbarkeit ebnen können, wenn unser Projekt wächst.