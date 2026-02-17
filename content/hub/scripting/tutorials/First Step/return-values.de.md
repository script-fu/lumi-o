---
title: "Rückgabewerte"
type: docs
weight: 8
---
Rückgabewerte sind wichtig, weil Sie damit den Fluss ohne zusätzlichen Status steuern können. In Scheme wird der zuletzt ausgewertete Ausdruck zum Rückgabewert.

Auf dieser Seite werden die Validierungshilfen aus dem Messaging-Beispiel verwendet, um zu zeigen, wie explizite Rückgabewerte das Verfassen von Code erleichtern.

### Was ist ein Rückgabewert?

In Scheme wird der Rückgabewert einer Funktion durch den letzten Ausdruck bestimmt, den die Funktion auswertet. Dies bedeutet, dass als Ergebnis der Funktion zurückgegeben wird, was auch immer die letzte Codezeile der Funktion auswertet. Wenn kein Wert explizit zurückgegeben wird, gibt die Funktion `#f` (falsch) oder `undefined` zurück.

Schauen wir uns noch einmal die Validierungsfunktion an (is-valid-string?)

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Wenn in dieser Funktion die Nachricht ungültig ist, wird ein Fehler ausgegeben. Wenn die Nachricht jedoch gültig ist, wird kein expliziter Rückgabewert angegeben und die Funktion gibt standardmäßig `#f` zurück.

### Rückgabewerte explizit machen

Wir können dies verbessern, indem wir den Rückgabewert expliziter machen. Beispielsweise könnten wir `#t` (true) zurückgeben, wenn die Nachricht gültig ist:

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

In dieser Version gibt die Funktion `#t` zurück, wenn die Nachricht gültig ist, und liefert so ein eindeutiges Ergebnis. Dadurch kann die Funktion flexibler in anderen Kontexten verwendet werden, in denen ein boolesches Ergebnis benötigt wird.

### Rückgabewerte effektiv nutzen

Indem wir entscheiden, was unsere Funktionen zurückgeben, können wir sie vorhersehbarer und nützlicher machen. Durch die Rückgabe von Werten wie `#t`, `#f` oder eines bestimmten Ergebnisses haben wir mehr Kontrolle darüber, wie die Funktion mit dem Rest des Codes interagiert. Sie können den Rückgabewert beispielsweise verwenden, um weitere Entscheidungen in der aufrufenden Funktion zu treffen oder ihn als Argument an eine andere Funktion zu übergeben.

Hier ist ein einfaches Beispiel für die Verwendung eines Rückgabewerts zur Steuerung des Logikflusses:

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

In diesem Fall verlässt sich (send-message) auf den Rückgabewert von (is-valid-output-display?), um zu entscheiden, ob fortgefahren werden soll.
Die bedingte Anweisung `cond` wird übersprungen, wenn der erste Test fehlschlägt. Beachten Sie auch, dass die Ausgabe auf recht natürliche Weise angezeigt wird. Ist die Ausgabe gültig?

## If-Anweisungslogik im Schema

Vor dem Beispiel der umgestalteten Bibliothek finden Sie hier einen kurzen Überblick über die bedingte Logik. Das Schema verwendet `if`, um zwischen zwei Pfaden zu wählen.

Hier ist eine einfache Form einer `if`-Anweisung:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Diese Struktur prüft die Bedingung und führt die erste Aktion aus, wenn die Bedingung wahr ist. Wenn die Bedingung falsch ist, wird die zweite Aktion ausgeführt.

In Fällen, in denen Sie mehrere Aktionen ausführen müssen, wenn die Bedingung wahr oder falsch ist, können Sie sie mit `begin` gruppieren:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Dadurch können Sie komplexere Situationen bewältigen, in denen abhängig vom Ergebnis des Bedingungstests mehrere Ausdrücke oder Anweisungen ausgeführt werden müssen.

Okay, hier ist der Bibliothekscode mit eingebetteten Rückgabewerten, der zur Steuerung des Ausführungsprozesses verwendet wird.

### Mit Rückgabewerten umgestaltet

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

Rückgabewerte sind ein wesentlicher Bestandteil, um Funktionen flexibel und wiederverwendbar zu machen. Indem wir sorgfältig entscheiden, was jede Funktion zurückgeben soll, können wir sicherstellen, dass unsere Funktionen gut miteinander interagieren und dem Rest des Codes nützliche Informationen liefern. Ob es darum geht, `#t` oder `#f` zurückzugeben oder etwas Spezifischeres: Rückgabewerte geben uns die Möglichkeit, den Ablauf unserer Programme zu steuern und verschiedene Ergebnisse zu verarbeiten.