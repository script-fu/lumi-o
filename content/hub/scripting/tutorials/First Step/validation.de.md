---
title: "Validierung"
type: docs
weight: 4
---
Beim Erstellen robuster Plug-Ins ist es wichtig sicherzustellen, dass unsere Funktionen Fehler ordnungsgemäß verarbeiten und wie erwartet funktionieren, selbst bei Missbrauch oder unerwarteten Eingaben. Die Validierung trägt dazu bei, die Integrität der Funktion zu schützen und Abstürze oder unbeabsichtigtes Verhalten zu verhindern.

Schauen wir uns an, wie wir die Funktion `send-message` verbessern können, indem wir Validierungsprüfungen hinzufügen, um sicherzustellen, dass Eingaben korrekt verarbeitet werden.

### Eingaben validieren

Bevor wir eine Nachricht senden, sollten wir sicherstellen, dass das an die Funktion `send-message` übergebene Argument `output` gültig ist. Wir können eine Prüfung hinzufügen, um zu bestätigen, dass das Ausgabeziel einer der erwarteten Werte ist (GUI, Fehlerkonsole oder Terminal).

Beispiel:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In diesem Beispiel verwenden wir `member`, um zu prüfen, ob das Argument `output` gültig ist. Wenn nicht, löst die Funktion einen Fehler mit einer klaren Meldung aus und verhindert so, dass ungültige Werte Probleme verursachen.

### Behandeln Sie leere Nachrichten

Es ist auch nützlich, sicherzustellen, dass das Argument `message` gültig ist. Wenn beispielsweise eine leere Zeichenfolge oder #f (false) als Nachricht übergeben wird, sollte die Funktion dies ordnungsgemäß verarbeiten.

Beispiel für den Umgang mit einer leeren Nachricht:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Dieser Ansatz stellt sicher, dass die Funktion immer gültige Eingaben erhält, wodurch ihre Zuverlässigkeit verbessert und unerwartetes Verhalten verhindert wird.

### Beispiel für eine kombinierte Validierung

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In dieser Version:
- Die Funktion prüft zunächst, ob das `message` leer oder ungültig ist. Wenn die Nachricht gültig ist, wird geprüft, ob `output` einer der akzeptierten Werte ist (`gui`, `error-console` oder `terminal`).
- Wenn beide Prüfungen erfolgreich sind, wird die Nachricht an den entsprechenden Ausgang gesendet. Andernfalls wird eine Fehlermeldung mit einer klaren Erklärung ausgegeben.
– Es wird eine zusätzliche Prüfung durchgeführt, um sicherzustellen, dass es sich bei der Nachricht auch um eine Zeichenfolge handelt.

Diese kombinierte Validierungsfunktion hält den Code sauberer und stellt sicher, dass beide Eingaben validiert werden, bevor Maßnahmen ergriffen werden, wodurch die Funktion robuster wird. Beachten Sie, dass wir auch ein Debug-Nachrichtensystem einbauen. Wenn die
Wenn der Code fehlschlägt, erhalten wir einen Grund, einen Grund, den wir selbst geschrieben haben.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```