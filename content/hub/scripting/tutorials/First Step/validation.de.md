---
title: "Validierung"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d5d160ddb40b6a09f1d92ebf0287ce6912dcc703702b7701c564688226e92842
---
Beim Erstellen robuster Plug-Ins ist es wichtig sicherzustellen, dass unsere Funktionen Fehler ordnungsgemäß verarbeiten und wie erwartet funktionieren, selbst bei Missbrauch oder unerwarteten Eingaben. Die Validierung trägt dazu bei, die Integrität der Funktion zu schützen und Abstürze oder unbeabsichtigtes Verhalten zu verhindern.

Schauen wir uns an, wie wir die Funktion `send-message` verbessern können, indem wir Validierungsprüfungen hinzufügen, um sicherzustellen, dass Eingaben korrekt verarbeitet werden.

### Eingaben validieren

Bevor wir eine Nachricht senden, sollten wir sicherstellen, dass das an die Funktion `send-message` übergebene Argument `output` gültig ist. Wir können eine Prüfung hinzufügen, um zu bestätigen, dass das Ausgabeziel einer der erwarteten Werte ist (GUI, Fehlerkonsole oder Terminal).

Beispiel:

```scheme
(define (send-message message output)
  ;; Validiert das Ausgabeargument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; An die Message Console senden
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; An das GUI-Dialogfeld senden
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; An das Terminalfenster senden
      ((eq? output 'terminal)
         (display message))))

  ;; Standard-Nachrichtenhandler auf die Message console zurücksetzen
  (lumi-message-set-handler 2))
```

In diesem Beispiel verwenden wir `member`, um zu prüfen, ob das Argument `output` gültig ist. Wenn nicht, löst die Funktion einen Fehler mit einer klaren Meldung aus und verhindert so, dass ungültige Werte Probleme verursachen.

### Behandeln Sie leere Nachrichten

Es ist auch nützlich, sicherzustellen, dass das Argument `message` gültig ist. Wenn beispielsweise eine leere Zeichenfolge oder #f (false) als Nachricht übergeben wird, sollte die Funktion dies ordnungsgemäß verarbeiten.

Beispiel für den Umgang mit einer leeren Nachricht:

```scheme
(define (send-message message output)
  ;; Prüfen, ob die Nachricht leer ist
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
;; Funktion zur Ausgabe von Nachrichten an verschiedene Ziele
(define (send-message message output)

  ;; Nachrichten- und Ausgabeargumente validieren
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; An die Message Console senden
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; An das GUI-Dialogfeld senden
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; An das Terminalfenster senden
        ((eq? output 'terminal)
           (display message)))))

  ;; Standard-Nachrichtenhandler auf die Message console zurücksetzen
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