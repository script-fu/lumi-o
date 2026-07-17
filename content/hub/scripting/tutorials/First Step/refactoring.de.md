---
title: "Refactoring"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bc83f55511f34e6f099f8de8c6af3bba5e459974aa4bff6265ae70d679517646
---
Sobald eine Funktion funktioniert, können wir einen Schritt zurücktreten und darüber nachdenken, wie wir unseren Code am besten strukturieren. Ziel ist es, unser Plug-in so klar, verständlich und wartbar wie möglich zu gestalten. Dieser Prozess der Verbesserung und Verfeinerung der Struktur des vorhandenen Codes ohne Änderung seines Verhaltens wird als Refactoring bezeichnet.

Hier noch einmal die Ausgangsfunktion:

```scheme
(define (scheme-hello-world)
  ;; Nachrichtenhandler so setzen, dass die Nachricht in ein GUI-Dialogfeld ausgegeben wird
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Nachrichtenhandler so setzen, dass die Nachricht an die Error Console ausgegeben wird
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Nachricht an das terminal senden, das OS-Fenster, das Lumi gestartet hat
  (display "Hello world!\n"))
```

Der Funktionsname ist der Name der Funktion und der Parameter ist das, was die Funktion als Eingabe akzeptiert. Der Körper ist der Codeblock, der ausgeführt wird, wenn die Funktion aufgerufen wird.

Abstrakte Form:

```scheme
(define (function-name parameter)
  body)
```

### Codewiederholung

Entfernen Sie Wiederholungen frühzeitig. `(lumi-message "Hello world!\n")` wird zweimal wiederholt, und die Nachrichtenzeichenfolge wird dreimal wiederholt. Eine Variable löst die wiederholte Zeichenfolge.

### Variablen

In Scheme hat eine Variable einen „Bereich“, über den sie bekannt ist, und dieser Bereich wird mithilfe einer `let`-Anweisung festgelegt. Die Variable ist im Bindungsteil an einen Wert gebunden und hat im Let-Körper einen Gültigkeitsbereich. Die Variable ist nur innerhalb des let-Blocks bekannt und kann außerhalb nicht aufgerufen werden.

```scheme
(let ((variable value))
  body)
```

Einführung einer Variablen namens „message“:

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Nachrichtenhandler so setzen, dass die Nachricht in ein GUI-Dialogfeld ausgegeben wird
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Nachrichtenhandler so setzen, dass die Nachricht an die Error Console ausgegeben wird
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Nachricht an das terminal senden, das OS-Fenster, das Lumi gestartet hat
    (display message)))
```

In unserem Beispiel haben wir eine Variable namens „message“ verwendet, die an eine Zeichenfolge „Hello world!\n“ gebunden ist. Dadurch können wir den Nachrichteninhalt einmal statt dreimal ändern, was die Fehlerwahrscheinlichkeit verringert und den Code flexibler macht.

### Funktionen extrahieren

In der funktionalen Programmierung ist die Umgestaltung von Code zum Extrahieren wiederverwendbarer Logik in separate Funktionen eine gängige Praxis. Dadurch wird die **Hauptfunktion** viel einfacher und fokussierter auf ihr übergeordnetes Ziel, während die **extrahierte Funktion** komplexer erscheint, da sie die detaillierte Logik verarbeitet. Dies ist beabsichtigt und steht im Einklang mit den Grundprinzipien der funktionalen Programmierung, wie Modularität, Trennung von Belangen und Lesbarkeit. Hier ist die Umgestaltung
Hallo Welt! nach der Extraktion.

Extrahieren der Logik:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; Hauptfunktion
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Funktion zur Ausgabe von Nachrichten an verschiedene Ziele
(define (send-message message output)
  (cond
    ;; An die Error Console senden
    ((eq? output 'error-console)
       ;; Handler auf Error Console setzen
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

  ;; Standard-Nachrichtenhandler auf die Error Console zurücksetzen
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Symbole

Im obigen Beispiel wird ein Datentyp namens Symbol verwendet, z. B. „gui“. Symbole werden als Parameter an die Funktion „Nachricht senden“ übergeben und können zum Treffen einfacher bedingter Entscheidungen verwendet werden. Wie symbolische Schlüssel sind sie eindeutige Bezeichner. Weitere Informationen zu Symbolen finden Sie auf [dieser Seite.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Vereinfachung der Hauptfunktion

In der ursprünglichen Funktion (scheme-hello-world) wurde die gesamte Logik zum Senden von Nachrichten an verschiedene Ausgänge (GUI, Fehlerkonsole, Terminal) in die Hauptfunktion integriert. Nach dem Refactoring konzentriert sich die Hauptfunktion einfach auf **was getan werden muss** und sendet die Nachricht an verschiedene Ziele.

Die überarbeitete Hauptfunktion ist einfacher:

- Der Zweck wird klar dargelegt: Dieselbe Nachricht an mehrere Ausgänge senden.
– Es wird vermieden, dass die Hauptlogik mit sich wiederholendem Code überladen wird, z. B. durch das Festlegen von Nachrichtenhandlern für verschiedene Ausgaben.
- Auf einen Blick ist es einfacher zu lesen und zu verstehen.

### Die Komplexität der extrahierten Funktion

Im Gegensatz dazu befindet sich in der Funktion **(Nachricht senden)** die detaillierte Logik. Es verarbeitet jetzt die Verhaltensunterschiede für jede Ausgabe (GUI, Fehlerkonsole, Terminal). Die Funktion ist etwas komplexer als zuvor, aber jetzt **zentralisiert** und **isoliert**.

## Dies hängt mit der funktionalen Programmierung zusammen

In der funktionalen Programmierung werden Funktionen als „erstklassige Bürger“ betrachtet, was bedeutet, dass sie wiederverwendet, weitergegeben und zu komplexerem Verhalten kombiniert werden können. Das Ziel ist:- **Probleme zerlegen** in kleinere, unabhängige Teile.
- **Isolieren Sie die Komplexität** in kleinere Funktionen, die bestimmte Aufgaben erledigen, wie `send-message`.
- **Halten Sie Funktionen auf höherer Ebene einfach**, damit sie sich auf die Orchestrierung des Daten- und Aktionsflusses konzentrieren können, ohne die Details darüber kennen zu müssen, wie die einzelnen Aufgaben ausgeführt werden.
- **Trennung von Belangen**: Die Funktion kümmert sich darum, wie die Nachricht basierend auf dem Ausgabetyp gesendet wird, wodurch diese Logik von der Hauptfunktion isoliert wird.
- **Modularität**: Durch die Verwaltung der gesamten Logik zum Senden von Nachrichten an einem Ort können wir problemlos Änderungen vornehmen (z. B. das Hinzufügen neuer Ausgabeoptionen), ohne die Hauptfunktion zu ändern.
- **Wiederverwendbarkeit**: Die Funktion `send-message` ist wiederverwendbar. Wenn wir also eine Nachricht an mehrere Ausgänge an anderer Stelle in unserem Code senden müssen, können wir diese Funktion einfach aufrufen, anstatt eine ähnliche Logik neu zu schreiben.

Durch Refactoring wird die Hauptfunktion in diesem Beispiel zu einer **deklarativen** Aussage darüber, was geschieht („eine Nachricht an drei Orte senden“), während die Komplexität, wie diese Nachrichten gesendet werden, in der Funktion `send-message` abstrahiert wird.