---
title: "Debuggen"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
---
Beim Scripting gibt es keine Funktion, die unfehlbar ist. Selbst die zuverlässigsten Befehle können fehlschlagen, wenn unerwartete Eingaben oder Bedingungen auftreten. Um uns dagegen zu schützen, können wir ein benutzerdefiniertes Debugging-System implementieren und defensive Programmiertechniken übernehmen. Indem wir Standardfunktionen mit Fehlerbehandlungsmechanismen umschließen und informatives Feedback bereitstellen, können wir unsere Skripte robuster und einfacher zu beheben machen.

Ein wichtiger Teil dieser Strategie ist die Verwendung eines globalen Debug-Flags zur Steuerung der ausführlichen Ausgabe, sodass wir bei Bedarf detaillierte Debugging-Informationen aktivieren und gleichzeitig die Ausgabe während der normalen Ausführung sauber halten können.

## Globales Debug-Flag

Ein globales Debug-Flag ist eine einfache, aber effektive Möglichkeit, den Grad der Informationsausgabe während der Skriptausführung zu steuern. Wenn es aktiviert ist, stellt es detaillierte Debugging-Meldungen bereit, die für das Aufspüren von Problemen von unschätzbarem Wert sein können. Wenn es deaktiviert ist, bleibt die Ausgabe für den Produktionsgebrauch prägnant.

```scheme
;; Zweck: Globales Flag zur Steuerung der Debug-Ausgabe.
(define debug #f)
```

Standardmäßig ist das Debuggen deaktiviert. Um eine ausführliche Ausgabe während der Entwicklung zu ermöglichen, setzen Sie einfach das Flag auf `#t`:

```scheme
;; Zweck: Globales Flag zur Steuerung der Debug-Ausgabe.
(define debug #t)
```

Mithilfe von Hilfsfunktionen können wir das Debuggen für bestimmte Codeabschnitte auch vorübergehend aktivieren oder deaktivieren.

### Lokale Debug-Steuerung

Für eine genauere Kontrolle können wir das Debuggen mithilfe von Hilfsfunktionen in bestimmten Teilen des Skripts ein- oder ausschalten.

```scheme
;; Zweck: Debug-Modus für einen Codeabschnitt deaktivieren.
(define (debug-off)
  (set! debug #f))

;; Zweck: Debug-Modus für einen Codeabschnitt aktivieren.
(define (debug-on)
  (set! debug #t))
```

Dadurch können wir das Debuggen dynamisch steuern:

```scheme
(debug-on)  ;; Ausführliche Ausgabe aktivieren

;; Hier etwas Skriptlogik

(debug-off) ;; Ausführliche Ausgabe deaktivieren
```

## Messaging-System debuggen

Um die Debug-Ausgabe in Scheme effizient zu verarbeiten, verwenden wir einen strukturierten Ansatz mit mehreren Hilfsfunktionen. Diese Funktionen stellen sicher, dass Debug- und Warnmeldungen klar, lesbar und wartbar sind.

### Übersicht über das Debug-Messaging-System

Unser Debug-Messaging-System besteht aus folgenden Komponenten:

1. `debug-message` – Zeigt Debug-Meldungen an, wenn das Debuggen aktiviert ist.
2. `serialize-item` – Konvertiert verschiedene Scheme-Datentypen in eine String-Darstellung.
3. `concat` – Verkettet mehrere Elemente zu einer einzigen Zeichenfolge.
4. `list->string` – Formatiert eine Liste in eine lesbare Zeichenfolge.
5. `message` – Zeigt die Ausgabe in der Nachrichtenkonsole von Lumi an.
6. `warning-message` – Zeigt Warnmeldungen an, wenn Warnungen aktiviert sind.

Jede Funktion spielt eine Rolle bei der Formatierung und Anzeige strukturierter Nachrichten.

---

### Debug-Nachrichtenfunktion

Die Funktion `debug-message` ist die Kernmethode zum Anzeigen der Debug-Ausgabe. Dadurch wird sichergestellt, dass Meldungen nur angezeigt werden, wenn das Debuggen aktiviert ist.

```scheme
;; Zweck: Eine Debug-Meldung anzeigen.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

– Die Bedingung `when debug` stellt sicher, dass Meldungen nur angezeigt werden, wenn das Debuggen aktiviert ist.
- Zur besseren Übersichtlichkeit wird den Nachrichten das Präfix `"> "` vorangestellt.
– Die Funktion verwendet `concat`, um den Nachrichteninhalt zu formatieren.
– Schließlich wird `message` aufgerufen, um die Ausgabe an Lumis Nachrichtenkonsole zu senden.

Beispielverwendung:

```scheme
;; Zweck: Gibt die Baumposition des Elements zurück oder #f, wenn das Element ungültig ist
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Bei aktiviertem Debugging könnte die Ausgabe wie folgt aussehen:

```scheme
> item: background-layer has tree position : 3
```

### Serialisieren von Daten für Debug-Nachrichten

Nachrichten können verschiedene Datentypen wie Listen, Vektoren und Zahlen enthalten. Um sicherzustellen, dass sie richtig formatiert sind, verwenden wir `serialize-item`.

```scheme
;; Zweck: Konvertiert verschiedene Scheme-Datentypen (Listen, Vektoren, Paare usw.)
;;          in eine Zeichenketten-Darstellung um.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Leere Liste
    ((and (string? item) (string=? item "")) "\"\"")  ; Leere Zeichenkette
    ((list? item) (list->string item))                ; Verschachtelte Liste
    ((vector? item)                                   ; Behandelt Vektoren
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Behandelt Paare
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Zahlen
    ((symbol? item) (symbol->string item))            ; Symbole
    ((boolean? item) (if item "#t" "#f"))             ; Boolesche Werte
    ((string? item) item)                             ; Zeichenketten
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Beispielverwendung:

```scheme
(serialize-item '(1 2 3))
```

Ausgabe:

```scheme
list:
1
2
3
```

### Verkettung für Nachrichten

Um mehrere Nachrichtenkomponenten in einer einzigen Zeichenfolge zusammenzuführen, verwenden wir `concat`.

```scheme
;; Zweck: Mehrere Elemente zu einer einzigen Zeichenkette verketten.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Beispielverwendung:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Listen als Strings formatieren

Die Funktion `list->string` konvertiert eine Liste in eine formatierte Zeichenfolge.

```scheme
;; Zweck: Eine Liste von Elementen in eine lesbare Zeichenkette umwandeln.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### WarnmeldungenDie Funktion `warning-message` funktioniert ähnlich wie `debug-message`, zeigt jedoch Warnungen an, auch wenn das Debuggen deaktiviert ist.

```scheme
;; Zweck: Eine Warnmeldung anzeigen.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

– Stellt sicher, dass Meldungen nur angezeigt werden, wenn Warnungen aktiviert sind (das Flag `warning` ist in `common.scm` auf `#t` gesetzt).
– Ruft `concat` auf, um den Nachrichteninhalt zu formatieren.
- Verwendet `message`, um die Ausgabe an Lumi zu senden.

## Standardfunktionen erweitern

Sobald ein Debugging-System vorhanden ist, können wir unsere Funktionsbibliothek durch die Einbindung detaillierter Meldungen erweitern. Dies bietet Einblick in Elementzustände, Variablenwerte und Funktionsaufrufe.

Ein häufiges Beispiel ist `item-is-valid?`, das `lumi-item-id-is-valid` umschließt, um `#t` oder `#f` zurückzugeben. Wenn `#f` zurückgegeben wird, können wir im aufrufenden Code ein `warning-message` auslösen. Wenn die Eingabe keine Zahl ist, können wir in der Funktion eine Warnung ausgeben.

```scheme
;; Zweck: Prüfen, ob ein Element gültig ist, gibt #t oder #f zurück.
;;          Gibt eine Warnung aus, wenn das Element keine Zahl ist.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Praktische Anwendung

Bei der Entwicklung von Scheme-Plug-Ins reduziert das Umschließen von Funktionen auf diese Weise die Debugging-Zeit erheblich und sorgt für robusten, wartbaren Code. Mit unserem Debugging-System können wir auf Knopfdruck einen strukturierten Debug-Stream in der Fehlerkonsole generieren.

In diesem Debug-Stream sind Funktionsaufrufe mit einem Sternchen (*) gekennzeichnet, was es einfacher macht, die Skriptausführung zu verfolgen und Fehler zu lokalisieren, insbesondere bei komplexen Plug-Ins. Diese Transparenz hilft uns, den Betriebsablauf zu verstehen und unerwartetes Verhalten effizient zu diagnostizieren.

Ein Wrapper für unsere Nachrichtenfunktion zur Verwendung eines `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Beispiel für die praktische Verwendung von `call`:

```scheme
;; Zweck: Wendet den Texturierungsprozess auf die gegebene Liste von Gruppenmasken an
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Beispiel eines Debug-Streams, wenn ein Plug-in ausgeführt wird:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Dieses strukturierte Protokoll bietet eine klare Zeitleiste von Funktionsaufrufen und Datenänderungen und erleichtert so das Debuggen und die Leistungsanalyse erheblich.

## Fazit

Durch die Implementierung eines strukturierten Debugging-Systems erstellen wir sicherere und besser wartbare Skripte, die Echtzeiteinblicke in ihre Ausführung bieten.

### Wichtige Erkenntnisse

- **Ausführlichkeit steuern** – Verwenden Sie ein globales Debug-Flag, um Ausgabeebenen zu verwalten.
- **Geben Sie klares Feedback** – Verpacken Sie Standardfunktionen mit informativen Debug-Meldungen.
- **Verbesserung der Robustheit** – Behandeln Sie unerwartete Eingaben elegant, um Fehler zu vermeiden.
- **Vereinfachen Sie die Fehlerbehebung** – Strukturierte Debug-Meldungen erleichtern die Diagnose und Behebung von Problemen.

Mit diesem Ansatz „erklären“ sich unsere Skripte effektiv selbst, während sie Daten verarbeiten, wodurch Frustrationen reduziert und die Effizienz des Arbeitsablaufs verbessert werden. Das Debuggen wird zu einem proaktiven Werkzeug und nicht zu einer reaktiven Aufgabe, wodurch unser Skripterstellungsprozess reibungsloser und lohnender wird.