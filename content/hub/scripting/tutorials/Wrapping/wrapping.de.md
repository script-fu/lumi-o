---
title: "Verpackung"
type: docs
weight: 4
---
Schemabefehle arbeiten auf einer niedrigen Ebene, was bedeutet, dass selbst einfache Aufgaben mehrere Schritte erfordern können. Diese Granularität bietet jedoch Flexibilität, wir können Befehle in kleine, wiederverwendbare Funktionen bündeln, die genau das tun, was wir brauchen. Verpackung ist kein Schwarz-Weiß-Konzept; Es kann von einfachen Aliasen für häufig verwendete Befehle bis hin zu komplexeren Funktionen reichen, die ganze Arbeitsabläufe verwalten. Manchmal ist ein Wrapper nur eine praktische Funktion zur Verbesserung der Lesbarkeit, während er sich in anderen Fällen zu einem voll ausgestatteten Dienstprogramm entwickelt, das mehrere Vorgänge kapselt.

### Warum Wrap-Funktionen?

Das Umschließen von Funktionen bietet mehrere wesentliche Vorteile:

- **Vereinfacht sich wiederholende Aufgaben** – Anstatt Befehle auf niedriger Ebene zu wiederholen, packen Sie sie in eine Hilfsfunktion ein und verwenden Sie sie wieder.
- **Verbessert die Lesbarkeit** – Wenn wir unseren umschlossenen Funktionen klare, beschreibende Namen geben, ist unser Code auf einen Blick leichter verständlich.
- **Kapselt Komplexität** – Anstatt uns mit langen, kryptischen Befehlslisten, tief verschachtelten Schleifen oder komplexen Nachrichtenanweisungen herumzuschlagen, können wir sie in kleinere, gut strukturierte Hilfsfunktionen aufteilen.
- **Verbessert die Wartbarkeit** – Wenn sich die Kernfunktionalität eines Befehls ändert, müssen wir unsere umschlossene Funktion nur einmal aktualisieren und so unsere Plug-Ins von den Details dieser Änderungen isolieren.
- **Fördert die Wiederverwendung von Code** – Jeder Helfer wird Teil Ihrer Bibliothek, wodurch zukünftige Skripte schneller geschrieben und debuggt werden können.

Wenn Ihre Plug-Ins wachsen, helfen Ihnen Wrapper dabei, die Kernlogik lesbar zu halten und sich wiederholende Details zu isolieren.

Ein weiterer Vorteil von Wrapping-Funktionen besteht darin, sie in einen Syntax-Highlighter wie Visual Studio Code zu integrieren. Dies verbessert die Lesbarkeit und Navigation und macht Skripte klarer. In einem Plug-in, das benutzerdefinierte Funktionen verwendet, bestätigt jede grün hervorgehobene Funktion, dass sie korrekt aus unserer Bibliothek referenziert wird.

Wenn Sie Ihre eigene Hilfsbibliothek verwalten, sollten Sie erwägen, die Funktionsnamen Ihres Projekts zur Syntaxhervorhebung Ihres Editors hinzuzufügen. Es beschleunigt die Navigation und das Refactoring.

Beispiele:

### Zufälliger Samen

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Während wir ***msrg-rand*** direkt in unserem Code verwenden könnten, verbessert das Einschließen in eine Funktion namens ***random-seed*** die Lesbarkeit. Indem Sie der Funktion einen klaren und beschreibenden Namen geben, ist es einfacher, ihren Zweck auf einen Blick zu verstehen.

Darüber hinaus ermöglicht uns die Definition von ***random-seed*** als eigenständige Funktion, es überall in unseren Plug-ins zu verwenden und gleichzeitig die Implementierung an einem einzigen Ort zu zentralisieren. Wenn wir jemals ändern müssen, wie der Seed generiert wird, müssen wir nur diese Funktion aktualisieren und den Rest unseres Codes unberührt lassen.

Wenn wir uns beispielsweise dazu entschließen, stattdessen auf ***zufällig*** umzusteigen:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

Der Funktionsname bleibt derselbe und stellt sicher, dass unsere Skripte weiterhin ohne Änderungen funktionieren. Durch diesen Ansatz bleibt unser Code flexibel, wartbar und leicht lesbar.

### JPEG-Export

Die JPEG-Exportfunktion in Scheme verfügt über viele Parameter und bietet eine genaue Kontrolle darüber, wie Bilder gespeichert werden. In den meisten Fällen kümmern wir uns jedoch nur um einige wenige wichtige Einstellungen, wie z. B. Dateiname und Qualität. Um den Prozess zu vereinfachen, können wir die Funktion umschließen.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

In dieser Wrapper-Funktion sind die meisten Exportoptionen fest codiert und legen nur die Parameter offen, die wir wahrscheinlich anpassen werden: Dateiname und Qualität. Dieser Ansatz verbessert die Lesbarkeit und vereinfacht das Speichern von Bildern.Wenn sich der Exporter von Lumi in Zukunft ändert, müssen wir außerdem nur diese eine Funktion aktualisieren, anstatt jedes Skript zu ändern, das ein JPEG exportiert.

### Verwendung des Wrappers

Um ein JPEG in unsere Plug-Ins zu exportieren, binden wir einfach die Bibliothek ein und rufen unsere benutzerdefinierte Funktion auf:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Dadurch bleibt unser Code sauber, lesbar und anpassungsfähig und ermöglicht uns gleichzeitig den effizienten Export von JPEGs mit minimalem Aufwand.

### Autoersatz

Die ***car***-Funktion kann kryptisch und anfällig für Skriptfehler sein. Es ist leicht, ***car*** versehentlich auf einen Vektor oder ein Nicht-Listenelement anzuwenden, was zu unerwartetem Verhalten führt. Um unseren Code robuster und lesbarer zu machen, können wir diese Funktionalität in eine sicherere Funktion einbinden.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Diese Funktion ruft sicher das erste Element einer Liste oder eines Vektors ab und gibt hilfreiche Warnungen aus, wenn ungültige oder leere Eingaben auftreten. Durch die Verwendung von ***first-item*** anstelle von ***car*** reduzieren wir das Risiko versehentlicher Fehler und verbessern die Klarheit unserer Skripte.

#### Warum diesen Wrapper verwenden?

- **Verhindert Skriptabstürze** – Vermeidet Fehler, die durch die Anwendung von ***car*** auf Nicht-Listen verursacht werden.
- **Unterstützt sowohl Listen als auch Vektoren** – Erweitert die Benutzerfreundlichkeit über reine Listen hinaus.
- **Bietet aussagekräftige Warnungen** – Hilft bei der Fehlerbehebung unerwarteter Eingabeprobleme.
- **Verbessert die Lesbarkeit** – Der Funktionsname vermittelt klar seinen Zweck.

Indem wir diese Logik im ersten Element kapseln, machen wir unsere Plug-Ins robuster und einfacher zu warten. Dies hängt natürlich von Ihren persönlichen Vorlieben ab. Möglicherweise können Sie die Funktionen car, caar, cadr und ähnliche Scheme-Funktionen direkt verwenden.

### Eine umschlossene Funktion umschließen

Das Umschließen einer bereits umschlossenen Funktion kann die Lesbarkeit und Wartbarkeit weiter verbessern. Wenn wir beispielsweise mit Koordinatenpaaren wie ***pixel-coords (list 100 200)*** arbeiten, könnten wir Folgendes verwenden:

```scheme
(first-item pixel-coords)
```

um die ***x***-Koordinate abzurufen. Dies ist zwar funktional, aber nicht sehr ausdrucksstark. Stattdessen können wir ***first-item*** in eine passendere Definition einschließen, um unsere Absicht klarer zu machen.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Warum diesen Ansatz verwenden?

- **Verbessert die Klarheit des Codes** – Anstatt generische Listenzugriffsfunktionen zu verwenden, definieren wir explizit Funktionen, die ihren Zweck beschreiben.
- **Verbessert die Wartbarkeit** – Wenn sich unsere Koordinatendarstellung ändert (z. B. Verwendung von Vektoren anstelle von Listen), müssen wir nur diese kleinen Funktionen aktualisieren.
- **Fördert die Konsistenz** – Durch die Verwendung von ***x-Koordinaten*** und ***y-Koordinaten*** ist das Skript auf einen Blick einfacher zu lesen und zu verstehen.

Anstatt nun im generischen Schema zu schreiben:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Wir können in _unser_ Schema schreiben:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Indem wir Low-Level-Funktionen in aussagekräftige Namen verpacken, schaffen wir eine intuitivere Möglichkeit, mit Daten zu arbeiten und reduzieren so Verwirrung und potenzielle Fehler.

### Mitgelieferte Wrapper: das Dienstprogramm Stdlib

Lumi liefert einen Satz vorgefertigter Wrapper aus, die beim Start automatisch geladen werden, sodass sie in jedem Plug-in oder in der Scheme-Konsole ohne `(load ...)` Aufruf verfügbar sind. Diese Bibliotheken (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` und `paths.scm`) basieren auf genau dem gleichen Prinzip wie die obigen Beispiele: Sie geben Operationen auf niedriger Ebene klare Namen, Blenden Sie sich wiederholende Boilerplates aus und stellen Sie einen einzigen Ort zum Aktualisieren bereit, wenn sich der zugrunde liegende Befehl ändert.Beispielsweise stellt `images.scm` `image-get-open-list` als lesbaren Wrapper um den rohen PDB-Aufruf bereit, und `files.scm` stellt pfadbildende Helfer bereit, die andernfalls wiederholte `string-append` Ketten erfordern würden.

Sie können jeden exportierten Namen durchsuchen, seine Dokumentzeichenfolge lesen und sehen, aus welcher Bibliothek er stammt, im **[Utility Browser](@@LUMI_TOKEN_21@@)** (Hilfe → Programmierung → Dienstprogramm-Browser). Es ist eine praktische Demonstration des Umwickelns im großen Maßstab und eine nützliche Quelle für Muster, die Sie beim Aufbau Ihrer eigenen Hilfsbibliothek ausleihen können.

### Fazit

Das Umschließen von Funktionen ist eine leistungsstarke Möglichkeit, die Schemaentwicklung zu vereinfachen und Skripte lesbarer, wartbarer und robuster zu machen. Indem wir die Komplexität einkapseln und nur die notwendigen Details offenlegen, schaffen wir einen strukturierteren Ansatz zum Schreiben von Plug-Ins.

Wichtigste Erkenntnisse aus diesem Ansatz:

- **Vereinfacht sich wiederholende Aufgaben** – Anstatt Befehle auf niedriger Ebene manuell zu wiederholen, erstellen wir wiederverwendbare Funktionen.
- **Verbessert die Lesbarkeit des Codes** – Gut benannte Wrapper erleichtern das Verständnis von Skripten.
- **Kapselt Komplexität** – Details auf niedriger Ebene werden innerhalb des Wrappers verarbeitet, wodurch das Hauptskript sauber bleibt.
- **Verbessert die Wartbarkeit** – Wenn sich die Kernfunktionalität ändert, müssen wir nur den Wrapper aktualisieren, nicht jedes Skript, das darauf basiert.
- **Fördert Wiederverwendung und Konsistenz** – Unsere persönliche Funktionsbibliothek wächst mit der Zeit und macht die Entwicklung schneller und effizienter.

Durch die konsequente Verwendung von Function Wrapping können wir die Art und Weise, wie wir Scheme-Plug-Ins schreiben, verändern und so eine modularere und ausdrucksstärkere Skriptumgebung schaffen. Unter Berücksichtigung dieser Grundsätze können wir unseren Ansatz weiter verfeinern und eine effizientere und maßgeschneiderte Version des Programms entwickeln, die unseren spezifischen Anforderungen entspricht.

Nächste Schritte: Identifizieren Sie sich wiederholende Blöcke in Ihren Skripten und extrahieren Sie kleine Helfer mit klaren Namen.