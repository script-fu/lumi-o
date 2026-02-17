---
title: "Das Filter-Plugin"
type: docs
weight: 2
---
Für das [First Step](../../first-step/) Tutorial haben wir ein _procedure_ Plug-in verwendet. Diese Arten von Plug-Ins funktionieren, ohne dass ein Bild oder eine Zeichendatei als Eingabe erforderlich ist. Normalerweise verwenden wir ein Plug-In, um ein Bild und seine Zeichenelemente zu ändern. Plug-ins wie diese werden als _Filter_-Plug-ins bezeichnet.

### Was ist ein Drawable?

Ein **Zeichenbares** in Lumi bezieht sich auf ein Bildelement, auf das gezeichnet werden kann, beispielsweise eine Ebene oder ein Kanal. Filter-Plug-Ins arbeiten normalerweise mit diesen Elementen.

### Ein einfaches Beispiel für ein Filter-Plug-in

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Kopieren Sie den Text und speichern Sie ihn als `simple-filter-plug-in.scm` in einem Ordner namens `simple-filter-plug-in` in einem der Plug-In-Ordner von Lumi. Ein Lumi-Plug-In-Ordner ist _jeder_ Ordner, der unten aufgeführt ist:
 **Lumi > Bearbeiten > Einstellungen > Ordner > Plug-ins**

Klicken Sie unter Linux mit der rechten Maustaste auf die Datei `simple-filter-plug-in.scm`, gehen Sie zu **Eigenschaften > Berechtigungen** und aktivieren Sie **Ausführung der Datei als Programm zulassen**. Sobald sich die Datei am richtigen Ort befindet, ausführbar und frei von Syntaxfehlern ist, wird sie beim Neustart von Lumi in der oberen Menükopfleiste in einem Menü namens **Plug-in** angezeigt.

### Ausführen des Plug-Ins

1. Öffnen Sie ein Bild (für dieses Filter-Plug-in ist ein Bild erforderlich).
2. Öffnen Sie **Windows > Andockbare Dialoge > Fehlerkonsole**, um eine Meldung anzuzeigen.
3. Wählen Sie **Simple Filter Plug-in Demo** aus dem **Plug-in**-Menü.
4. Die Farben einer der ausgewählten Ebenen werden invertiert und eine Meldung wird an die Fehlerkonsole ausgegeben.

### Bearbeiten des Plug-Ins

Sie können das Plug-in anpassen, indem Sie seine Datei `.scm` bearbeiten. Um beispielsweise die angezeigte Meldung zu ändern:

1. Öffnen Sie die Datei und suchen Sie die Zeile, die `message` definiert.
2. Ersetzen Sie `"hello, world"` durch Ihren benutzerdefinierten Text.
3. Speichern Sie die Datei.

In Lumi Version 3 müssen Plug-Ins nicht aktualisiert werden, damit gespeicherte Änderungen wirksam werden. Führen Sie einfach das Plug-in erneut aus, um die aktualisierte Nachricht anzuzeigen.

### Plug-in-Prüfung

#### Shebang-Linie

Die erste Zeile stellt sicher, dass das Skript als Plug-In in Lumi 3 funktioniert:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Prozedurdefinition

Die Prozedur akzeptiert zwei Argumente: das aktive Bild und die ausgewählten Zeichenelemente.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Kernlogik

Eine `let`-Anweisung definiert eine Variable und führt Operationen für das Zeichenobjekt aus.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Plug-in-Registrierung

Das Plug-in ist bei Lumi als Filter-Plug-in registriert:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Menüregistrierung
Diese Zeile gibt die Menüposition für das Plug-in an:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Fehlerbehebung

Wenn ein Plug-in nicht angezeigt wird, überprüfen Sie seinen Speicherort, seinen Namen und seine ausführbare Eigenschaft.

Der Speicherort muss sich in einem Plug-in-Suchpfad befinden.
Der Dateiname muss mit dem Namen des enthaltenen Ordners übereinstimmen.
Die Datei muss als ausführbar festgelegt sein.


Die **Fehlerkonsole** ist ein wertvolles Tool zur Fehlerbehebung bei benutzerdefinierten Plug-Ins. Wenn sich Ihr Plug-in nicht wie erwartet verhält, suchen Sie hier nach Fehlermeldungen oder Protokollen. Das **Terminal**-Fenster kann auch Debugging-Informationen bereitstellen und Ladeprobleme melden.