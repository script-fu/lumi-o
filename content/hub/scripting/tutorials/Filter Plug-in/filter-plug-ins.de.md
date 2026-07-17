---
title: "Das Filter-Plugin"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e8eb69ed9dff7c65cc926ba4bfb4c333fdd8baa3832aa92765ba6bb19b17516d
---
Für das Tutorial [Erster Schritt](../../first-step/) haben wir ein _procedure_-Plug-in verwendet. Diese Arten von Plug-Ins funktionieren, ohne dass ein Bild oder eine Zeichendatei als Eingabe erforderlich ist. Normalerweise verwenden wir ein Plug-In, um ein Bild und seine Zeichenelemente zu ändern. Plug-ins wie diese werden als _Filter_-Plug-ins bezeichnet.

### Was ist ein Drawable?

Ein **Zeichenbares** in Lumi bezieht sich auf ein Bildelement, auf das gezeichnet werden kann, beispielsweise eine Ebene oder ein Kanal. Filter-Plug-Ins arbeiten normalerweise mit diesen Elementen.

### Ein einfaches Beispiel für ein Filter-Plug-in

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; let-Anweisung verwenden, um eine Nachrichtenvariable und den Kern-Code zu definieren
  (let ((message "hello, world"))
    ;; Nachricht in Lumis Error Console anzeigen
    (lumi-message message)
    ;; Farben des ersten ausgewählten Drawables invertieren
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Registriert das Plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Name der Hauptprozedur
  "Simple Filter Plug-in Demo"             ;; Der Name, wie er im Lumi-Menü erscheint
  "Tests a basic Scheme filter plug-in"    ;; QuickInfo-Beschreibung
  "Author Name"                            ;; Gib dir selbst etwas Anerkennung
  "License"                                ;; Lizenz
  "Date written"                           ;; Schreibdatum
  "*"                                      ;; Gibt an, dass dieses Plug-in ein Bild benötigt
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Erfordert ein oder mehrere ausgewählte Drawables

;; Menüposition für das Plug-in angeben
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Kopieren Sie den Text und speichern Sie ihn als `simple-filter-plug-in.scm` in einem Ordner namens `simple-filter-plug-in` in einem der Plug-In-Ordner von Lumi. Ein Lumi-Plug-In-Ordner ist _jeder_ Ordner, der unten aufgeführt ist:
 **Lumi > Bearbeiten > Einstellungen > Ordner > Plug-ins**

Klicken Sie unter Linux mit der rechten Maustaste auf die Datei `simple-filter-plug-in.scm`, gehen Sie zu **Eigenschaften > Berechtigungen** und aktivieren Sie **Ausführung der Datei als Programm zulassen**. Sobald sich die Datei am richtigen Ort befindet, ausführbar und frei von Syntaxfehlern ist, wird sie beim Neustart von Lumi in der oberen Menükopfleiste in einem Menü namens **Plug-in** angezeigt.

### Ausführen des Plug-Ins

1. Öffnen Sie ein Bild (für dieses Filter-Plug-in ist ein Bild erforderlich).
2. Öffnen Sie **Tools > Debug > Nachrichtenkonsole**, um eine Nachricht anzuzeigen.
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
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
  (lumi-message message) ;; Zeigt eine Nachricht in Lumis Error Console an
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Invertiert die Farben des ersten ausgewählten Drawables
```

### Plug-in-Registrierung

Das Plug-in ist bei Lumi als Filter-Plug-in registriert:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Registriert die Hauptprozedur
  "Simple Filter Plug-in Demo"             ;; Der Name, wie er im Lumi-Menü erscheint
  "Tests a basic Scheme filter plug-in"    ;; QuickInfo-Beschreibung
  "Author Name"                            ;; Name des Autors
  "License"                                ;; Lizenztyp
  "Date written"                           ;; Schreibdatum
  "*"                                      ;; Gibt an, dass das Plug-in ein Bild benötigt
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Erfordert ein oder mehrere ausgewählte Drawables
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


Die **Nachrichtenkonsole** ist ein wertvolles Tool zur Fehlerbehebung bei benutzerdefinierten Plug-Ins. Wenn sich Ihr Plug-in nicht wie erwartet verhält, suchen Sie hier nach Fehlermeldungen oder Protokollen. Das **Terminal**-Fenster kann auch Debugging-Informationen bereitstellen und Ladeprobleme melden.