---
title: "Pinselwerkzeug"
type: docs
---
Der Pinsel ist das wichtigste Malwerkzeug, das für eine reaktionsschnelle, intelligente Pinselführung mit vollständiger Kontrolle über Druck, Geschwindigkeit, Neigung und Abstandsdynamik entwickelt wurde.

## Übersicht

Das Pinselwerkzeug unterstützt Raster-, prozedural generierte und animierte Pinseltypen. Striche können stabilisiert, geglättet und nachbearbeitet werden. Die Pinseldynamik reagiert auf die Stifteingabe und ermöglicht eine präzise Kontrolle über Deckkraft, Größe, Farbe, Winkel und andere Eigenschaften während eines Pinselstrichs.

## Pinseltypen

### Rasterpinsel
Bitmap-Pinselbilder, die aus `.png`- oder `.vbr`-Dateien geladen wurden. Unterstützt Alpha-Transparenz und animierte Rohrrahmen.

### Generierte Pinsel
Prozedural gerenderte Formen (Kreis, Quadrat, Raute, Dreieck) mit einstellbaren Parametern: Härte, Seitenverhältnis, Winkel, Rundheit und Eckenradius. Generierte Pinsel sind leichtgewichtig und skalierbar.

### Animierte Pinsel (.anim)
Sequentielle Bildsequenzen, die während der Striche fortschreiten. Frames können inkrementell durchlaufen werden (Bildvorlauf pro Tupfer), zufällig pro Tupfer ausgewählt oder nach Dynamik (Druck, Geschwindigkeit, Neigung, Winkel) indiziert werden.

## Malcursor
Der Cursor passt sich dem aktuellen Werkzeugstatus an, um klares, kontextbezogenes Feedback zu geben:

- **Pinselumriss**: Der Cursor verfolgt die genaue Pinselform und -größe und bietet eine Live-Vorschau, wo die Farbe landen wird.
- **Löschmodus**: Wenn das Radieren aktiv ist, ändert sich der Umriss in einen gestrichelten Kreis, um Radierstriche optisch von Farbstrichen zu unterscheiden.
- **Einfache Pinselgrenze**: Für komplexe oder sehr große Pinsel, bei denen das Rendern der genauen Kontur kostspielig ist, aktivieren Sie **Einfache Pinselgrenze** (unter „Zusätzliche Optionen“), um stattdessen einen einfachen Kreis zu verwenden.

## Werkzeugoptionen

### Steuerelemente der obersten Ebene
Immer präsent, außerhalb jedes Expanders:
- **Modus**: Farbmischmodus (Normal, Multiplizieren, Bildschirm usw.)
- **Deckkraft**: Gesamtdeckkraft des Strichs (0–100).

### Pinseloptionen
Im Expander **Pinseloptionen** (standardmäßig erweitert):
- **Größe**: Pinseldurchmesser in Pixel.
- **Verhältnis**: Die Pinselform stauchen oder strecken (-1,0–1,0). 0 = unverändert; Negative Werte drehen den Kürbis um 90°.
- **Winkel**: Dreht den Pinselstempel (-180–180°). Unabhängig von der Schlagrichtungsdynamik.
- **Abstand**: Abstand zwischen gemalten Tupfen als Prozentsatz der Pinselgröße. Niedriger = sanftere Striche; höher = Streumuster.
- **Härte**: Sanfter Übergang (0,0) bis scharfe Kante (1,0).
- **Kraft**: Pinselanwendungskraft (0,0–1,0). Für das Stiftwerkzeug ausgeblendet.
- **Jitter**: Versetzt jede Spitzenposition nach dem Zufallsprinzip um bis zu so viele Pixel (0–1024).
- **Radiergummi**: Größenmultiplikator, der angewendet wird, wenn dieser Pinsel als Radiergummi verwendet wird (0,1–10,0). Wird im Radiergummi selbst nicht angezeigt.

### Stricheffekte
Im Expander **Stricheffekte**:
- **Nachbearbeitung**: Wendet Stabilisierung, Geschwindigkeitskomprimierung und Wiederholungskorrektur an, nachdem der Schlag abgeschlossen ist, und verbessert so die Konsistenz ohne Latenz.
  - **Drehschwellenwert**: Winkelschwellenwert (0–180°) zur Richtungskorrektur an scharfen Ecken. 0 = Sprungrichtung fix.
  - **Vorschaugeschwindigkeit**: Unterdrückt die Nachbearbeitungsvorschau, wenn die Strichgeschwindigkeit diesen Wert überschreitet (0 = immer Vorschau).
- **Aufbau**: Wenn diese Option aktiviert ist, erhöht sich die Deckkraft jedes Tupfers, anstatt dass er als einzelner Strich zusammengesetzt wird.#### Kalligraphisch
Wenn aktiv, wird das Stempeln von Spitzen durch einen kontinuierlichen geometrischen Korridor ersetzt:
- **Breite** und **Höhe**: Abmessungen des kalligrafischen Korridors.
- **Winkel**: Ausrichtung der Spitze (Grad).
- **Dynamische Deckkraft**: Moduliert die Deckkraft innerhalb des Strichs basierend auf Geschwindigkeits- und Richtungsänderungen. Funktioniert am besten bei feinen, kontrollierten Strichen; Bei schnellem Gekritzel sind die Ergebnisse weniger vorhersehbar. Experimental.
- **Geschwindigkeitswachstum** (0–100 %): Maximal zulässige Größenzunahme pro Probe als Prozentsatz der Größe der vorherigen Probe. Begrenzt, wie schnell eine geschwindigkeitsgesteuerte Größendynamik wachsen kann, und verhindert so plötzliche Sprünge, wenn der Schlag beschleunigt wird.
- **Velocity Shrink** (0–100 %): Maximal zulässige Größenverringerung pro Probe. Begrenzt, wie schnell die Größe sinken kann, wenn der Hub verlangsamt wird.

#### Glättung
Ermöglicht die Eingabeglättung in Echtzeit, die beim Malen auf den Strichpfad angewendet wird. Wird erweitert und zeigt Folgendes:
  - **Tiefe** (2–256): Anzahl der vorherigen Eingabeproben, die bei der Berechnung der geglätteten Position berücksichtigt werden. Höhere Werte führen zu einer längeren, engagierteren Verzögerung.
  - **Position** (0–100): Intensität der auf die Pinselposition angewendeten Glättung. Höhere Werte runden scharfe Richtungsänderungen ab.
  - **Druck** (0–100): Glättung des Stiftdrucksignals, wodurch Druckspitzen und Jitter reduziert werden.
  - **Richtung** (0–100): Glättung der Strichrichtung, Stabilisierung der winkelempfindlichen Dynamik.

#### Dynamik
Weisen Sie Malparametern Stifteingaben oder andere Live-Werte zu:

- **Druck** (Stift): Steuert Größe, Deckkraft, Rate, Härte, Farbe und mehr basierend auf dem Stiftdruck.
- **Geschwindigkeit**: Ordnet die Strichgeschwindigkeit den Pinseleigenschaften zu.
- **Neigung**: X- und Y-Neigungswinkel des Stifts wirken sich auf den Winkel und andere Parameter aus.
- **Rad**: Mausrad- oder Stiftrad-Eingabe.
- **Richtung**: Winkel der Hubrichtung.
- **Ausblenden**: Deckkraft oder Größe über eine feste Anzahl von Tupfen verblassen lassen.

Jede dynamische Eingabe kann mehreren Eigenschaften unabhängig zugeordnet werden. Öffnen Sie zum Konfigurieren **Tool-Optionen** → **Dynamik**.

#### Verblassen und Farbe
Im Expander **Fade and Colour** (verschachtelt in Stroke Effects; nur sichtbar, wenn **Dynamics System** aktiviert ist):

- **Relativer Anfangswinkel**: Der Wert **Anfangswinkel** wird relativ zur Strichrichtung und nicht als absoluter Leinwandwinkel interpretiert.
- **Anfangswinkel verblassen**: Blendet im Verlauf des Strichs vom **Anfangswinkel** am Strichanfang in Richtung des Live-Dynamikwinkels aus. Durch die Aktivierung wird **Relativer Anfangswinkel** aktiviert.
- **Anfangswinkel** (-180–180°): Der Pinselwinkel ganz am Anfang eines Strichs, bevor die Dynamik übernimmt.
- **Angle Blend Factor** (0,0–1,0): Steuert, wie schnell der Pinselwinkel vom Anfangswinkel zum dynamischen Winkel übergeht. 0 = hält den Anfangswinkel; 1 = nutzt sofort den volldynamischen Winkel.
- **Richtungsstabilisierung** (0–100 px): Verzögert die richtungsabhängige Dynamik, indem der Zeiger so viele Pixel zurücklegen muss, bevor die Strichrichtung aktualisiert wird. Nur aktiv, wenn **Post Process** ausgeschaltet ist (Post Process sorgt für seine eigene Stabilisierung). 0 = deaktiviert (unmittelbare Richtung, kann bei Hubbeginn springen).
- **Fade-Länge**: Abstand in Leinwandeinheiten, über den der Fade abläuft.
- **Wiederholen**: Wie der Fade wiederholt wird, sobald die Fade-Länge erschöpft ist (Keine, Schleife, Sägezahn, Dreieck).


### Bürstenköpfe
Malen Sie mit mehreren unabhängigen Pinselköpfen, die ringförmig um den Strichweg angeordnet sind. Die Steuerelemente werden im Expander **Pinselköpfe** im Bedienfeld „Werkzeugoptionen“ angezeigt.- **Köpfe**: Anzahl der gleichzeitigen Bürstenköpfe (1–16).
- **Größe**: Gerenderte Größe jedes Kopfes im Verhältnis zur globalen Pinselgröße (0,1–1,0).
- **Steifheit**: Wie steif der Umlaufradius der dynamikskalierten Pinselgröße folgt. 0 = Orbit verfolgt die Dynamikgröße; 1 = Orbit bleibt auf der Basisgröße fixiert.
- **Folgt** (0,0–1,0): Wie stark der Formationsring der Schlagbewegungsrichtung folgt. Bei 1,0 (Standard) steht der Ring immer senkrecht zur Fahrtrichtung. Bei 0,0 ist es an den statischen **Winkel**-Wert gebunden. Zwischenwerte vermischen sich zwischen den beiden Ausrichtungen. Dies ist unabhängig vom Dynamics-System – es ist keine Winkeldynamikkonfiguration erforderlich.
- **Winkel** (0–360°): Statische Ausrichtung des Formationsrings, verwendet, wenn **Follows** unter 1,0 liegt. Wenn **An Ansicht sperren** aktiv ist, wird der Winkel automatisch an die Drehung der Leinwand angepasst.
- **Variation**: Größenvariation pro Kopf und auf die Dynamik angewendete Druckverzerrung.
- **Deckkraftvariation**: Opazitätsvariation pro Kopf, unabhängig von der Größenvariation.
- **Seed**: Zufälliger Seed für die Pro-Kopf-Variation behoben. Gilt nur, wenn **Zufällige Borsten** ausgeschaltet ist.
- **Zufällige Borsten**: Borstencharakter bei jedem Strich zufällig auswählen (Seed wird ignoriert).
- **Unabhängige Frames**: Für animierte Pinsel – wenn diese Option aktiviert ist, bewegt jeder Kopf sein Animationsframe unabhängig weiter.

### Zusätzliche Optionen

Im Expander **Zusätzliche Optionen** (standardmäßig ausgeblendet):

- **An Ansicht sperren**: Hält das Erscheinungsbild des Pinsels relativ zur Leinwandansicht fest – wenn Sie die Leinwand drehen, dreht sich der Pinsel mit.
- **Einfache Pinselgrenze**: Verwendet einen einfachen Kreis als Umriss des Pinselcursors, anstatt die vollständige Pinselform darzustellen. Nützlich für komplexe oder große Pinsel, bei denen das Zeichnen der genauen Grenze aufwändig ist.
- **Gleichmäßiger Jitter**: Wenn diese Option aktiviert ist, werden Spitzenversätze vom Schieberegler **Jitter** aus einer gleichmäßigen Verteilung gezogen (jeder Versatz ist innerhalb des Bereichs gleich wahrscheinlich). Wenn diese Option deaktiviert ist, ist die Verteilung eine Gaußsche Verteilung (versetzt den Cluster zur Mitte hin).
- **Zuletzt verwendete Farben wiederherstellen**: Stellt beim Start die Vordergrund- und Hintergrundfarben der vorherigen Sitzung wieder her, anstatt standardmäßig Schwarz und Weiß zu verwenden.
- **Zufällige Horizontale**: 50 % Chance, jeden Stempel pro Tupfer von links nach rechts zu spiegeln.
- **Zufällige Vertikale**: 50 % Chance, jeden Stempel pro Tupfer auf den Kopf zu stellen.
- **Zufällige Drehung**: Rotiert jeden Stempel zufällig um 0°, 90°, 180° oder 270° pro Tupfer.
- **Animation zurücksetzen**: Für animierte Pinsel – wenn diese Option aktiviert ist, startet die Animation bei jedem neuen Strich ab Bild 0 neu; Wenn diese Option deaktiviert ist, wird an der Stelle fortgesetzt, an der der vorherige Strich endete.