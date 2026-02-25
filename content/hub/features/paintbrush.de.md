---
title: "Pinselwerkzeug"
type: docs
---
Der Pinsel ist das wichtigste Malwerkzeug, das für eine reaktionsschnelle, intelligente Pinselführung mit vollständiger Kontrolle über Druck, Geschwindigkeit, Neigung und Abstandsdynamik entwickelt wurde.

## Übersicht

Das Pinselwerkzeug unterstützt Raster-, prozedural generierte und animierte Pinseltypen. Striche können stabilisiert, geglättet und nachbearbeitet werden. Die Pinseldynamik reagiert auf die Stifteingabe und ermöglicht eine präzise Kontrolle über Deckkraft, Größe, Farbe, Winkel und andere Eigenschaften während eines Pinselstrichs.

## Pinseltypen

### Rasterpinsel (.raster)

Bitmap-Pinselbilder, die Alpha-Transparenz unterstützen.

### Generierte Pinsel (.param)

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

### Pinseleigenschaften

Im Expander **Pinseleigenschaften** (standardmäßig erweitert):
- **Größe**: Pinseldurchmesser in Pixel.
- **Seitenverhältnis**: Die Pinselform stauchen oder strecken (-1,0–1,0). 0 = unverändert; Negative Werte drehen den Kürbis um 90°.
- **Winkel**: Dreht den Pinselstempel (-180–180°). Unabhängig von der Schlagrichtungsdynamik.
- **Härte**: Sanfter Übergang (0,0) bis scharfe Kante (1,0).
- **Abstand**: Abstand zwischen gemalten Tupfen als Prozentsatz der Pinselgröße. Niedriger = sanftere Striche; höher = Streumuster.
- **Texture Bias**: Verzerrt die Reaktion der Stempeltextur; 50 ist neutral. Niedrigere Werte begünstigen das Aufbrechen der Textur und eine geglättete Oberfläche, indem sie zum Fuß der Wertekurve hin gezogen werden. Höhere Werte klemmen in Richtung der festen Füllungen, indem sie in Richtung der Schulter drücken. Der sichtbare Effekt hängt vom Tonumfang der Textur ab.
- **Jitter**: Versetzt jede Spitzenposition nach dem Zufallsprinzip um bis zu so viele Pixel (0–1024).
- **Radiergummi**: Größenmultiplikator, der angewendet wird, wenn dieser Pinsel als Radiergummi verwendet wird (0,1–10,0). Wird im Radiergummi selbst nicht angezeigt.

### Dynamik

Im **Dynamics**-Expander:
- **Dynamik**: Master-Freigabe für das aktive Dynamik-Preset.
- **Dynamik-Voreinstellung**: Wählt aus, welche Eingangszuordnungen verwendet werden.
- **Mit Druck multiplizieren**: Umschalter für zusätzliche Druckmultiplikation (wird angezeigt, wenn Dynamics aktiviert ist).### Schlaganfallverhalten
Im **Stroke Behaviour**-Expander:
- **Aufbau**: Wenn diese Option aktiviert ist, erhöht sich die Deckkraft jedes Tupfers, anstatt dass er als einzelner Strich zusammengesetzt wird.
- **Nachbearbeitung**: Wendet Stabilisierung, Geschwindigkeitskomprimierung und Wiederholungskorrektur an, nachdem der Schlag abgeschlossen ist, und verbessert so die Konsistenz ohne Latenz.
  - **Drehschwellenwert**: Winkelschwellenwert (0–180°) zur Richtungskorrektur an scharfen Ecken. 0 = Sprungrichtung fix.
  - **Vorschauschwellenwert**: Unterdrückt die Nachbearbeitungsvorschau, wenn die Strichgeschwindigkeit diesen Wert überschreitet (0 = immer Vorschau).

#### Kalligraphisch

Wenn aktiv, wird das Stempeln von Spitzen durch einen kontinuierlichen geometrischen Korridor ersetzt:
- **Dynamische Deckkraft**: Moduliert die Deckkraft innerhalb des Strichs basierend auf Geschwindigkeits- und Richtungsänderungen. Funktioniert am besten bei feinen, kontrollierten Strichen; Bei schnellem Gekritzel sind die Ergebnisse weniger vorhersehbar. Experimental.
- **Geschwindigkeitswachstum** (0–100 %): Maximal zulässige Größenzunahme pro Probe als Prozentsatz der Größe der vorherigen Probe. Begrenzt, wie schnell eine geschwindigkeitsgesteuerte Größendynamik wachsen kann, und verhindert so plötzliche Sprünge, wenn der Schlag beschleunigt wird.
- **Velocity Shrink** (0–100 %): Maximal zulässige Größenverringerung pro Probe. Begrenzt, wie schnell die Größe sinken kann, wenn der Hub verlangsamt wird.

#### Stabilisierung und Glättung

- **Richtungsstabilisierungsabstand** (0–100 px): Minimaler Zeigerweg, bevor richtungsempfindliches Verhalten einsetzt, um frühe Winkelsprünge zu vermeiden.

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

### Hubmodulation

Im **Stroke Modulation**-Expander (wird nur angezeigt, wenn **Dynamics** aktiviert ist):- **Relativer Anfangswinkel**: Der Wert **Anfangswinkel** wird relativ zur Strichrichtung und nicht als absoluter Leinwandwinkel interpretiert.
- **Anfangswinkel verblassen**: Blendet im Verlauf des Strichs vom **Anfangswinkel** am Strichanfang in Richtung des Live-Dynamikwinkels aus. Durch die Aktivierung wird **Relativer Anfangswinkel** aktiviert.
- **Anfangswinkel des Pinsels** (-180–180°): Der Pinselwinkel ganz am Anfang eines Strichs, bevor die Dynamik übernimmt.
- **Anfangswinkelüberblendung** (0,0–1,0): Steuert, wie schnell der Pinselwinkel vom Anfangswinkel zum dynamischen Winkel übergeht. 0 = hält den Anfangswinkel; 1 = nutzt sofort den volldynamischen Winkel.
- **Fade-Länge**: Abstand in Leinwandeinheiten, über den der Fade abläuft.
- **Wiederholen**: Wie der Fade wiederholt wird, sobald die Fade-Länge erschöpft ist (Keine, Schleife, Sägezahn, Dreieck).


### Bürstenköpfe

Mit „Bürstenköpfe“ werden mehrere unabhängige Bürstenköpfe auf einem kreisförmigen **Umlaufring** platziert, der auf dem Strichpfad zentriert ist. Jeder Kopf malt bei jedem fortschreitenden Strich einen vollständigen Tupfer an seiner eigenen Position, wodurch mehrere parallele oder gefächerte Striche gleichzeitig erzeugt werden.

Der Umlaufradius wird durch die globale Pinselgröße minus der Kopfgröße bestimmt: Größere Köpfe sitzen näher an der Mitte; kleinere Köpfe kreisen weiter draußen. Die Köpfe verteilen sich gleichmäßig um den Ring. Mit zwei Köpfen erhalten Sie einen auf jeder Seite des Strichs, wodurch eine symmetrische Ausbreitung entsteht, die sich wie eine Kalligraphiefeder verhält. Der Schieberegler **Richtung folgen** dreht den gesamten Ring so, dass er senkrecht zum Strich bleibt, sodass die Spitze beim Malen auf natürliche Weise der Richtung folgt. Durch das Hinzufügen weiterer Köpfe werden diese nach und nach um den Ring verteilt, bis hin zu einem vollständigen Sprühkreis bei 16.

Die Steuerelemente werden im Expander **Pinselköpfe** im Bedienfeld „Werkzeugoptionen“ angezeigt.

- **Anzahl**: Anzahl gleichzeitiger Bürstenköpfe (1–16).
- **Kopfgröße**: Gerenderte Größe jedes Kopfes im Verhältnis zur globalen Pinselgröße (0,1–1,0).
- **Orbit Aspect Ratio** (0,1–1,0): Formt die Formationsbahn von einem Kreis in eine Ellipse. 1,0 = Kreisbahn; niedrigere Werte quetschen die Nebenachse.
- **Formationswinkel** (0–360°): Statische Ausrichtung des Formationsrings, verwendet, wenn **Follow Direction** unter 1,0 liegt.
- **Follow Direction** (0,0–1,0): Wie stark der Formationsring der Schlagrichtung folgt. Bei 1,0 steht der Ring immer senkrecht zur Fahrtrichtung; Bei 0,0 wird der statische **Formationswinkel**-Wert festgelegt.
- **Druckvariation**: Größenvariation pro Kopf, angewendet als unabhängige Druckvorspannung durch die Dynamikkurven.
- **Deckkraftvariation**: Opazitätsvariation pro Kopf, unabhängig von der Größenvariation.

#### Streuung

Haupt-Scatter-Steuerelemente im **Brush Heads**-Expander:

- **Streuungswinkel** (0–360°, Standard 10°): Dreht nur die zufällige Streuungskomponente (nicht den Füllabstand). Die Winkel pro Kopf/pro Tupfer sind mit kontrollierter Überkreuzung nach außen gerichtet, um starre Spiegelfahnen zu vermeiden. Auf 360° geklemmt.
- **Streuentfernung** (0–10.000 px): Zufällige Vorwärtsverschiebung von der Füllabstandsposition jedes Kopfes. Jeden Tupfer erneut aufrollen.
- **Scatter Size Balance** (0,0–1,0): Steuert die Unterdrückungssteilheit für Köpfe über dem Schwellenwert. Bei 1,0 streuen alle Köpfe gleichmäßig; Niedrigere Werte unterdrücken zunehmend größere Köpfe, während Köpfe bei/unter dem Schwellenwert bei voller Streuentfernung bleiben.

### Zusätzliche Optionen

Im Expander **Zusätzliche Optionen** (standardmäßig ausgeblendet) werden Steuerelemente als Überlaufabschnitte gruppiert, die seltener geändert werden. Dadurch bleibt der Fokus der Hauptexpander auf häufig angepassten Malsteuerungen.#### Pinseleigenschaften (Überlauf)
- **Winkel an den Bildschirmbereich anpassen**: Der Pinselwinkel wird an den Bildschirmbereich angepasst, sodass der Winkel eben bleibt, während sich die Leinwand dreht/spiegelt. Keine Auswirkung, wenn Dynamics den Winkel steuert.
- **Zufälliges horizontales Spiegeln**: 50 % Chance, jeden Stempel pro Tupfer von links nach rechts zu spiegeln.
- **Zufälliges vertikales Spiegeln**: 50 % Chance, jeden Stempel pro Tupfer auf den Kopf zu stellen.
- **Zufällige Drehung**: Rotiert jeden Stempel zufällig um 0°, 90°, 180° oder 270° pro Tupfer.
- **Gleichmäßiger Jitter**: Wenn diese Option aktiviert ist, werden Spitzenversätze vom Schieberegler **Jitter** aus einer gleichmäßigen Verteilung gezogen (jeder Versatz ist innerhalb des Bereichs gleich wahrscheinlich). Wenn diese Option deaktiviert ist, ist die Verteilung eine Gaußsche Verteilung (versetzt den Cluster zur Mitte hin).
- **Animation zurücksetzen**: Für animierte Pinsel: Wenn diese Option aktiviert ist, startet die Animation bei jedem neuen Strich ab Bild 0 neu. Wenn diese Option deaktiviert ist, wird an der Stelle fortgesetzt, an der der vorherige Strich endete.

#### Bürstenköpfe (Überlauf)

Bildung:
- **Borstensteifheit**: Wie steif der Umlaufradius der dynamikskalierten Pinselgröße folgt. 0 = Umlaufbahn dehnt sich durch Druck aus und zieht sich zusammen; 1 = Orbit bleibt auf der Basisgröße fixiert.
- **Füllabstand** (0,0–1,0): Verteilt die Köpfe über die Lücke zwischen aufeinanderfolgenden Tupferpositionen. Der stabile Charakterwert jedes Kopfes bestimmt seine Neigungsrichtung; Bei 1,0 Köpfen wird das gesamte Abstandsintervall ausgefüllt. Der Charakter ist pro Samen stabil.

Streuung:
- **Schwellenwert für Streugröße** (0,01–100 px): Schwellenwertradius für die gesamte Streuentfernung. Köpfe mit oder unter diesem Radius nutzen die volle Streudistanz; Größere Köpfe werden zunehmend näher an den Strich herangezogen.

Randomisierung:
- **Charakter-Seed** (0–255): Der Seed für Pro-Kopf-Charaktere wurde korrigiert (Größe, Position des Füllabstands). Derselbe Samen reproduziert bei jedem Schlag die gleiche Formation. Desensibilisiert, wenn **Kopfcharakter zufällig auswählen** aktiviert ist.
- **Kopfzeichen randomisieren**: Zeichnet die Zeichenwerte pro Kopf (Größe, Streuposition) bei jedem Stempel neu, sodass die Formation entlang des Strichs völlig chaotisch ist. Überschreibt **Charakter-Seed**.
- **Animationsrahmen randomisieren**: Für animierte Pinsel: Jeder Kopf bewegt seinen Animationsrahmen unabhängig weiter.

#### Strichverhalten (Überlauf)

- **Zuletzt verwendete Farben wiederherstellen**: Stellt beim Start die Vordergrund- und Hintergrundfarben der vorherigen Sitzung wieder her, anstatt standardmäßig Schwarz und Weiß zu verwenden.
- **Einfache Pinselgrenze**: Verwendet einen einfachen Kreis als Umriss des Pinselcursors, anstatt die vollständige Pinselform darzustellen. Nützlich für komplexe oder große Pinsel, bei denen das Zeichnen der genauen Grenze aufwändig ist.