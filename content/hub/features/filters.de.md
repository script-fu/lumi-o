---
title: "Filter"
type: docs
url: "hub/features/filters"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
---
Lumis Filter-Menü vereint Korrekturen, stilisierte Linseneffekte, prozedurale Texturgeneratoren, druckinspirierte Behandlungen und Analysewerkzeuge an einem Ort. Die Menüreihenfolge ist praktisch statt akademisch: Unschärfe- und Verbesserungswerkzeuge liegen nebeneinander, Verzerrungs- und Beleuchtungseffekte nach Erscheinung gruppiert, Textur- und Mustergeneratoren zusammen, wenn Quellmaterial erzeugt statt ein vorhandenes Bild verändert werden soll.

Filterdialoge folgen demselben Workflow. Voreinstellungen, Vorschau, geteilte Ansicht sowie Deckkraft- oder Mischsteuerungen ermöglichen schnelles Feintuning; auf Ebenen kann das Ergebnis als bearbeitbarer zerstörungsfreier Filter verbleiben, statt sofort zusammengeführt zu werden. Lumi führt zudem einen Verlauf der Filternutzung — den letzten Effekt wiederholen oder den letzten Dialog erneut öffnen gehört zum normalen Malrhythmus, nicht zu einer Extraaufgabe.

## Unschärfe

### Gaußsche Unschärfe

Gaussian Blur ist Lumis Standard-Weichzeichner: saubere, gleichmäßige Unschärfe mit separaten horizontalen und vertikalen Größen, Kantenbehandlung und Kernel-Optionen. Allzweckwahl für Weichzeichnung, weiche Masken, atmosphärische Tiefe und jeden Workflow, in dem die Unschärfe selbst neutral bleiben soll.

### Pixelisieren

Pixelize reduziert Details zu bewussten Blockstrukturen statt weicher Unschärfe. Blockbreite, -höhe, Versätze, Pixelform und Füllverhalten im Dialog machen ihn zum groben Zensur-Effekt, steuerbarem Mosaik oder grafischer Niedrigauflösungs-Behandlung.

### Selektive Gaußsche Unschärfe

Selective Gaussian Blur weichzeichnet innerhalb von Bereichen und bewahrt stärkere Kanten. Nützlich, wenn ein Bild ruhigere Textur oder weniger Rauschen braucht, ohne die großen Formgrenzen zu verlieren, die klar lesbar bleiben müssen.

### Linsenunschärfe

Lens Blur ist einer der illustrationorientierten Unschärfefilter. Steuerungen für Polygon-Iris, Blattkrümmung, anamorphe Streckung, Glanzlicht-Verstärkung und konfigurierbaren Fokusbereich — weniger generischer Weichzeichner, mehr stilisiertes Tiefenschärfewerkzeug mit geformtem Bokeh.

### Tilt-Shift

Tilt-Shift hält ein steuerbares Fokusband scharf und verwischt das Bild darüber und darunter progressiv. Bandwinkel, Weichzeichnung, Perspektivbias, Irisform und Miniatur-Verstärkung eignen sich für Miniatur-Look, Architekturansichten und Kompositionen, in denen der Fokus als entworfener Streifen statt kreisförmiger Tiefenhinweis wirken soll.

### Kreisförmige Bewegungsunschärfe

Circular Motion Blur verwischt Details um einen Mittelpunkt und verwandelt Kanten in Rotationsspuren. Natürliche Wahl für sich drehende Motive, turbinenartige Energie oder Illustrationen mit orbitaler Bewegung.

### Lineare Bewegungsunschärfe

Linear Motion Blur streckt Details in eine Richtung — Reise, Kamerabewegung oder schnelle Geste über das Bild. Besonders nützlich, wenn die Bewegung richtungsweisend und grafisch statt diffus wirken soll.

### Zoom-Bewegungsunschärfe

Zoom Motion Blur strahlt Details von einer Mitte nach außen — das Gefühl eines Ansturms auf oder weg vom Betrachter. Gut für Aufprallmomente, Geschwindigkeitslinien und Kompositionen mit Kamera-Zoom-Energie, ohne das ganze Bild neu zu malen.

## Verbessern

### Hochpass

High Pass isoliert feinen lokalen Kontrast statt breiter Tonveränderung. Mit nur Skala und Kontrast ist es ein unkompliziertes Werkzeug zum Extrahieren von Kantendetails, gestochen scharfen Overlays oder Schärfungsdurchgängen, die Struktur stärker als Farbe betonen sollen.

### Rauschreduzierung

Noise Reduction ist der Gegenmove: unerwünschte feine Variation wird unterdrückt, damit größere Formen klarer lesen. Nützlich bei gescanntem Material, komprimierten Texturen oder überarbeiteten Passagen, die vor weiterem Malen oder Filtern vereinfacht werden müssen.

### Schärfen

Sharpen nutzt ein Unscharfmasken-Modell — Radius, Stärke und Schwellenwert steuern, wie stark lokaler Kontrast angehoben wird. Praktisch zur Wiederherstellung von Klarheit nach Unschärfe, Export-Skalierung oder subtilen Finish-Durchgängen, bei denen Details hervortreten sollen, ohne jedes Pixel in Rauschen zu verwandeln.

## Farbe

### Tonwertkorrektur

Tonal Grading ordnet Farben nach Tonwertbereich neu zu — ohne Kontrastkurven neu zu zeichnen. Die Luminanz jedes Pixels wählt eine sanfte Mischung aus drei Nutzerfarben für Schatten, Mittelton und Lichter; das Bild behält seine Hell-Dunkel-Struktur, während sich die Palette verschiebt. Stärke pro Bereich, Balance-Bias im Lightroom-Stil (links begünstigt Schattengrad, rechts Lichtergrad) und Übergangsweichheit steuern Reichweite und Überlappung der Abstufungen. Für Illustration, Comics, Concept Art und Fotografie, wenn ein stimmiger Look das Ziel ist.

## Verzerren

### Chromatische Aberration

Chromatic Aberration trennt Farbkanäle von einem gewählten Zentrum nach außen — mit Steuerungen für radiale oder tangentiale Richtung, Bias zwischen Kanalpaaren, Abfall und Luminanzerhalt. Code und Dialog behandeln es als Zwei-Wege-Werkzeug: stilisierte Linsenfringing für Energie hinzufügen oder Vorzeichen umkehren, um leichte Aberration im Quellmaterial zu korrigieren.

### Linsenverzärung

Lens Distortion formt das Bild durch Tonen- oder Kissenverzerrung, Randterme, Zoomkompensation, Mittelversatz und Eckenaufhellung. Nützlich zur Korrektur optisch verbogener Bilder oder zum bewussten Weitwinkel- oder Retro-Objektivcharakter.

## Beleuchtung

### Bloom

Bloom verwandelt helle Bereiche in kontrolliertes Leuchten — Schwellenwert, Weichheit, Radius und Stärke bestimmen Ausbreitung und Anhebung. Zusätzliche Belichtungsbegrenzung hält es als Highlight-Effekt nutzbar statt automatischem Auswaschen.

### Himmel

Sky ist mehr als Tönung oder Verlaufs-Overlay: ein analytischer Himmel mit Preetham-, Hosek/Wilkie- oder Nishita-Modellen. Projektion, Sonnenwinkel, Trübung, atmosphärische Dichte, Höhe, Sonnenscheiben-Steuerungen und Belichtung im Dialog — von einfachem klaren Hintergrund bis zu physisch fundierterem Sonnenuntergang oder Dämmerung.

### Vignette

Vignette verdunkelt, färbt oder tilgt Richtung Bildränder — mit Form, Radius, Weichheit, Gamma, Proportion, Stauchung, Rotation und Positionierung auf der Leinwand. Klassische fotografische Randbehandlung, flexibel genug als Rahmenmaske oder unregelmäßiger Kompositions-Spotlight.

## Rauschen

### HSV-Rauschen

HSV Noise randomisiert Farbton, Sättigung und Helligkeit unabhängig. Nützlich, wenn ein Bild Farblebendigkeit oder analoge Instabilität braucht, ohne die lokale Struktur vollständig aufzubrechen.

### Hurl

Hurl ist die extreme Rauschvariante: Pixel werden durch völlig zufällige Farben ersetzt. Destruktive Chaosquelle für Glitch-Arbeit, distressed Texturen oder Masken, die aggressive Auflösung brauchen.

### Pick

Pick ersetzt jedes Pixel durch einen zufällig gewählten Nachbarn — das Bild bleibt mit der Quelle verwandt statt reines Rauschen zu werden. Gemischte, körnige Variation, die organischer wirken kann als völlig zufälliges Rauschen.

### Spread

Spread streut Pixel durch zufällige Verschiebung innerhalb eines Radius. Für bewegungslose Störung: gebrochene Oberfläche, verschmierte Kante oder abgenutzte Textur, die noch die Farbbeziehungen des Quellbilds trägt.

### Fraktal

Fractal erzeugt kachelbar fraktales Perlin-Rauschen — besonders wertvoll als wiederverwendbare Quelle für Masken, Wolken, Papiertextur, geländeähnliche Auflockerung und prozedurale Overlays. Durch Kachelbarkeit ohne sichtbare Nähte in größeren Workflows.

### Blue-Noise-Körnung

Blue Noise Grain ist Lumis monochromer Körnungsgenerator im Film- und Druckstil. Korngrößen-Voreinstellungen, Blue-Noise-Maskierung, Mittelton- und Schatten-Bias sowie Seed-Steuerungen im Dialog — gleichmäßige, kontrollierbare Körnung statt zufälliger monochromer Flecken.

### Risograph-Körnung

Risograph Grain baut auf derselben Körnungslogik auf und macht daraus einen Zwei-Platten-Druckeffekt. Separate Tintenfarben, Plattenbalance, absichtliche Fehlregistrierung und geseedete Variation — für Poster, Indie-Print-Ästhetik und Illustrationen, die physisch überdruckt statt digital perfekt wirken sollen.

### Halbton (FM)

Halftone (FM) erzeugt stochastischen, frequenzmodulierten Halbton mit Blue-Noise oder verwandten Schwellenwertmethoden. Farbmodi für Monochrom, Duoton und CMYK plus Punktvergrößerung und Platten-Dekorrelation — druckähnliche Textur, unregelmäßig und lebendig statt starrem Raster.

## Kanten

### Differenz zweier Gauß-Funktionen

Difference of Gaussians erkennt Kanten, indem zwei unscharfe Bildversionen voneinander subtrahiert werden. Kompakter, nützlicher Operator für Kantenkarten, stilisierte Linienextraktion und Strukturübergänge ohne vollständigen Schwellenwert-Umriss.

## Morphologie

### Median

Median ersetzt jedes Pixel durch den Medianwert der Nachbarschaft — entfernt isoliertes Rauschen und bewahrt stärkere Grenzen besser als einfache Unschärfe. Praktischer Bereinigungsfilter gegen kleines visuelles Rauschen, ohne gleich das ganze Bild weichzuzeichnen.

### Dilatation

Dilate lässt helle Regionen nach außen wachsen mit derselben formbewussten Nachbarschaftslogik. Helle Marken verdicken, helle Formen erweitern, kleine dunkle Lücken schließen.

### Erosion

Erode wächst dunklere Regionen und zieht helle zurück. Nützlich zum Ausdünnen heller Details, Vergrößern dunkler Massen oder Straffen von Masken und grafischen Formen.

## Muster

### Schachbrett

Checkerboard erzeugt ein regelmäßig wechselndes Kachelmuster. Einfach — und deshalb nützlich zum Testen von Transparenz, Maskenbau, grafischen Hintergründen oder sauberem geometrischem Quellmaterial.

### Raster

Grid zeichnet wiederholte horizontale und vertikale Unterteilungen — Layout-Hilfen, Design-Hintergründe, technische Illustration, prozedurale Maskierung. Als Filter generiert, lassen sich Abstand und Erscheinung abstimmen ohne manuelles Musterbauen.

### Voronoi

Voronoi erzeugt kachelbare Zelltextur aus geseedeten Punkten — Feature-Typ, Distanzmetrik, Zufälligkeit, fraktales Detail, nahtlose Umhüllung. Von sauberen Riss-Zellstrukturen bis zu organischem Stein, Haut, Karte oder abstraktem Netzwerk.

### Welle

Wave erzeugt band- oder ringförmige Muster aus Wellenformprofil, geometrischer Anordnung, Verzerrung, fraktalem Detail und Phasenversatz. Mehr als Streifenwerkzeug: kontrollierte Wellen, topografische Bänder, Moiré-ähnliche Grafik oder verrauschte konzentrische Musterfelder.

### Halbton (AM)

Halftone (AM) wendet klassisches amplitudenmoduliertes Punktraster an — Frequenz, Punktform, Schärfe, Farbmodus und CMYK-Winkel für rosettenartige Druckstruktur. Gegenüber FM-Halbton die geordnetere, erkennbar mechanische Option für Zeitungspapier, Offset-Lithografie oder bewusst sichtbare Siebgeometrie.
