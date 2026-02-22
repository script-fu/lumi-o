---
title: "Ebenen und zerstörungsfreie Bearbeitung"
type: docs
---
Das Ebenensystem von Lumi ermöglicht komplexe, zerstörungsfreie Arbeitsabläufe mit vollständiger Kontrolle über Mischung, Maskierung und Komposition.

## Übersicht

Ebenen sind die Grundlage einer strukturierten Illustration. Jede Ebene ist unabhängig und verfügt über einen eigenen Mischmodus, eine eigene Deckkraft und eine optionale Ebenenmaske. Gruppen können Ebenen hierarchisch mit ihren eigenen Misch- und Beschneidungseigenschaften verschachteln.

## Zugriff

**Bedienfelder** → **Ebenen** oder das standardmäßige **Ebenen**-Bedienfeld auf der rechten Seite.

## Ebenentypen

### Farbschichten
Standard-Rasterebenen für gemalte Inhalte. Speichern Sie Pixeldaten als GEGL-Puffer mit optionaler Alpha-Transparenz.

### Gruppenebenen
Hierarchische Container zum Organisieren zusammengehöriger Ebenen. Gruppen können ihren eigenen Mischmodus, ihre eigene Deckkraft und ihre eigenen Schnittmasken haben. Gruppenprognosen werden bei Bedarf zusammengestellt.

### Ebenenmasken
An jede Ebene angehängte Graustufenmasken, die die Deckkraft pro Pixel steuern. Das Malen auf einer Maske mit Weiß macht Pixel undurchsichtig; Schwarz macht sie transparent; Grau sorgt für teilweise Deckkraft.

## Mischmodi

Jede Ebene verfügt über einen Mischmodus, der bestimmt, wie sie mit den darunter liegenden Ebenen kombiniert wird:

- **Normal**: Direkte Deckkraftmischung.
- **Multiplizieren**: Abdunkeln durch Multiplizieren der Farbwerte.
- **Bildschirm**: Durch Invertieren, Multiplizieren und erneutes Invertieren aufhellen.
- **Overlay**: Kombination aus Multiplikation und Bildschirm.
- **Hinzufügen**: Additive Mischung (summiert Farbwerte).
- **Subtrahieren**: Subtraktives Mischen.
- **Farbe, Farbton, Sättigung, Helligkeit**: HSL-Komponentenmischung.

## Ausschneiden und Maskieren

- **Composite-Modus – Auf Hintergrund zuschneiden**: Wenn Sie den Zusammensetzungsmodus einer Ebene auf **Auf Hintergrund zuschneiden** festlegen, wird das Compositing auf Bereiche beschränkt, in denen die angesammelten **Union**-Ebenen darunter eine Deckkraft hergestellt haben. Die Ebene malt nur dort, wo diese Ebenen Inhalte haben – sie kann den Alpha-Fußabdruck nicht erweitern. Dies wird pro Ebene im Dialogfeld „Ebenenattribute“ (Dropdown-Menü „Composite-Modus“) festgelegt. Wenn der effektive Zusammensetzungsmodus einer Ebene etwas anderes als „Vereinigen“ ist, wird das Augensymbol im Ebenenbedienfeld durch ein zusammengesetztes Symbol ersetzt, um das nicht standardmäßige Zusammensetzungsverhalten anzuzeigen.

  **Beispiel – gemeinsam genutzte Alphaform:** In einer Gruppe enthält die untere Ebene einen gefüllten Kreis auf einem transparenten Hintergrund, der auf den Standard-Kompositmodus **Union** eingestellt ist. Jede darüber liegende Ebene in derselben Gruppe ist auf **Auf Hintergrund zuschneiden** eingestellt. Diese Ebenen können nur dort malen, wo der Kreis für Deckkraft sorgt – eine Form, viele Ebenen. Dies ist ein gängiges Muster zum Färben, Schattieren und Detaillieren innerhalb einer definierten Silhouette, ohne dass man sich Gedanken über Verschütten machen muss.
- **Ebenenmasken**: Wenden Sie eine Graustufenmaske an, um die Sichtbarkeit der Ebene Pixel für Pixel zu steuern. Das Aufmalen der Maske mit Weiß zeigt; schwarze Kaschierungen; Grau sorgt für teilweise Deckkraft.
- **Reine untergeordnete Masken**: Masken werden als untergeordnete Masken im Zeichenstapel gespeichert, um Datenverlust während Transformationen zu verhindern.

## Ebenenauswahl (Alt-Taste)

Durch Tippen auf **Alt** (linke Alt-Taste), während Sie mit der Maus über die Leinwand fahren, wird die Ebene mit sichtbaren Pixeln unter dem Cursor ausgewählt – ohne Werkzeugwechsel oder Klicken.

### Wie es funktioniert

- **Alt drücken**: Der Cursor verwandelt sich in ein Fadenkreuz und zeigt damit an, dass der Auswahlmodus aktiv ist.
- **Alt-Taste loslassen**: Lumi wählt die oberste nicht transparente Ebene an der Cursorposition (Deckkraft > 25 %) aus und wählt sie aus. Die Ebene wird im Ebenenbedienfeld hervorgehoben und in der Statusleiste wird **"Ausgewählte Ebene: 'Ebenenname'"** angezeigt.
- Am Mittelpunkt der ausgewählten Ebene auf der Leinwand wird ein Griff gezeichnet. Der Griff wird kleiner und verblasst, wenn sich der Cursor wegbewegt.

### Durch Schichten radelnBei jedem weiteren Antippen der Alt-Taste an derselben Stelle wird an dieser Stelle die **nächste Ebene** im Stapel ausgewählt. Lumi merkt sich die zuletzt ausgewählte Ebene und springt an dieser vorbei zur darunter liegenden Ebene. Sobald die Unterseite des Stapels erreicht ist, wechselt der nächste Tipp zurück zur obersten Ebene an dieser Position. Dies erleichtert den Zugriff auf verschachtelte Ebenen in komplexen Szenen durch wiederholtes Antippen der Alt-Taste.

### Stornierungsregeln

Der Auswahlvorgang wird abgebrochen (wird nicht beim Loslassen der Alt-Taste ausgelöst), wenn eines der folgenden Ereignisse eintritt, während die Alt-Taste gedrückt gehalten wird:

- Eine Maustaste wird gedrückt (Links- oder Rechtsklick).
- Eine beliebige andere Taste wird gedrückt.

Dadurch wird sichergestellt, dass Alt-Ziehgesten (z. B. Anpassung der Pinselgröße) und Alt-modifizierte Tastenkombinationen funktionieren, ohne versehentlich die aktive Ebene zu ändern.

### Einschränkungen

- Die Ebenenauswahl wird während der Werkzeugoperationen **Transformieren** nicht aktiviert – Alt hat dort eine andere Bedeutung.
- Die Auswahl erfolgt nicht, wenn eine schwebende Auswahl vorhanden ist.
- Nur die linke Alt-Taste löst das Auswählen aus; Die rechte Alt-Taste wird als Standardmodifikator behandelt.

## Operationen

Im Ebenenbedienfeld:

- **Ebene erstellen**: Klicken Sie mit der rechten Maustaste → **Neue Ebene** oder verwenden Sie das Menü **Ebene**.
- **Duplizieren**: Klicken Sie mit der rechten Maustaste → **Duplizieren** oder **Ebene** → **Duplizieren**.
- **Löschen**: Klicken Sie mit der rechten Maustaste → **Löschen**, oder wählen Sie **Löschen** aus und drücken Sie.
- **Neu anordnen**: Ziehen Sie Ebenen nach oben oder unten, um die Stapelreihenfolge zu ändern.
- **Umbenennen**: Doppelklicken Sie auf den Ebenennamen.
- **Nach unten zusammenführen**: Klicken Sie mit der rechten Maustaste → **Nach unten zusammenführen**, um es mit der darunter liegenden Ebene zu kombinieren.
- **Bild reduzieren**: **Bild** → **Bild reduzieren**, um alle sichtbaren Ebenen zusammenzuführen.

## Ebeneneigenschaften

- **Deckkraft**: 0–100 %, steuert die Gesamttransparenz der Ebene.
- **Mischmodus**: Dropdown-Menü zur Auswahl, wie die Ebene mit den darunter liegenden Ebenen kombiniert wird.
- **Sichtbar/Ausgeblendet**: Das Augensymbol schaltet die Sichtbarkeit der Ebene um.

## Ebenensperren

Schlosssymbole werden in der Kopfzeile des Ebenenbedienfelds angezeigt. Jede Sperre kann unabhängig umgeschaltet werden. Wenn Sie mit der rechten Maustaste auf ein Schlosssymbol klicken, wird es exklusiv festgelegt (es wird nur dieser Typ gesperrt, alle anderen auf derselben Ebene werden entsperrt).

- **Alpha sperren**: Verhindert das Malen auf transparenten Bereichen. Pinselstriche wirken sich nur auf Pixel aus, die bereits über Deckkraft verfügen. Vollständig transparente Pixel werden nicht verändert. Nützlich zum Malen innerhalb vorhandener Formen, ohne dass etwas darüber hinausläuft.

- **Maske sperren**: Verhindert das Bearbeiten der Ebenenmaske. Die Maske bleibt sichtbar und aktiv, kann jedoch nicht übermalt oder geändert werden, während diese Sperre aktiviert ist.

- **Farbe sperren**: Sperrt das Malen auf eine bestimmte Farbe – die aktuelle Vordergrundfarbe zum Zeitpunkt der Anwendung der Sperre. Nachfolgende Striche auf dieser Ebene verwenden diese gespeicherte Farbe unabhängig von der aktiven Vordergrundfarbe. Beim Entsperren wird die gespeicherte Farbe verworfen.

- **Inhalt sperren** (Pixel sperren): Verhindert alle Pixeländerungen an der Ebene. Die Ebene kann nicht übermalt, gefüllt, transformiert oder anderweitig geändert werden. Nützlich zum Schutz fertiger Schichten.

- **Position sperren**: Verhindert, dass die Ebene verschoben oder transformiert wird. Die Ebene kann weiterhin bearbeitet werden; nur Positionsänderungen (Werkzeug verschieben, Werkzeug transformieren) werden blockiert.

- **Sichtbarkeit sperren**: Verhindert, dass das Augensymbol die Sichtbarkeit der Ebene umschaltet. Schützt Ebenen, die während der Bearbeitung immer sichtbar (oder ausgeblendet) bleiben sollen.

Alle Sperren werden mit dem Projekt gespeichert und bleiben sitzungsübergreifend bestehen.

## Ebeneneffekte (fx)

Zerstörungsfreie GEGL-Filter, die über das Menü **Filter** angewendet werden, werden als festgeschriebene Effekte auf der Ebene gespeichert, anstatt Pixel sofort zu ändern. Wenn eine Ebene mindestens einen festgeschriebenen Effekt hat, wird im Ebenenbedienfeld neben dieser Ebene ein **fx**-Symbol angezeigt.### Zugriff auf das Effekt-Popup

Klicken Sie auf das **fx**-Symbol in einer Ebenenzeile im Ebenenbedienfeld, um das Popover **Ebeneneffekte** für diese Ebene zu öffnen.

Das Popover zeigt den Filterstapel für die Ebene an – jeder festgeschriebene Effekt wird nach Namen aufgelistet, mit einem Sichtbarkeitsschalter daneben.

### Steuerelemente

- **Sichtbarkeits-Augenumschaltung** (oben im Popup): Schaltet alle Effekte gleichzeitig ein oder aus.
- **Sichtbarkeitsumschaltung pro Filter**: Jede Filterzeile verfügt über ein eigenes Augensymbol, um diesen Effekt unabhängig zu aktivieren oder zu deaktivieren.
- **Bearbeiten**: Öffnet den Einstellungsdialog für den ausgewählten Filter und ermöglicht die zerstörungsfreie Anpassung seiner Parameter.
- **Erhöhen / Senken**: Verschiebt den ausgewählten Filter im Stapel nach oben oder unten und ändert die Reihenfolge, in der die Effekte angewendet werden.
- **Zusammenführen**: Überträgt alle aktuell sichtbaren Effekte auf die Pixel der Ebene und macht die Änderungen dauerhaft. Das FX-Symbol wird entfernt, wenn alle Effekte zusammengeführt werden. Zusammenführen ist auf Gruppenebenen nicht verfügbar.
- **Entfernen**: Löscht den ausgewählten Filter vollständig. Das Popover wird automatisch geschlossen, wenn keine Effekte mehr vorhanden sind.

Durch Doppelklicken auf einen Filter in der Liste wird auch dessen Bearbeitungsdialog geöffnet.

**Bearbeiten** und **Entfernen** sind blockiert, wenn „Pixel sperren“ auf der Ebene aktiv ist. Filter können nicht neu angeordnet werden, während einer aktiv bearbeitet wird.

### Effekte hinzufügen

Wenden Sie einen Filter aus **Filter** → (beliebige Kategorie) an. Wenn die aktive Ebene als Ziel ausgewählt ist und der Vorgang zerstörungsfrei ausgeführt wird, wird das Ergebnis als Ebeneneffekt gespeichert und nicht in die Pixeldaten integriert. Das FX-Symbol erscheint auf der Ebene, wenn mindestens ein Effekt vorhanden ist.

## Dialogfeld „Ebenenattribute“.

Doppelklicken Sie im Ebenenbedienfeld auf eine Ebene, um das Dialogfeld „Ebenenattribute“ zu öffnen.

### Identität

- **Farbtag**: Farbetikett für die visuelle Organisation im Ebenenbedienfeld.

### Zusammengesetzter Raum und Modus

- **Zusammengesetzter Raum**: Der Farbraum, der beim Zusammensetzen dieser Ebene mit den darunter liegenden Ebenen verwendet wird. Optionen: Auto, Linear (RGB), Wahrnehmung (RGB).
- **Zusammengesetzter Modus**: Steuert, wie das Ebenen-Alpha mit dem Hintergrund interagiert. Zu den Optionen gehören „Vereinigung“ (betrifft alle Bereiche – die Standardeinstellung für den Normalmodus), „Auf Hintergrund ausschneiden“ (betrifft nur Bereiche mit vorhandenem Inhalt – die Standardeinstellung für die meisten anderen Mischmodi) und „Schnittpunkt“.

### Größe und Offsets

Für eine vorhandene Ebene zeigt **Größen** die Ebenenabmessungen und Maskenabmessungen (sofern eine Maske angehängt ist) als schreibgeschützte Beschriftungen an.

**Ebenenversätze** – X- und Y-Drehregler, die die Position der Ebene auf der Leinwand steuern. Änderungen werden sofort wirksam und nicht erst beim Schließen des Dialogs.

Wenn die Ebene eine Maske hat, werden unten **Maskenversätze** – X- und Y-Drehregler für die unabhängige Position der Maske – angezeigt.

Beim Erstellen einer neuen Ebene ersetzen die Felder „Breite“ und „Höhe“ sowie ein Dropdown-Menü „Füllen mit“ (Vordergrund, Hintergrund, Weiß, Transparent) die schreibgeschützte Größenanzeige.

### Layer-Attribute (Persistente Parasiten)

Der untere Abschnitt des Dialogs enthält eine scrollbare Namens-/Werttabelle für persistente Parasiten – beliebige Schlüsselwert-Metadaten, die an die Ebene angehängt sind. Diese Werte werden mit dem Projekt gespeichert und sind über die Scheme-Skriptschnittstelle zugänglich.

- Klicken Sie auf eine beliebige Zelle in der Spalte „Name“ oder „Wert“, um sie direkt zu bearbeiten.
- **Hinzufügen**: Fügt eine neue leere Zeile hinzu.
- **Löschen**: Entfernt die ausgewählte Zeile und ihren Parasiten aus der Ebene.

Wenn die Schicht keine hartnäckigen Parasiten aufweist, werden drei leere Starterreihen angezeigt.

### InhaltsstatusEine schreibgeschützte Infozeile unten zeigt den aktuellen Inhaltsstatus der Ebene (und der Maske, falls vorhanden): **Klar**, **Einheitlich** oder **Gemischt**. Ein `*`-Präfix gibt an, dass die Ebene seit dem letzten Speichern nicht gespeicherte Änderungen aufweist.

## Leistung

- **Schnellmodus**: Beim Malen auf einer einzelnen Ebene, die in einer Gruppe verschachtelt ist, schaltet Lumi die Vorfahrengruppen für die Dauer des Strichs vorübergehend auf Pass-Through-Rendering um und überspringt die Neuzusammenstellung der vollständigen Gruppenprojektion. Dadurch wird eine Verzögerung bei der Aktualisierung verschachtelter Projektionen beim Einfärben und Malen vermieden. Das vollständige Compositing wird fortgesetzt, wenn der Strich endet, die aktive Ebene wechselt oder bevor ein Speichervorgang durchgeführt wird.

  Der Schnellmodus ist deaktiviert, wenn eine der folgenden Bedingungen auf eine Vorfahrengruppe zutrifft:
  - Die Gruppe verfügt über sichtbare zerstörungsfreie Filter (Filter benötigen den Projektionspuffer).
  – Der Mischmodus der Gruppe ist etwas anderes als **Normal** oder **Durchgang**.
  - Die Gruppe verfügt über ein direktes untergeordnetes Element, das den zusammengesetzten Modus **Auf Hintergrund ausschneiden** oder **Schnittpunkt** verwendet (diese erfordern Hintergrunddaten aus dem Projektionspuffer).

  Der Schnellmodus wird auch nicht für Ebenen der obersten Ebene, schwebende Auswahlen oder wenn mehrere Ebenen gleichzeitig als Ziel ausgewählt werden, aktiviert.

  Durch die Strukturierung von Dateien, um diese Bedingungen in Malgruppen zu vermeiden, indem Sie die Mischmodi „Normal“ auf Ebenen verwenden, wird sichergestellt, dass der Schnellmodus während einer Farb- oder Malsitzung aktiv bleibt.
- **Lazy Loading**: Große Projekte werden schnell geladen; Ebenendaten werden nur bei Bedarf geladen (z. B. wenn sie sichtbar gemacht oder aufgemalt werden).

## Dateiformat

Alle Ebenen, Masken und Eigenschaften werden im offenen `.lum`-Format von Lumi gespeichert. Bei der Datei handelt es sich um ein Verzeichnis, das einzelne Layer-Puffer und Metadaten enthält und so Kompatibilität und langfristige Zugänglichkeit gewährleistet.