---
title: "Palettenkarte"
type: docs
---
Die Palettenkarte beantwortet eine praktische Frage für Maler: Welche Farben können angesichts einer Reihe von Pigmenten tatsächlich daraus gemischt werden? Ausgehend von den Eingabepigmenten der Palette untersucht es prozedural jede Kombination (Zwei-Pigment-Mischungen, Drei-Wege-Mischungen, Tonvariationen) und ordnet die Ergebnisse einem Farbkreis zu. Die Ausgabe ist ein Bild des erreichbaren Farbraums für diesen bestimmten Pigmentsatz.

Die Karte ist auch ein koordinatenbasiertes Navigationstool. Es organisiert jede generierte Mischung nach Farbton und Helligkeit in einem kreisförmigen Raster, sodass die gesamte Palette auf einen Blick lesbar ist und jede Farbe eine stabile Heimatadresse hat.

## Gitterstruktur

Die Karte ist in ein 36 × 15-Raster unterteilt:

- **36 Farbtonsektoren**: 10°-Schritte um das Rad, zentriert auf die wichtigsten Farbtonnamen.
- **15 Helligkeitszellen**: 3 Zellen pro Werteband × 5 Bänder (High Key, Upper Mid, Middle, Lower Mid, Deep), von Weiß außen bis Schwarz in der Mitte.

Jede Zelle ist ein kleiner Keil am Rad. Ein in einer Zelle platzierter Eintrag soll diese Zelle als **Ursprung** haben: seine logische Heimatadresse auf der Karte.

## Farben in Zellen

Wenn mehrere Farben um dieselbe Zelle konkurrieren, wird nur ein **Gewinner** prominent angezeigt:

1. **Primäre** Einträge gewinnen immer ihre Zelle, unabhängig von anderen Bewohnern.
2. Wenn keine Primärquelle vorhanden ist, gewinnt der generierte Mix (Sekundär oder Tertiär) mit der **höchsten Chroma**.

Beiträge, die nicht gewinnen, werden Zweitplatzierte und bleiben über Click-Cycling zugänglich (siehe unten).

Benutzerdefinierte Einträge (gespeicherte Mischungen) werden als quadratische Punkte dargestellt. Erzeugte Mischungen und Primärfarben werden als runde Punkte dargestellt.

## Klicken Sie auf Radfahren

Durch Klicken auf eine besetzte Zelle wird der Gewinner als Vordergrundfarbe ausgewählt. Durch erneutes Klicken auf dieselbe Zelle wird zum nächsten Bewohner gewechselt (vom Zweitplatzierten generierte Mischungen, dann alle benutzerdefinierten Einträge, die an dieser Rasteradresse gespeichert sind). Jeder Klick rückt einen Schritt durch den Stapel vor.

**Linksklick** führt in den Vordergrund. Wenn das Farbziel auf „Hintergrund“ eingestellt ist (aus der Toolbox), werden Klicks stattdessen auf „Hintergrund“ umgeleitet.

## Umschalt-Auswahl: Mixer-Endpunkte laden

Halten Sie **Shift** gedrückt, um in den Endpunkt-Lademodus zu wechseln:

- **Linksklick** weist den angeklickten Eintrag im Palettenmischer als **Übergeordnetes Element A (CCW)** zu.
- **Rechtsklick** weist es als **Übergeordnetes B (CW)** zu.

In diesem Modus sind nur Einträge der Klasse A (Primäre und benutzerdefinierte Mischungen mit intakter Herkunft) auswählbar. Tertiäre sind ausgeblendet und Nicht-Klasse-A-Punkte sind abgeblendet. Eine kurze Einblendung bestätigt, dass der Modus aktiv ist.

## Mixer Parent-Highlights

Wenn der Palettenmixer über aktive Endpunkte „Parent A“ und „Parent B“ verfügt, werden beide auf der Karte mit **Diamantringen** (einer Rautenform mit schwarzem Rand) markiert. Diese Hervorhebungen bleiben auch dann sichtbar, wenn andere Anzeigeelemente umgeschaltet werden, sodass die aktiven übergeordneten Elemente der Mischung immer erkennbar sind.

## Ursprung vs. visuelle Position

Jeder Eintrag hat zwei Positionen auf der Karte:

- **Ursprung (Quellzelle)**: Die logische Gitteradresse, zu der der Eintrag gehört, für die gesamte Lebensdauer festgelegt.
- **Visuelle Punktposition**: Wo die Farbe tatsächlich basierend auf ihrem wahrgenommenen Farbton und ihrer Helligkeit wiedergegeben wird.

Mit **Best-Match Relocation** berechnet das System beim Speichern einer Mischung das optimale Rezept für die endgültige Farbe und stellt den Ursprung so ein, dass er mit der visuellen Position der Farbe übereinstimmt. Dadurch bleiben gespeicherte Farben nahe an ihrer visuellen Position auf dem Rad und die Karte wird räumlich kohärent.

## Gespeicherte Mischungen ziehen

Benutzerdefinierte Einträge (gespeicherte Mischungen) können durch Ziehen neu positioniert werden:1. Klicken Sie auf einen benutzerdefinierten Eintrag (quadratischer Punkt), halten Sie die Maustaste gedrückt und ziehen Sie über den 5-Pixel-Schwellenwert hinaus.
2. Der Cursor ändert sich, um den Ziehmodus anzuzeigen. Die übergeordneten Hervorhebungen werden live aktualisiert, während Sie sich über die Karte bewegen, um die neuen Blend-Eltern an jeder Kandidatenposition anzuzeigen.
3. Der gezogene Punkt rastet an der nächstgelegenen gültigen Probenposition ein.
4. Zum Festschreiben loslassen. Der Eintrag übernimmt das Rezept der Zielzelle: Ihre Eltern, Mischung, Ton und Chroma werden entsprechend aktualisiert, und ihr Ursprung wird aktualisiert, um mit der neuen visuellen Position übereinzustimmen.

Ziehbewegungen können über **Bearbeiten → Rückgängig** rückgängig gemacht werden.

## Doppelklick: Umschalten des Kartenarbeitsbereichs

Wenn Sie im **Paletteneditor** auf einen beliebigen Paletteneintrag doppelklicken, wird die Arbeitsbereichsansicht „Palettenübersicht“ ein- und ausgeschaltet. Dies ist eine schnelle Möglichkeit, ohne Verwendung eines Menüs zwischen dem Durchsuchen gespeicherter Farben und dem Mischen auf der Karte zu wechseln. Das Einzelklickverhalten (Wiederherstellen des Rezepts des Eintrags im Mixer) bleibt davon unberührt.

## Leinwandüberlagerung

Die Palettenkarte kann als Vollbild-Overlay direkt auf die Bildleinwand geladen werden, indem Sie in der Toolbox auf das **Vordergrund-/Hintergrund-Farbfeld** klicken. Dadurch entsteht eine große Mischfläche, ohne dass der Karte ein permanentes Bedienfeld zugewiesen werden muss.

## Zentrales Farbfeld

Ein kreisförmiges Farbfeld befindet sich in der Mitte des Donut-Lochs und spiegelt die Farbe der Zelle wider, über der sich der Cursor befindet:

- **Hover-Farbe**: Wenn der Cursor auf einem Karteneintrag ruht, wird das Farbfeld sofort aktualisiert und zeigt die Farbe dieses Eintrags an.
- **Ausgewählte Farbe als Fallback**: Wenn sich keine Zelle bewegt, zeigt das Farbfeld das berechnete Ergebnis des Palettenmixers für den aktuell ausgewählten Eintrag an. Wenn der Mixer noch nicht aufgelöst wurde, verwendet er die Grundanzeigefarbe des Eintrags, sodass die Stelle nie leer wird.
- Ein dünner dunkler Rand umrandet das Farbfeld jederzeit.
- Nachdem der Cursor kurz über dem zentralen Farbfeld verweilt, erscheint ein weiß-schwarzer äußerer Ring, um zu signalisieren, dass der Bereich interaktiv ist.
- **Durch Klicken auf das mittlere Farbfeld** wird die Leinwandüberlagerung geschlossen und zur normalen Bildansicht zurückgekehrt (dasselbe wie beim Klicken außerhalb des äußeren Rings).

## Alt-Taste: Canvas-Vergleichsmodus

Wenn die Palettenkarten-Überlagerung geöffnet ist, wird durch Gedrückthalten von **Alt** vorübergehend das Bild darunter angezeigt:

- Die gesamte Benutzeroberfläche der Palettenzuordnung wird unsichtbar (ihre Deckkraft sinkt auf Null) und gibt die Leinwand frei.
- Dem Cursor folgt ein kreisförmiges 64-Pixel-Farbfeld, das mit der aktuell vom Palettenmischer aufgenommenen Farbe gefüllt ist, sodass Sie beim Betrachten des Bildes über die aktive Mischung informiert bleiben.
- Durch Loslassen der Alt-Taste wird die Palettenkarte mit voller Deckkraft wiederhergestellt.

Als Erinnerung wird in der Arbeitsbereichsansicht der Hinweis „Halten Sie die Alt-Taste gedrückt, um das Bild anzuzeigen“* angezeigt.