---
title: "Palettenmischer"
type: docs
---
Der Palettenmischer leitet mithilfe einer festen dreistufigen Pipeline neue Farben aus Paaren von Paletteneinträgen ab. Da das Mischen im Spektralbereich und nicht im RGB-Bereich erfolgt, verhalten sich die Ergebnisse wie physikalische Pigmente: Blau und Gelb ergeben Grün, gesättigte Farben verschieben sich beim Mischen in Richtung Neutral.

## Die Pipeline

Jede vom Mixer erzeugte Farbe durchläuft in einer festen Reihenfolge drei Stufen:

1. **Mischung**: Spektrale WGM zwischen Elternteil A (CCW) und Elternteil B (CW).
2. **Chroma**: In Richtung des neutralen Spektrums der Palette mischen, um die Sättigung zu reduzieren.
3. **Ton**: Zum Mischen von Weiß (Tönung) oder Schwarz (Farbton) mischen.

Der Ton wird immer zuletzt angewendet. Dadurch wird die Helligkeit dominant: Eine Tonwertanpassung erreicht genau den beabsichtigten Helligkeitswert, ohne dass sie durch die vorangehende Chroma-Anpassung verwässert wird.

## Eltern auswählen

Übergeordnetes A und übergeordnetes B sind die beiden Einträge, zwischen denen der Blend-Schieberegler mischt. Sie werden aus der Palettenkarte geladen:

- Halten Sie **Shift** auf der Palettenzuordnung gedrückt und **klicken Sie**, um Parent A (CCW) festzulegen.
- Halten Sie **Shift** gedrückt und **klicken Sie mit der rechten Maustaste**, um Parent B (CW) einzustellen.

Als Eltern werden nur Einträge der **Klasse A** (Primär- und Sondermischungen mit intakter Herkunft) akzeptiert. Tertiäre und Einträge mit verlorener Abstammung sind ausgeschlossen.

Die Positionen „Parent A“ und „Parent B“ des Mixers werden auf der Karte als **Diamantring**-Hervorhebungen angezeigt, sodass Sie immer sehen können, welche Einträge geladen sind.

## Die Slider

| Schieberegler | Wirkung |
| :--- | :--- |
| **Mischung** | Bewegt sich zwischen Parent A (CCW-Ende) und Parent B (CW-Ende). Bei 0,0 stimmt das Ergebnis mit Parent A überein; Bei 1,0 stimmt es mit Parent B überein. |
| **Chroma** | Entsättigt die Mischung in Richtung Neutral der Palette. Höhere Werte führen zu gedämpfteren, erdigeren Ergebnissen. |
| **Ton** | Verschiebt die Helligkeit in Richtung der Mischung von Weiß (Tönungsrichtung) oder der Mischung von Schwarz (Richtung des Farbtons). |

## Wertkontrollen

**Value Lock** friert die Wahrnehmungshelligkeit (CIE L\*) auf ihrem aktuellen Niveau ein, während sich die anderen Schieberegler bewegen. Verwenden Sie dies, um Chroma- oder Farbtonvariationen zu erkunden, ohne den Wert einer Mischung zu ändern.

**Band Clamp** begrenzt das Ergebnis so, dass es innerhalb der Grenzen seines aktuellen Wertebands bleibt (z. B. innerhalb der unteren Mitte). Der Tonregler lässt sich weiterhin verschieben, aber die Ausgabehelligkeit ist begrenzt.

Der Tone-Schieberegler spiegelt auch alle im Paletten-Editor konfigurierten Wertelücken wider. Helligkeitsbereiche, die in eine Lücke fallen, werden als halbtransparente graue Bänder auf dem Schieberegler angezeigt. Der Schieberegler springt automatisch über diese Lücken: Wenn Sie durch einen grauen Bereich ziehen, springen Sie zur nächsten gültigen Bandgrenze auf der anderen Seite.

## Endpunkte mischen (Weiß, Schwarz, Neutral)

Für die Ton- und Chroma-Stufen sind Referenzendpunkte erforderlich: ein Mischweiß, ein Mischschwarz und ein Neutralton. Lumi erkennt diese automatisch, indem es die aktive Palette nach den besten Kandidaten durchsucht:

- **Mischen von Weiß**: Primärfarbe mit dem höchsten Chroma-Wert, die reinem Weiß am nächsten kommt.
- **Mischen von Schwarz**: Primärfarbe mit der geringsten Helligkeit.
- **Neutral**: die Primärfarbe, die der Achromatik am nächsten kommt (geringste Chroma).

Diese können manuell überschrieben werden, indem Sie mit der rechten Maustaste auf einen Eintrag im Paletteneditor klicken.

## Einen Mix speichernKlicken Sie auf **Zur Palette hinzufügen**, um das aktuelle Mischergebnis als **Gespeicherte Mischung** (benutzerdefinierter Eintrag) zu speichern. Vor dem Speichern wendet das System die **Best-Match-Verlagerung** an: Es durchsucht die Palette nach dem optimalen Rezept, das dieselbe Endfarbe mit der besten räumlichen Anpassung an die Palettenkarte erzeugt. Wenn ein näheres Rezept gefunden wird, springen die Mixer-Schieberegler, um es anzuzeigen. Dies bestätigt, dass das System einen besseren Ursprung gefunden hat, und die Position des gespeicherten Eintrags wird mit seinem visuellen Punkt auf der Karte ausgerichtet.

Gespeicherte Mischungen speichern ihr vollständiges Rezept (übergeordnete A/B-UIDs, Mischfaktor, Ton, Chroma), sodass sie exakt reproduziert werden können.

## Rezeptwiederherstellung

Durch einmaliges Klicken auf einen benutzerdefinierten Eintrag im Paletteneditor wird das Rezept dieses Eintrags im Mixer wiederhergestellt:

- Parent A und Parent B werden neu geladen.
- Die Schieberegler für Mischung, Ton und Chroma kehren in ihre ursprüngliche Position zurück.
- Jede Wertesperre oder Bandklemme, die während der Erstellung aktiv war, wird wieder aktiviert.

Dies macht es einfacher, zu einer Farbe zurückzukehren und sie weiter anzupassen oder sie als Ausgangspunkt für eine neue Mischung zu verwenden.