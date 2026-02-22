---
title: "Spektrale Farbmischung"
type: docs
---
Das Palettensystem von Lumi verwendet ein Spektralfarbmodell, um zu simulieren, wie sich echte Pigmente mischen. Das Ziel besteht darin, das Erstellen und Auswählen von Farben aus einer digitalen Palette so zu gestalten, als würden sie physische Farben mischen. Sobald eine Farbe auf die Leinwand angewendet wird, handelt es sich um Standard-RGB.

## Was Spektralmischung bedeutet

Die herkömmliche RGB-Mischung ist additiv: Durch die Mischung zweier RGB-Werte werden diese in Richtung eines Mittelpunkts gemittelt. Das Mischen von Pigmenten ist subtraktiv: Jedes Pigment absorbiert bestimmte Wellenlängen, und ihre kombinierte Wirkung ist dunkler und verändert häufig den Farbton.

Lumi modelliert dies mithilfe einer 10-Band-Spektralreflexionsdarstellung für Palettenfarben anstelle von RGB.

Dies führt zu lackähnlichen Ergebnissen: Durch die Mischung von Blau und Gelb entsteht Grün, nicht Grau. Durch das Mischen zweier gesättigter Farben entsteht eine Farbe, die sich in Richtung Neutral verschiebt, wie dies bei physikalischen Pigmenten der Fall ist.

Die Spektralberechnung wird während der Palettenkonstruktion, beim Generieren sekundärer und tertiärer Paletteneinträge und beim Mischen zweier übergeordneter Farben durch den Palettenmischer ausgeführt. Die resultierende Farbe wird zur Anzeige und zum Malen in lineares RGB umgewandelt.

## Pigmentprofile

Paletteneinträge können auf echten Pigmentdaten unter Verwendung von **Color Index (CI)-Codes** basieren. Jede CI-Pigmentfamilie weist eine charakteristische spektrale Tendenz auf, die sich auf die Mischung auswirkt.

| Pigmentrolle | Mischverhalten | Beispiel |
| :--- | :--- | :--- |
| **Primär** | Hohe Chroma, saubere Sekundärteile | PY3 (Zitronengelb), PR122 (Magenta) |
| **Körper** | Deckender, kräftiger Massenton, der in grünen Mischungen ins Oliv übergeht | PY35 (Cadmiumgelb), PR108 (Cadmiumrot) |
| **Neutralisator** | Entsättigt schnell und schaltet stumm | PBk11 (Marsschwarz), PBr7 (Sienna) |
| **Chroma-Anker** | Hohe Farbstärke, dominiert Mischungen | PB29 (Ultramarinblau), PG7 (Phthalogrün) |

Durch das Hinzufügen von Primärfarben mit CI-Codes zu einer Palette erhält die Misch-Engine eine genaue spektrale Ausrichtung für diese Farben, sodass erzeugte Sekundär- und Tertiärmischungen das reale Mischverhalten widerspiegeln.

## Lumi-Pigmente

Die Master-Palette wird mit den folgenden Pigmenten geliefert. Die Muster zeigen das typische Volltonbild jedes Pigments (volle Stärke, unverdünnt).

### Orangen und Gelbtöne

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Pyrrolorange | PO73 | Rot (Scharlachrot) |
| {{< swatch "243,114,64" >}} | Cadmiumorange | PO20 | Gelb (Körper) |
| {{< swatch "240,180,80" >}} | Cadmiumgelb | PY35 | Gelb (Körper) |
| {{< swatch "245,210,25" >}} | Kadmiumgelb blass | PY35:Blass | Gelb (Cadmiumblass) |
| {{< swatch "250,230,5" >}} | Zitronengelb | PY3 | Gelb (Zitrone) |
| {{< swatch "225,155,10" >}} | Nickel-Azo-Gelb | PY150 | Gelb (Mittel) |
| {{< swatch "180,175,45" >}} | Grünes Gold | PY129 | Gelbgrün (Gold) |

### Erdfarben

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Gebrannte Siena | PBr7:Verbrannt | Erde (Rotbraun) |
| {{< swatch "117,66,0" >}} | Gebrannter Umber | PBr7:Umber | Erde (Neutral) |
| {{< swatch "205,68,35" >}} | Rohe Siena | PBr7:Raw | Erde (Gelbbraun) |
| {{< swatch "187,124,25" >}} | Gelber Ocker | PY42 | Erde (Gelb) |

### Grüne

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Phthalogrün (YS) | PG36 | Grün (Phthalo-Gelbton) |
| {{< swatch "64,130,109" >}} | Viridian | PG18 | Grün (Viridian) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Grün (Erde kühl) |
| {{< swatch "0,110,100" >}} | Winsor Green (BS) | PG7 | Grün (Phthalo-Blauton) |

### Blues und Cyan

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Kobalt-Türkis-Licht | PG50 | Cyan (Mineral) |
| {{< swatch "0,148,214" >}} | Himmelblau | PB35 | Cyan (Mineral) |
| {{< swatch "0,100,110" >}} | Phthalo-Türkis | PB16 | Blau (Phthalo) |
| {{< swatch "0,123,194" >}} | Kobaltblau | PB28 | Blau (Violett-Mager) |
| {{< swatch "0,75,115" >}} | Winsorblau | PB15 | Blau (Phthalo) |
| {{< swatch "27,63,148" >}} | Ultramarin | PB29 | Blau (Violett-Mager) |

### Veilchen, Magentas und Rottöne

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Brillantes Violett | PV23 | Violett (Dioxazin) |
| {{< swatch "230,90,180" >}} | Permanente Rose | PV19:Rose | Magenta (Chinacridon) |
| {{< swatch "190,40,120" >}} | Chinacridon Magenta | PV19:Magenta | Magenta (Chinacridon) |
| {{< swatch "160,30,65" >}} | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Chinacridon) |
| {{< swatch "120,35,65" >}} | Perylenviolett | PV29 | Magenta (Chinacridon) |
| {{< swatch "135,10,45" >}} | Perylene Kastanienbraun | PR179 | Rot (Karmesinrot) |
| {{< swatch "215,30,60" >}} | Pyrrolrot | PR254 | Rot (Scharlachrot) |
| {{< swatch "225,55,65" >}} | Pyrrol Rotlicht | PR255 | Rot (Pyrrollicht) |

### Schwarze und Weiße

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Marsschwarz (warm) | PBk11 | Schwarz (Mars) |
| {{< swatch "18,28,12" >}} | Perylengrün | PBk31 | Schwarz (Perylengrün) |
| {{< swatch "10,18,19" >}} | Elfenbeinschwarz (Cool) | PBk9 | Schwarz (Elfenbein) |
| {{< swatch "18,18,18" >}} | Lampenschwarz (Neutral) | PBk7 | Schwarz (Lampe) |
| {{< swatch "255,249,235" >}} | Titanweiß (Warm) | PW6:Warm | Weiß (Titanium Warm) |
| {{< swatch "255,255,255" >}} | Titanweiß (Neutral) | PW6 | Weiß (Titan Neutral) |
| {{< swatch "245,250,255" >}} | Zinkweiß (Kühl) | PW4 | Weiß (Zink Cool) |

### Grautöne kontrollieren

Kontrollgrautöne sind standardisierte Neutralisatoren, die zur vorhersehbaren Entsättigung von Mischungen verwendet werden.

| Muster | Name | CI-Code |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Warmes Grau | N_WARM |
| {{< swatch "128,128,128" >}} | Neutrales Grau | N_NEUTRAL |
| {{< swatch "120,128,135" >}} | Kühles Grau | N_COOL |

## Die Palettenkarte

Die Palettenkarte visualisiert die aktive Palette als Farbtonrad: 36 Farbtonsektoren (10°-Schritte) × 15 Helligkeitszellen. Wenn Primärstoffe hinzugefügt werden, generiert das System Sekundär- und Tertiärmischungen und platziert sie an den entsprechenden Kartenpositionen.

Durch Klicken auf eine Zelle wird eine Farbe als Vordergrund ausgewählt. Durch Klicken bei gedrückter Umschalttaste wird es als übergeordneter Endpunkt im Palettenmixer zugewiesen.

## Der Palettenmischer

Der Palettenmischer leitet neue Farben aus zwei übergeordneten Einträgen mithilfe einer festen dreistufigen Pipeline ab:

1. **Mischung**: Spektrale WGM zwischen Elternteil A (CCW) und Elternteil B (CW).
2. **Chroma**: In Richtung des neutralen Spektrums der Palette mischen, um die Sättigung zu reduzieren.
3. **Ton**: Zum Mischen von Weiß oder Schwarz mischen und dabei die Helligkeit anpassen.

Der Farbton wird zuletzt angewendet, damit Helligkeitsanpassungen nicht durch Chroma-Änderungen verwässert werden. Die Steuerelemente „Value Lock“ und „Band Clamp“ beschränken die Ergebnisse auf eine bestimmte Helligkeitsstufe oder ein bestimmtes Werteband.

Gemischte Farben können als **benutzerdefinierte** Einträge in der Palette gespeichert werden, wobei das vollständige Rezept (übergeordnete UIDs, Mischfaktor, Farbton, Chroma-Werte) zur späteren Wiederherstellung gespeichert wird.

## Canvas-Pixel sind RGB

Das Spektralsystem arbeitet vollständig innerhalb der Palettenkonstruktion und Farbauswahl. Wenn ein Pinselstrich angewendet wird, wird die Vordergrundfarbe (bereits in lineares RGB konvertiert) gemalt. Die Leinwand speichert Standard-RGB-Pixeldaten.Die spektrale Mischung verbessert die Erfahrung beim Aufbau einer Palette und der Auswahl von Farben im Einklang mit dem physikalischen Pigmentverhalten, ohne die Art und Weise zu ändern, wie Bilddaten gespeichert oder zusammengesetzt werden.