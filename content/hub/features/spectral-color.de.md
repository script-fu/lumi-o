---
title: "Spektrale Farbmischung"
type: docs
---
Das Palettensystem von Lumi verwendet ein Spektralfarbmodell, um zu simulieren, wie sich echte Pigmente mischen. Das Ziel besteht darin, das Erstellen und Auswählen von Farben aus einer digitalen Palette so zu gestalten, als würden sie physische Farben mischen. Sobald eine Farbe auf die Leinwand angewendet wird, handelt es sich um Standard-RGB.

## Was Spektralmischung bedeutet

Die herkömmliche RGB-Mischung ist additiv: Durch die Mischung zweier RGB-Werte werden diese in Richtung eines Mittelpunkts gemittelt. Das Mischen von Pigmenten ist subtraktiv: Jedes Pigment absorbiert bestimmte Wellenlängen, und ihre kombinierte Wirkung ist dunkler und verändert häufig den Farbton.

Lumi modelliert dies mithilfe einer 10-Band-Spektralreflexionsdarstellung für Palettenfarben anstelle von RGB.

Dies führt zu lackähnlichen Ergebnissen: Durch die Mischung von Blau und Gelb entsteht Grün, nicht Grau. Durch das Mischen zweier gesättigter Farben entsteht eine Farbe, die sich in Richtung Neutral verschiebt, wie dies bei physikalischen Pigmenten der Fall ist.

Die Spektralberechnung wird während der Palettenkonstruktion ausgeführt – beim Generieren sekundärer und tertiärer Paletteneinträge und wenn der Palettenmischer zwei übergeordnete Farben mischt. Die resultierende Farbe wird zur Anzeige und zum Malen in lineares RGB umgewandelt.

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

### Orangen und Gelbtöne| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrolorange | PO73 | Rot (Scharlachrot) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cadmiumorange | PO20 | Gelb (Körper) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cadmiumgelb | PY35 | Gelb (Körper) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kadmiumgelb blass | PY35:Blass | Gelb (Cadmiumblass) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zitronengelb | PY3 | Gelb (Zitrone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Nickel-Azo-Gelb | PY150 | Gelb (Mittel) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Grünes Gold | PY129 | Gelbgrün (Gold) |

### Erdfarben

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gebrannte Siena | PBr7:Verbrannt | Erde (Rotbraun) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gebrannter Umber | PBr7:Umber | Erde (Neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rohe Siena | PBr7:Raw | Erde (Gelbbraun) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gelber Ocker | PY42 | Erde (Gelb) |

### Grüne

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalogrün (YS) | PG36 | Grün (Phthalo-Gelbton) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridian | PG18 | Grün (Viridian) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Grün (Erde kühl) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Green (BS) | PG7 | Grün (Phthalo-Blauton) |### Blues und Cyan

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kobalt-Türkis-Licht | PG50 | Cyan (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Himmelblau | PB35 | Cyan (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo-Türkis | PB16 | Blau (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kobaltblau | PB28 | Blau (Violett-Mager) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsorblau | PB15 | Blau (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramarin | PB29 | Blau (Violett-Mager) |

### Veilchen, Magentas und Rottöne

| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Brillantes Violett | PV23 | Violett (Dioxazin) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanente Rose | PV19:Rose | Magenta (Chinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Chinacridon Magenta | PV19:Magenta | Magenta (Chinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Chinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylenviolett | PV29 | Magenta (Chinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Kastanienbraun | PR179 | Rot (Karmesinrot) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrolrot | PR254 | Rot (Scharlachrot) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrol Rotlicht | PR255 | Rot (Pyrrollicht) |

### Schwarze und Weiße| Muster | Name | CI-Code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Marsschwarz (warm) | PBk11 | Schwarz (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylengrün | PBk31 | Schwarz (Perylengrün) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Elfenbeinschwarz (Cool) | PBk9 | Schwarz (Elfenbein) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lampenschwarz (Neutral) | PBk7 | Schwarz (Lampe) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanweiß (Warm) | PW6:Warm | Weiß (Titanium Warm) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanweiß (Neutral) | PW6 | Weiß (Titan Neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zinkweiß (Kühl) | PW4 | Weiß (Zink Cool) |

### Grautöne kontrollieren

Kontrollgrautöne sind standardisierte Neutralisatoren, die zur vorhersehbaren Entsättigung von Mischungen verwendet werden.

| Muster | Name | CI-Code |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Warmes Grau | N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Neutrales Grau | N_NEUTRAL |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kühles Grau | N_COOL |

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

## Canvas-Pixel sind RGBDas Spektralsystem arbeitet vollständig innerhalb der Palettenkonstruktion und Farbauswahl. Wenn ein Pinselstrich angewendet wird, wird die Vordergrundfarbe – bereits in lineares RGB konvertiert – gemalt. Die Leinwand speichert Standard-RGB-Pixeldaten.

Die spektrale Mischung verbessert die Erfahrung beim Aufbau einer Palette und der Auswahl von Farben im Einklang mit dem physikalischen Pigmentverhalten, ohne die Art und Weise zu ändern, wie Bilddaten gespeichert oder zusammengesetzt werden.