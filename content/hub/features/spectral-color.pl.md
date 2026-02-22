---
title: "Widmowe mieszanie kolorów"
type: docs
---
System palet Lumi wykorzystuje widmowy model kolorów do symulacji mieszania się prawdziwych pigmentów. Celem jest, aby doświadczenie budowania i wybierania kolorów z cyfrowej palety przypominało mieszanie fizycznych farb. Po nałożeniu koloru na płótno jest to standardowy kolor RGB.

## Co oznacza mieszanie widmowe

Tradycyjne mieszanie RGB ma charakter addytywny: zmieszanie dwóch wartości RGB uśrednia je w kierunku punktu środkowego. Mieszanie pigmentów ma charakter subtraktywny: każdy pigment pochłania określone długości fal, a ich łączny efekt jest ciemniejszy i często zmienia odcień.

Lumi modeluje to przy użyciu 10-pasmowej reprezentacji widmowego współczynnika odbicia dla palety kolorów, a nie RGB.

Daje to rezultaty przypominające farbę: zmieszanie koloru niebieskiego i żółtego daje kolor zielony, a nie szary. Mieszanie dwóch nasyconych kolorów daje kolor, który zmienia się w kierunku neutralnym, tak jak robią to pigmenty fizyczne.

Obliczenia widmowe są przeprowadzane podczas konstruowania palety, podczas generowania wpisów palety drugorzędnej i trzeciorzędnej oraz podczas mieszania dwóch kolorów nadrzędnych w programie Palette Mixer. Wynikowy kolor jest konwertowany do liniowego RGB na potrzeby wyświetlania i malowania.

## Profile pigmentów

Wpisy w palecie mogą opierać się na rzeczywistych danych dotyczących pigmentów przy użyciu **kodów indeksu kolorów (CI)**. Każda rodzina pigmentów CI ma charakterystyczne odchylenie widmowe, które wpływa na sposób mieszania.

| Rola pigmentu | Mieszanie zachowań | Przykład |
| :--- | :--- | :--- |
| **Podstawowy** | Wysokie nasycenie, czyste elementy wtórne | PY3 (żółty cytrynowy), PR122 (magenta) |
| **Ciało** | Kryjący, mocny ton masowy, w zielonych mieszankach przechodzi w oliwkowy | PY35 (żółcień kadmowa), PR108 (czerwień kadmowa) |
| **Neutralizator** | Szybko desaturacji i wyciszenia | PBk11 (Czarny Mars), PBr7 (Sienna) |
| **Kotwica Chroma** | Wysoka siła barwienia, dominuje w mieszaninach | PB29 (niebieski ultramarynowy), PG7 (zielony ftalowy) |

Dodanie kolorów podstawowych z kodami CI do palety zapewnia silnikowi miksującemu dokładne odchylenie widmowe dla tych kolorów, dzięki czemu wygenerowane mieszanki drugorzędne i trzeciorzędne odzwierciedlają zachowanie mieszania w świecie rzeczywistym.

## Pigmenty Lumi

Paleta Master zawiera następujące pigmenty. Próbki pokazują typowy wygląd kamienia kamiennego dla każdego pigmentu (o pełnej mocy, nierozcieńczony).

### Pomarańcze i żółcie

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Pirolowa Pomarańcza | PO73 | Czerwony (Szkarłatny) |
| {{< swatch "243,114,64" >}} | Pomarańcz kadmowy | PO20 | Żółty (korpus) |
| {{< swatch "240,180,80" >}} | Żółcień kadmowa | PY35 | Żółty (korpus) |
| {{< swatch "245,210,25" >}} | Kadm Żółty Blady | PY35: Blady | Żółty (jasny kadm) |
| {{< swatch "250,230,5" >}} | Cytrynowo Żółty | PY3 | Żółty (cytrynowy) |
| {{< swatch "225,155,10" >}} | Żółcień niklowo-azowa | PY150 | Żółty (środkowy) |
| {{< swatch "180,175,45" >}} | Zielone złoto | PY129 | Żółto-zielony (złoty) |

### Kolory Ziemi

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Spalona Sienna | PBr7:Spalony | Ziemia (czerwony brąz) |
| {{< swatch "117,66,0" >}} | Spalony Umber | PBr7:Umber | Ziemia (neutralny) |
| {{< swatch "205,68,35" >}} | Surowa Sienna | PBr7:Surowy | Ziemia (żółty brąz) |
| {{< swatch "187,124,25" >}} | Żółta ochra | PY42 | Ziemia (żółty) |

### Zieloni

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Zieleń ftalowa (YS) | PG36 | Zielony (odcień żółci Phtalo) |
| {{< swatch "64,130,109" >}} | Wiridian | PG18 | Zielony (Viridian) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Zielony (zimny spokój) |
| {{< swatch "0,110,100" >}} | Winsor Zielony (BS) | PG7 | Zielony (odcień Phthalo Blue) |

### Błękit i cyjan

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Kobaltowy Turkusowy Jasny | PG50 | Cyjan (minerał) |
| {{< swatch "0,148,214" >}} | Błękit Cerulean | PB35 | Cyjan (minerał) |
| {{< swatch "0,100,110" >}} | Turkus Phtalo | PB16 | Niebieski (Ftalo) |
| {{< swatch "0,123,194" >}} | Kobaltowy błękit | PB28 | Niebieski (fioletowy) |
| {{< swatch "0,75,115" >}} | Winsor Niebieski | PB15 | Niebieski (Ftalo) |
| {{< swatch "27,63,148" >}} | Ultramaryna | PB29 | Niebieski (fioletowy) |

### Fiołki, magenty i czerwienie

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Genialny fiolet | PV23 | Fioletowy (Dioksazyna) |
| {{< swatch "230,90,180" >}} | Trwała Róża | PV19:Róża | Magenta (chinakrydon) |
| {{< swatch "190,40,120" >}} | Quinakrydon Magenta | PV19:Magenta | Magenta (chinakrydon) |
| {{< swatch "160,30,65" >}} | Stały alizarynowy karmazyn | PV19:Szkarłat | Magenta (chinakrydon) |
| {{< swatch "120,35,65" >}} | Fiolet perylenowy | PV29 | Magenta (chinakrydon) |
| {{< swatch "135,10,45" >}} | Perylen Bordowy | PR179 | Czerwony (karmazynowy) |
| {{< swatch "215,30,60" >}} | Czerwony Pirol | PR254 | Czerwony (Szkarłatny) |
| {{< swatch "225,55,65" >}} | Czerwone światło pirolu | PR255 | Czerwony (światło pirolowe) |

### Czarni i biali

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Mars Czarny (Ciepły) | PBk11 | Czarny (Mars) |
| {{< swatch "18,28,12" >}} | Zieleń perylenowa | PBk31 | Czarny (zielony perylenowy) |
| {{< swatch "10,18,19" >}} | Kość słoniowa czarna (fajna) | PBk9 | Czarny (kość słoniowa) |
| {{< swatch "18,18,18" >}} | Lampa Czarna (Neutralna) | PBk7 | Czarny (Lampa) |
| {{< swatch "255,249,235" >}} | Tytanowa biel (ciepła) | PW6:Ciepły | Biały (tytanowy ciepły) |
| {{< swatch "255,255,255" >}} | Biel tytanowa (neutralna) | PW6 | Biały (neutralny tytan) |
| {{< swatch "245,250,255" >}} | Cynk biały (chłodny) | PW4 | Biały (cynkowy chłodny) |

### Kontroluj szarości

Szarości kontrolne to standaryzowane neutralizatory stosowane do przewidywalnej desaturacji mieszanek.

| Próbka | Imię | Kod CI |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Ciepły szary | N_CIEPŁY |
| {{< swatch "128,128,128" >}} | Neutralny szary | N_NEUTRALNY |
| {{< swatch "120,128,135" >}} | Chłodny Szary | N_COOL |

## Mapa palet

Mapa palet wizualizuje aktywną paletę w postaci koła odcieni: 36 sektorów odcieni (w krokach co 10°) × 15 komórek jasności. Po dodaniu składników podstawowych system generuje miksy wtórne i trzeciorzędowe i umieszcza je w odpowiednich pozycjach na mapie.

Kliknięcie komórki powoduje wybranie koloru jako pierwszego planu. Kliknięcie z wciśniętym klawiszem Shift przypisuje go jako nadrzędny punkt końcowy w mikserze palet.

## Mikser palet

Palette Mixer uzyskuje nowe kolory z dwóch wpisów nadrzędnych przy użyciu stałego, trzyetapowego potoku:

1. **Mieszanka**: Widmowe WGM pomiędzy rodzicem A (CCW) i rodzicem B (CW).
2. **Chroma**: Mieszaj w kierunku neutralnego spektrum palety, zmniejszając nasycenie.
3. **Ton**: Mieszaj w kierunku mieszania bieli lub czerni, dostosowując jasność.

Ton jest stosowany jako ostatni, więc korekty jasności nie są osłabiane przez zmiany chrominancji. Elementy sterujące Blokada wartości i Zacisk pasma ograniczają wyniki do określonego poziomu jasności lub pasma wartości.

Mieszane kolory można zapisać na palecie jako wpisy **Niestandardowe**, przechowując pełną recepturę (nadrzędne identyfikatory UID, współczynnik mieszania, ton, wartości nasycenia barwy) w celu późniejszego odzyskania.

## Piksele płótna są RGB

System widmowy działa całkowicie w oparciu o konstrukcję palety i dobór kolorów. Po zastosowaniu pociągnięcia pędzlem malowany jest kolor pierwszego planu (już przekonwertowany na liniowy RGB). Płótno przechowuje standardowe dane pikseli RGB.Mieszanie widmowe usprawnia budowanie palety i wybieranie kolorów w sposób zgodny z fizycznym zachowaniem pigmentu, bez zmiany sposobu przechowywania lub komponowania danych obrazu.