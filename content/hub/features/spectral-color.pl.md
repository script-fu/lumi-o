---
title: "Widmowe mieszanie kolorów"
type: docs
---
System palet Lumi wykorzystuje widmowy model kolorów do symulacji mieszania się prawdziwych pigmentów. Celem jest, aby doświadczenie budowania i wybierania kolorów z cyfrowej palety przypominało mieszanie fizycznych farb. Po nałożeniu koloru na płótno jest to standardowy kolor RGB.

## Co oznacza mieszanie widmowe

Tradycyjne mieszanie RGB ma charakter addytywny: zmieszanie dwóch wartości RGB uśrednia je w kierunku punktu środkowego. Mieszanie pigmentów ma charakter subtraktywny: każdy pigment pochłania określone długości fal, a ich łączny efekt jest ciemniejszy i często zmienia odcień.

Lumi modeluje to przy użyciu 10-pasmowej reprezentacji widmowego współczynnika odbicia dla palety kolorów, a nie RGB.

Daje to rezultaty przypominające farbę: zmieszanie koloru niebieskiego i żółtego daje kolor zielony, a nie szary. Mieszanie dwóch nasyconych kolorów daje kolor, który zmienia się w kierunku neutralnym, tak jak robią to pigmenty fizyczne.

Obliczenia widmowe są przeprowadzane podczas konstruowania palety — podczas generowania wpisów palety drugorzędnej i trzeciorzędnej oraz gdy Mikser palet łączy dwa kolory nadrzędne. Wynikowy kolor jest konwertowany do liniowego RGB na potrzeby wyświetlania i malowania.

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

### Pomarańcze i żółcie| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pirolowa Pomarańcza | PO73 | Czerwony (Szkarłatny) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pomarańcz kadmowy | PO20 | Żółty (korpus) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Żółcień kadmowa | PY35 | Żółty (korpus) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Kadm Żółty Blady | PY35: Blady | Żółty (jasny kadm) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cytrynowo Żółty | PY3 | Żółty (cytrynowy) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Żółcień niklowo-azowa | PY150 | Żółty (środkowy) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zielone złoto | PY129 | Żółto-zielony (złoty) |

### Kolory Ziemi

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Spalona Sienna | PBr7:Spalony | Ziemia (czerwony brąz) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Spalony Umber | PBr7:Umber | Ziemia (neutralny) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Surowa Sienna | PBr7:Surowy | Ziemia (żółty brąz) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Żółta ochra | PY42 | Ziemia (żółty) |

### Zieloni

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zieleń ftalowa (YS) | PG36 | Zielony (odcień żółci Phtalo) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Wiridian | PG18 | Zielony (Viridian) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Terre Verte | PG23 | Zielony (zimny spokój) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Zielony (BS) | PG7 | Zielony (odcień Phthalo Blue) |### Błękit i cyjan

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Kobaltowy Turkusowy Jasny | PG50 | Cyjan (minerał) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Błękit Cerulean | PB35 | Cyjan (minerał) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Turkus Phtalo | PB16 | Niebieski (Ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kobaltowy błękit | PB28 | Niebieski (fioletowy) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Niebieski | PB15 | Niebieski (Ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramaryna | PB29 | Niebieski (fioletowy) |

### Fiołki, magenty i czerwienie

| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Genialny fiolet | PV23 | Fioletowy (Dioksazyna) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Trwała Róża | PV19:Róża | Magenta (chinakrydon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinakrydon Magenta | PV19:Magenta | Magenta (chinakrydon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Stały alizarynowy karmazyn | PV19:Szkarłat | Magenta (chinakrydon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Fiolet perylenowy | PV29 | Magenta (chinakrydon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylen Bordowy | PR179 | Czerwony (karmazynowy) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Czerwony Pirol | PR254 | Czerwony (Szkarłatny) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Czerwone światło pirolu | PR255 | Czerwony (światło pirolowe) |

### Czarni i biali| Próbka | Imię | Kod CI | Rodzina |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Mars Czarny (Ciepły) | PBk11 | Czarny (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zieleń perylenowa | PBk31 | Czarny (zielony perylenowy) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kość słoniowa czarna (fajna) | PBk9 | Czarny (kość słoniowa) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lampa Czarna (Neutralna) | PBk7 | Czarny (Lampa) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Tytanowa biel (ciepła) | PW6:Ciepły | Biały (tytanowy ciepły) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Biel tytanowa (neutralna) | PW6 | Biały (neutralny tytan) |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Cynk biały (chłodny) | PW4 | Biały (cynkowy chłodny) |

### Kontroluj szarości

Szarości kontrolne to standaryzowane neutralizatory stosowane do przewidywalnej desaturacji mieszanek.

| Próbka | Imię | Kod CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Ciepły szary | N_CIEPŁY |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Neutralny szary | N_NEUTRALNY |
| <span style="display:inline-block;width:1,3em;height:1,3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0,25)"></span> | Chłodny Szary | N_COOL |

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

## Piksele płótna są RGBSystem widmowy działa całkowicie w oparciu o konstrukcję palety i dobór kolorów. Po zastosowaniu pociągnięcia pędzla kolor pierwszego planu — już przekonwertowany na liniowy RGB — jest malowany. Płótno przechowuje standardowe dane pikseli RGB.

Mieszanie widmowe usprawnia budowanie palety i wybieranie kolorów w sposób zgodny z fizycznym zachowaniem pigmentu, bez zmiany sposobu przechowywania lub komponowania danych obrazu.