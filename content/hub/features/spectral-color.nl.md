---
title: "Spectrale kleurenmenging"
type: docs
---
Het paletsysteem van Lumi maakt gebruik van een spectraal kleurenmodel om te simuleren hoe echte pigmenten zich vermengen. Het doel is om de ervaring van het bouwen en selecteren van kleuren uit een digitaal palet te laten lijken op het mengen van fysieke verf. Zodra een kleur op het canvas is toegepast, is deze standaard RGB.

## Wat spectrale menging betekent

Traditionele RGB-menging is additief: door twee RGB-waarden te mengen, worden ze gemiddeld naar een middelpunt. Pigmentmenging is subtractief: elk pigment absorbeert bepaalde golflengten, en hun gecombineerde effect is donkerder en verandert vaak van tint.

Lumi modelleert dit met behulp van een 10-bands spectrale reflectierepresentatie voor paletkleuren, in plaats van RGB.

Dit levert verfachtige resultaten op: het mengen van blauw en geel levert groen op, geen grijs. Het mengen van twee verzadigde kleuren levert een kleur op die naar neutraal verschuift, zoals fysieke pigmenten dat doen.

De spectrale berekening wordt uitgevoerd tijdens de paletconstructie – bij het genereren van secundaire en tertiaire paletinvoeren en wanneer de Palette Mixer twee ouderkleuren mengt. De resulterende kleur wordt omgezet naar lineaire RGB voor weergave en schilderen.

## Pigmentprofielen

Paletinvoer kan worden gebaseerd op echte pigmentgegevens met behulp van **Colour Index (CI)-codes**. Elke CI-pigmentfamilie heeft een karakteristieke spectrale bias die van invloed is op de manier waarop deze zich vermengt.

| Pigmentrol | Menggedrag | Voorbeeld |
| :--- | :--- | :--- |
| **Primair** | Hoge chroma, schone secundaire delen | PY3 (citroengeel), PR122 (magenta) |
| **Lichaam** | Ondoorzichtige, sterke massatoon, verschuift naar olijfgroen in groene mengsels | PY35 (cadmiumgeel), PR108 (cadmiumrood) |
| **Neutralizer** | Desatureert en dempt snel | PBk11 (Mars Zwart), PBr7 (Sienna) |
| **Chroma-anker** | Hoge kleurkracht, domineert mengsels | PB29 (ultramarijnblauw), PG7 (ftalogroen) |

Door primaire kleuren met CI-codes aan een palet toe te voegen, krijgt de mengmachine nauwkeurige spectrale bias voor die kleuren, zodat de gegenereerde secundaire en tertiaire mengsels het menggedrag in de echte wereld weerspiegelen.

## Lumi-pigmenten

Het Master-palet wordt geleverd met de volgende pigmenten. Stalen tonen het typische masstone-uiterlijk (volledige sterkte, onverdund) van elk pigment.

### Sinaasappelen en geel| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Pyrrool Oranje | PO73 | Rood (Scharlakenrood) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Cadmium-oranje | PO20 | Geel (lichaam) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Cadmiumgeel | PY35 | Geel (lichaam) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Cadmiumgeel bleek | PY35: Bleek | Geel (cadmiumbleek) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Citroengeel | PY3 | Geel (citroen) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Nikkel Azogeel | PY150 | Geel (midden) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Groen Goud | PY129 | Geelgroen (goud) |

### Aardekleuren

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Verbrande Sienna | PBr7: Verbrand | Aarde (roodbruin) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Verbrande Omber | PBr7:Omber | Aarde (neutraal) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Rauwe Sienna | PBr7: Rauw | Aarde (geelbruin) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Okergeel | PY42 | Aarde (Geel) |

### Groenen

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Ftalogroen (YS) | PG36 | Groen (Phthalo geel-schaduw) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Viridiaan | PG18 | Groen (viridiaan) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Groen (aardekoel) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Winsor Groen (BS) | PG7 | Groen (Phthalo Blue-Shade) |### Blauw en cyaan

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177.176);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Kobaltturkoois licht | PG50 | Cyaan (Mineraal) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148.214);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Ceruleumblauw | PB35 | Cyaan (Mineraal) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Ftalo-Turkoois | PB16 | Blauw (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Kobaltblauw | PB28 | Blauw (violet-mager) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Winsor Blauw | PB15 | Blauw (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Ultramarijn | PB29 | Blauw (violet-mager) |

### Viooltjes, magenta's en rood

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Briljant violet | PV23 | Violet (Dioxazine) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Permanente roos | PV19:Roos | Magenta (quinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Quinacridon Magenta | PV19:Magenta | Magenta (quinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Permanente Alizarin Crimson | PV19: Karmozijnrood | Magenta (quinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Peryleen Violet | PV29 | Magenta (quinacridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Peryleen Kastanjebruin | PR179 | Rood (karmozijnrood) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Pyrrool Rood | PR254 | Rood (Scharlakenrood) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Pyrrole rood licht | PR255 | Rood (Pyrrollicht) |

### Zwarten en blanken| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Mars Zwart (Warm) | PBk11 | Zwart (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Peryleen Groen | PBk31 | Zwart (peryleengroen) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Ivoorzwart (koel) | PBk9 | Zwart (ivoor) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Lamp Zwart (Neutraal) | PBk7 | Zwart (lamp) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Titaanwit (warm) | PW6: Warm | Wit (Titanium Warm) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Titaanwit (neutraal) | PW6 | Wit (titaniumneutraal) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Zinkwit (koel) | PW4 | Wit (zinkkoel) |

### Controle grijstinten

Controlegrijstinten zijn gestandaardiseerde neutralisatoren die worden gebruikt om mengsels voorspelbaar te desatureren.

| Staal | Naam | CI-code |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Warm grijs | N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Neutraal Grijs | N_NEUTRAAL |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px effen rgba(0,0,0,0.25)"></span> | Koel grijs | N_COOL |

## De paletkaart

De Paletkaart visualiseert het actieve palet als een tintwiel: 36 tintsectoren (stappen van 10°) × 15 helderheidscellen. Wanneer primaire kleuren worden toegevoegd, genereert het systeem secundaire en tertiaire mengsels en plaatst deze op de juiste kaartposities.

Als u op een cel klikt, wordt een kleur als voorgrond geselecteerd. Houd Shift ingedrukt en wijs het toe als bovenliggend eindpunt in de Paletmixer.

## De paletmixer

De Palette Mixer ontleent nieuwe kleuren aan twee bovenliggende items met behulp van een vaste pijplijn in drie fasen:

1. **Blend**: Spectrale WGM tussen ouder A (CCW) en ouder B (CW).
2. **Chroma**: vervaag naar het neutrale spectrum van het palet, waardoor de verzadiging wordt verminderd.
3. **Toon**: Meng naar het mengen van wit of het mengen van zwart, waarbij de lichtheid wordt aangepast.

De toon wordt als laatste toegepast, zodat aanpassingen aan de lichtheid niet worden verwaterd door chromaveranderingen. De besturingselementen Value Lock en Band Clamp beperken de resultaten tot een specifiek lichtheidsniveau of waardeband.

Gemengde kleuren kunnen in het palet worden opgeslagen als **Aangepaste** waarden, waarbij het volledige recept (bovenliggende UID's, overvloeifactor, toon, chromawaarden) wordt opgeslagen voor later herstel.

## Canvaspixels zijn RGBHet spectrale systeem werkt volledig binnen de paletconstructie en kleurselectie. Wanneer een penseelstreek wordt toegepast, wordt de voorgrondkleur (die al is omgezet in lineaire RGB) geschilderd. Het canvas slaat standaard RGB-pixelgegevens op.

Spectrale menging verbetert de ervaring van het bouwen van een palet en het kiezen van kleuren op een manier die consistent is met het fysieke pigmentgedrag, zonder de manier te veranderen waarop beeldgegevens worden opgeslagen of samengesteld.