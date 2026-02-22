---
title: "Spectrale kleurenmenging"
type: docs
---
Het paletsysteem van Lumi maakt gebruik van een spectraal kleurenmodel om te simuleren hoe echte pigmenten zich vermengen. Het doel is om de ervaring van het bouwen en selecteren van kleuren uit een digitaal palet te laten lijken op het mengen van fysieke verf. Zodra een kleur op het canvas is toegepast, is deze standaard RGB.

## Wat spectrale menging betekent

Traditionele RGB-menging is additief: door twee RGB-waarden te mengen, worden ze gemiddeld naar een middelpunt. Pigmentmenging is subtractief: elk pigment absorbeert bepaalde golflengten, en hun gecombineerde effect is donkerder en verandert vaak van tint.

Lumi modelleert dit met behulp van een 10-bands spectrale reflectierepresentatie voor paletkleuren, in plaats van RGB.

Dit levert verfachtige resultaten op: het mengen van blauw en geel levert groen op, geen grijs. Het mengen van twee verzadigde kleuren levert een kleur op die naar neutraal verschuift, zoals fysieke pigmenten dat doen.

De spectrale berekening wordt uitgevoerd tijdens de paletconstructie, bij het genereren van secundaire en tertiaire paletinvoeren en wanneer de Palette Mixer twee ouderkleuren mengt. De resulterende kleur wordt omgezet naar lineaire RGB voor weergave en schilderen.

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

### Sinaasappelen en geel

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Pyrrool Oranje | PO73 | Rood (Scharlakenrood) |
| {{< swatch "243,114,64" >}} | Cadmium-oranje | PO20 | Geel (lichaam) |
| {{< swatch "240,180,80" >}} | Cadmiumgeel | PY35 | Geel (lichaam) |
| {{< swatch "245,210,25" >}} | Cadmiumgeel bleek | PY35: Bleek | Geel (cadmiumbleek) |
| {{< swatch "250,230,5" >}} | Citroengeel | PY3 | Geel (citroen) |
| {{< swatch "225,155,10" >}} | Nikkel Azogeel | PY150 | Geel (midden) |
| {{< swatch "180,175,45" >}} | Groen Goud | PY129 | Geelgroen (goud) |

### Aardekleuren

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Verbrande Sienna | PBr7: Verbrand | Aarde (roodbruin) |
| {{< swatch "117,66,0" >}} | Verbrande Omber | PBr7:Omber | Aarde (neutraal) |
| {{< swatch "205,68,35" >}} | Rauwe Sienna | PBr7: Rauw | Aarde (geelbruin) |
| {{< swatch "187,124,25" >}} | Okergeel | PY42 | Aarde (Geel) |

### Groenen

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Ftalogroen (YS) | PG36 | Groen (Phthalo geel-schaduw) |
| {{< swatch "64,130,109" >}} | Viridiaan | PG18 | Groen (viridiaan) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Groen (aardekoel) |
| {{< swatch "0,110,100" >}} | Winsor Groen (BS) | PG7 | Groen (Phthalo Blue-Shade) |

### Blauw en cyaan

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Kobaltturkoois licht | PG50 | Cyaan (Mineraal) |
| {{< swatch "0,148,214" >}} | Ceruleumblauw | PB35 | Cyaan (Mineraal) |
| {{< swatch "0,100,110" >}} | Ftalo-Turkoois | PB16 | Blauw (ftalo) |
| {{< swatch "0,123,194" >}} | Kobaltblauw | PB28 | Blauw (violet-mager) |
| {{< swatch "0,75,115" >}} | Winsor Blauw | PB15 | Blauw (ftalo) |
| {{< swatch "27,63,148" >}} | Ultramarijn | PB29 | Blauw (violet-mager) |

### Viooltjes, magenta's en rood

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Briljant violet | PV23 | Violet (Dioxazine) |
| {{< swatch "230,90,180" >}} | Permanente roos | PV19:Roos | Magenta (quinacridon) |
| {{< swatch "190,40,120" >}} | Quinacridon Magenta | PV19:Magenta | Magenta (quinacridon) |
| {{< swatch "160,30,65" >}} | Permanente Alizarin Crimson | PV19: Karmozijnrood | Magenta (quinacridon) |
| {{< swatch "120,35,65" >}} | Peryleen Violet | PV29 | Magenta (quinacridon) |
| {{< swatch "135,10,45" >}} | Peryleen Kastanjebruin | PR179 | Rood (karmozijnrood) |
| {{< swatch "215,30,60" >}} | Pyrrool Rood | PR254 | Rood (Scharlakenrood) |
| {{< swatch "225,55,65" >}} | Pyrrole rood licht | PR255 | Rood (Pyrrollicht) |

### Zwarten en blanken

| Staal | Naam | CI-code | Familie |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Mars Zwart (Warm) | PBk11 | Zwart (Mars) |
| {{< swatch "18,28,12" >}} | Peryleen Groen | PBk31 | Zwart (peryleengroen) |
| {{< swatch "10,18,19" >}} | Ivoorzwart (koel) | PBk9 | Zwart (ivoor) |
| {{< swatch "18,18,18" >}} | Lamp Zwart (Neutraal) | PBk7 | Zwart (lamp) |
| {{< swatch "255,249,235" >}} | Titaanwit (warm) | PW6: Warm | Wit (Titanium Warm) |
| {{< swatch "255,255,255" >}} | Titaanwit (neutraal) | PW6 | Wit (titaniumneutraal) |
| {{< swatch "245,250,255" >}} | Zinkwit (koel) | PW4 | Wit (zinkkoel) |

### Controle grijstinten

Controlegrijstinten zijn gestandaardiseerde neutralisatoren die worden gebruikt om mengsels voorspelbaar te desatureren.

| Staal | Naam | CI-code |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Warm grijs | N_WARM |
| {{< swatch "128,128,128" >}} | Neutraal Grijs | N_NEUTRAAL |
| {{< swatch "120,128,135" >}} | Koel grijs | N_COOL |

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

## Canvaspixels zijn RGB

Het spectrale systeem werkt volledig binnen de paletconstructie en kleurselectie. Wanneer een penseelstreek wordt toegepast, wordt de voorgrondkleur (al geconverteerd naar lineaire RGB) geschilderd. Het canvas slaat standaard RGB-pixelgegevens op.Spectrale menging verbetert de ervaring van het bouwen van een palet en het kiezen van kleuren op een manier die consistent is met het fysieke pigmentgedrag, zonder de manier te veranderen waarop beeldgegevens worden opgeslagen of samengesteld.