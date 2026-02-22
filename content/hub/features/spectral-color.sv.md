---
title: "Spektral färgblandning"
type: docs
---
Lumis palettsystem använder en spektral färgmodell för att simulera hur riktiga pigment blandas. Målet är att få upplevelsen av att bygga och välja färger från en digital palett att bete sig som att blanda fysiska färger. När en färg väl har applicerats på duken är den standard RGB.

## Vad spektralblandning betyder

Traditionell RGB-blandning är additiv: att blanda två RGB-värden ger dem ett medelvärde mot en mittpunkt. Pigmentblandning är subtraktiv: varje pigment absorberar vissa våglängder, och deras kombinerade effekt är mörkare och skiftar ofta i nyans.

Lumi modellerar detta med en 10-bands spektral reflektansrepresentation för palettfärger, snarare än RGB.

Detta ger färgliknande resultat: att blanda blått och gult ger grönt, inte grått. Att blanda två mättade färger ger en färg som skiftar mot neutralt på det sätt som fysiska pigment gör.

Den spektrala beräkningen körs under palettkonstruktion, när sekundära och tertiära palettposter genereras och när palettmixern blandar två överordnade färger. Den resulterande färgen konverteras till linjär RGB för visning och för målning.

## Pigmentprofiler

Palettposter kan baseras på verkliga pigmentdata med hjälp av **Colour Index (CI)-koder**. Varje CI-pigmentfamilj har en karakteristisk spektral bias som påverkar hur den blandas.

| Pigmentroll | Blandningsbeteende | Exempel |
| :--- | :--- | :--- |
| **Primär** | Hög kromatografi, rena sekundärer | PY3 (citrongul), PR122 (magenta) |
| **Kroppen** | Ogenomskinlig, stark masston, skiftar till oliv i gröna blandningar | PY35 (kadmiumgul), PR108 (kadmiumröd) |
| **Neutraliserare** | Avmättar snabbt och tystar | PBk11 (Mars Black), PBr7 (Sienna) |
| **Chroma Anchor** | Hög färgstyrka, dominerar blandningar | PB29 (Ultramarinblått), PG7 (Phthalo Green) |

Att lägga till primära koder med CI-koder till en palett ger blandningsmotorn exakt spektral förspänning för dessa färger, så genererade sekundära och tertiära blandningar återspeglar verkliga blandningsbeteende.

## Lumi Pigment

Masterpaletten levereras med följande pigment. Prover visar varje pigments typiska masstone (fullstyrka, outspädd) utseende.

### Apelsiner och gula

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Pyrrolapelsin | PO73 | Röd (Scarlet) |
| {{< swatch "243,114,64" >}} | Kadmium Orange | PO20 | Gul (kropp) |
| {{< swatch "240,180,80" >}} | Kadmiumgul | PY35 | Gul (kropp) |
| {{< swatch "245,210,25" >}} | Kadmium gul blek | PY35:Blek | Gul (kadmium blek) |
| {{< swatch "250,230,5" >}} | Citrongul | PY3 | Gul (citron) |
| {{< swatch "225,155,10" >}} | Nickel Azo Gul | PY150 | Gul (Mellan) |
| {{< swatch "180,175,45" >}} | Grönt guld | PY129 | Gul-grön (guld) |

### Jordens färger

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Brända Sienna | PBr7:Bränt | Jorden (rödbrun) |
| {{< swatch "117,66,0" >}} | Bränd Umber | PBr7:Umber | Jorden (neutral) |
| {{< swatch "205,68,35" >}} | Raw Sienna | PBr7:Raw | Jord (gulbrun) |
| {{< swatch "187,124,25" >}} | Gul ockra | PY42 | Jord (Gul) |

### Gröna

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Phthalo Green (YS) | PG36 | Grön (Phthalo Yellow-Shade) |
| {{< swatch "64,130,109" >}} | Viridian | PG18 | Grön (Viridian) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Grön (Earth Cool) |
| {{< swatch "0,110,100" >}} | Winsor Green (BS) | PG7 | Grön (Phthalo Blue-Shade) |

### Blues och cyan

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Kobolt turkos ljus | PG50 | Cyan (mineral) |
| {{< swatch "0,148,214" >}} | Cerulean Blue | PB35 | Cyan (mineral) |
| {{< swatch "0,100,110" >}} | Phthalo Turkos | PB16 | Blå (Phthalo) |
| {{< swatch "0,123,194" >}} | Koboltblå | PB28 | Blå (Violet-Lean) |
| {{< swatch "0,75,115" >}} | Winsor Blue | PB15 | Blå (Phthalo) |
| {{< swatch "27,63,148" >}} | Ultramarin | PB29 | Blå (Violet-Lean) |

### Violer, magenta och röda

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Brilliant Violet | PV23 | Violett (dioxazin) |
| {{< swatch "230,90,180" >}} | Permanent Rose | PV19:Rose | Magenta (Quinakridon) |
| {{< swatch "190,40,120" >}} | Quinacridon Magenta | PV19:Magenta | Magenta (Quinakridon) |
| {{< swatch "160,30,65" >}} | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Quinakridon) |
| {{< swatch "120,35,65" >}} | Perylene Violet | PV29 | Magenta (Quinakridon) |
| {{< swatch "135,10,45" >}} | Perylene Maroon | PR179 | Röd (Crimson) |
| {{< swatch "215,30,60" >}} | Pyrrolröd | PR254 | Röd (Scarlet) |
| {{< swatch "225,55,65" >}} | Pyrrol rött ljus | PR255 | Röd (Pyrrol Light) |

### Svarta och vita

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Mars Black (Varm) | PBk11 | Svart (Mars) |
| {{< swatch "18,28,12" >}} | Perylene Grön | PBk31 | Svart (Perylene Grön) |
| {{< swatch "10,18,19" >}} | Ivory Black (Cool) | PBk9 | Svart (elfenben) |
| {{< swatch "18,18,18" >}} | Lampa Svart (Neutral) | PBk7 | Svart (Lampa) |
| {{< swatch "255,249,235" >}} | Titanvit (varm) | PW6:Varm | Vit (Titan varm) |
| {{< swatch "255,255,255" >}} | Titanvit (neutral) | PW6 | Vit (Titan Neutral) |
| {{< swatch "245,250,255" >}} | Zinkvit (Cool) | PW4 | Vit (Zink Cool) |

### Kontrollgrå

Kontrollgrå färger är standardiserade neutralisatorer som används för att förutsägbart avmätta blandningar.

| Swatch | Namn | CI-kod |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Varmgrå | N_WARM |
| {{< swatch "128,128,128" >}} | Neutral grå | N_NEUTRAL |
| {{< swatch "120,128,135" >}} | Cool Grey | N_COOL |

## Palettkartan

Palettkartan visualiserar den aktiva paletten som ett nyanshjul: 36 nyanssektorer (10° steg) × 15 ljushetsceller. När primärer läggs till genererar systemet sekundära och tertiära blandningar och placerar dem i lämpliga kartpositioner.

Genom att klicka på en cell väljs en färg som förgrund. Skift-klicka tilldelar den som en överordnad slutpunkt i palettmixern.

## Palettmixern

Palettmixern hämtar nya färger från två överordnade poster med hjälp av en fast trestegspipeline:

1. **Blandning**: Spektral WGM mellan förälder A (CCW) och förälder B (CW).
2. **Chroma**: Blanda mot palettens neutrala spektrum, vilket minskar mättnaden.
3. **Ton**: Blanda mot att blanda vitt eller blanda svart, justera ljusheten.

Tonen appliceras sist så att ljushetsjusteringar inte späds ut av färgförändringar. Value Lock and Band Clamp-kontroller begränsar resultaten till en specifik ljushetsnivå eller värdeband.

Blandade färger kan sparas i paletten som **Custom**-poster, och lagrar hela receptet (överordnade UID:n, blandningsfaktor, ton, färgvärden) för senare återställning.

## Canvas pixlar är RGB

Spektralsystemet fungerar helt inom palettkonstruktion och färgval. När ett penseldrag appliceras är förgrundsfärgen (redan omvandlad till linjär RGB) det som målas. Duken lagrar standard RGB-pixeldata.Spektralblandning förbättrar upplevelsen av att bygga en palett och välja färger på ett sätt som överensstämmer med fysiskt pigmentbeteende, utan att ändra hur bilddata lagras eller sammansätts.