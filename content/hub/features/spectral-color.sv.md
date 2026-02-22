---
title: "Spektral färgblandning"
type: docs
---
Lumis palettsystem använder en spektral färgmodell för att simulera hur riktiga pigment blandas. Målet är att få upplevelsen av att bygga och välja färger från en digital palett att bete sig som att blanda fysiska färger. När en färg väl har applicerats på duken är den standard RGB.

## Vad spektralblandning betyder

Traditionell RGB-blandning är additiv: att blanda två RGB-värden ger dem ett medelvärde mot en mittpunkt. Pigmentblandning är subtraktiv: varje pigment absorberar vissa våglängder, och deras kombinerade effekt är mörkare och skiftar ofta i nyans.

Lumi modellerar detta med en 10-bands spektral reflektansrepresentation för palettfärger, snarare än RGB.

Detta ger färgliknande resultat: att blanda blått och gult ger grönt, inte grått. Att blanda två mättade färger ger en färg som skiftar mot neutralt på det sätt som fysiska pigment gör.

Den spektrala beräkningen körs under palettkonstruktion — när sekundära och tertiära palettposter genereras och när Palettmixern blandar två överordnade färger. Den resulterande färgen konverteras till linjär RGB för visning och för målning.

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

### Apelsiner och gula| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrolapelsin | PO73 | Röd (Scarlet) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kadmium Orange | PO20 | Gul (kropp) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kadmiumgul | PY35 | Gul (kropp) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kadmium gul blek | PY35:Blek | Gul (kadmium blek) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Citrongul | PY3 | Gul (citron) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Nickel Azo Gul | PY150 | Gul (Mellan) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Grönt guld | PY129 | Gul-grön (guld) |

### Jordens färger

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Brända Sienna | PBr7:Bränt | Jorden (rödbrun) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bränd Umber | PBr7:Umber | Jorden (neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Raw Sienna | PBr7:Raw | Jord (gulbrun) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gul ockra | PY42 | Jord (Gul) |

### Gröna

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo Green (YS) | PG36 | Grön (Phthalo Yellow-Shade) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridian | PG18 | Grön (Viridian) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Grön (Earth Cool) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Green (BS) | PG7 | Grön (Phthalo Blue-Shade) |### Blues och cyan

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Kobolt turkos ljus | PG50 | Cyan (mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cerulean Blue | PB35 | Cyan (mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo Turkos | PB16 | Blå (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Koboltblå | PB28 | Blå (Violet-Lean) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Blue | PB15 | Blå (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramarin | PB29 | Blå (Violet-Lean) |

### Violer, magenta och röda

| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Brilliant Violet | PV23 | Violett (dioxazin) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanent Rose | PV19:Rose | Magenta (Quinakridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinacridon Magenta | PV19:Magenta | Magenta (Quinakridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Quinakridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Violet | PV29 | Magenta (Quinakridon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Maroon | PR179 | Röd (Crimson) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrolröd | PR254 | Röd (Scarlet) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrol rött ljus | PR255 | Röd (Pyrrol Light) |

### Svarta och vita| Swatch | Namn | CI-kod | Familj |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Mars Black (Varm) | PBk11 | Svart (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Grön | PBk31 | Svart (Perylene Grön) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ivory Black (Cool) | PBk9 | Svart (elfenben) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lampa Svart (Neutral) | PBk7 | Svart (Lampa) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanvit (varm) | PW6:Varm | Vit (Titan varm) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanvit (neutral) | PW6 | Vit (Titan Neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zinkvit (Cool) | PW4 | Vit (Zink Cool) |

### Kontrollgrå

Kontrollgrå färger är standardiserade neutralisatorer som används för att förutsägbart avmätta blandningar.

| Swatch | Namn | CI-kod |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Varmgrå | N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Neutral grå | N_NEUTRAL |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cool Grey | N_COOL |

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

## Canvas pixlar är RGBSpektralsystemet fungerar helt inom palettkonstruktion och färgval. När ett penseldrag appliceras är förgrundsfärgen – redan omvandlad till linjär RGB – det som målas. Duken lagrar standard RGB-pixeldata.

Spektralblandning förbättrar upplevelsen av att bygga en palett och välja färger på ett sätt som överensstämmer med fysiskt pigmentbeteende, utan att ändra hur bilddata lagras eller sammansätts.