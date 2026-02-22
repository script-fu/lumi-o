---
title: "Paletmixer"
type: docs
---
De Palette Mixer leidt nieuwe kleuren af ​​uit paren paletingangen met behulp van een vaste pijplijn in drie fasen. Omdat menging plaatsvindt in het spectrale domein en niet in RGB, gedragen de resultaten zich als fysieke pigmenten: blauw en geel produceren groen, terwijl verzadigde kleuren tijdens het mengen naar neutraal verschuiven.

## De pijpleiding

Elke kleur die door de Mixer wordt geproduceerd, doorloopt drie fasen in een vaste volgorde:

1. **Blend**: Spectrale WGM tussen ouder A (CCW) en ouder B (CW).
2. **Chroma**: vervaag naar het neutrale spectrum van het palet, waardoor de verzadiging wordt verminderd.
3. **Toon**: Blend naar het mengen van wit (tint) of het mengen van zwart (tint).

De toon wordt altijd als laatste toegepast. Dit maakt lichtheid dominant: een toonaanpassing komt precies op het beoogde lichtheidsniveau terecht zonder te worden verdund door de chromaaanpassing die eraan voorafgaat.

## Ouders selecteren

Parent A en Parent B zijn de twee items waartussen de overvloeischuifregelaar mengt. Ze worden geladen vanuit de Paletkaart:

- Houd **Shift** ingedrukt op de paletkaart en **klik met de linkermuisknop** om Parent A (CCW) in te stellen.
- Houd **Shift** ingedrukt en **klik met de rechtermuisknop** om Parent B (CW) in te stellen.

Alleen **Klasse A**-inzendingen (primaire en aangepaste mixen met intacte herkomst) worden als ouders geaccepteerd. Tertiarissen en inzendingen met verloren voorouders zijn uitgesloten.

De posities van Parent A en Parent B van de Mixer worden op de kaart weergegeven als **diamanten ring** hoogtepunten, zodat u altijd kunt zien welke items zijn geladen.

## De schuifregelaars

| Schuifregelaar | Effect |
| :--- | :--- |
| **Meng** | Beweegt tussen Parent A (CCW-einde) en Parent B (CW-einde). Bij 0,0 komt het resultaat overeen met Ouder A; bij 1.0 komt het overeen met Ouder B. |
| **Chroma** | Desatureert de overvloei naar de neutrale kleur van het palet. Hogere waarden produceren meer gedempte, aardse resultaten. |
| **Toon** | Verschuift de lichtheid naar het mengen van wit (tintrichting) of het mengen van zwart (schaduwrichting). |

## Waardecontroles

**Waardevergrendeling** bevriest de perceptuele lichtheid (CIE L\*) op het huidige niveau terwijl de andere schuifregelaars bewegen. Gebruik dit om chroma- of tintvariatie te onderzoeken zonder de waarde van een mix te veranderen.

**Bandklem** zorgt ervoor dat het resultaat binnen de grenzen van de huidige waardeband blijft (bijvoorbeeld binnen Lower Mid). De toonschuifregelaar is nog steeds versleepbaar, maar de lichtheid van de uitvoer is beperkt.

De schuifregelaar Toon weerspiegelt ook eventuele waardeverschillen die zijn geconfigureerd in de Palette Editor. Lichtheidsbereiken die binnen een opening vallen, worden weergegeven als semi-transparante grijze banden op de schuifgoot. De schuifregelaar springt automatisch over deze gaten: als u door een grijs gebied sleept, springt u naar de dichtstbijzijnde geldige bandgrens aan de andere kant.

## Eindpunten mengen (wit, zwart, neutraal)

De toon- en chromafasen vereisen referentie-eindpunten: een mengwit, een mengzwart en een neutraal. Lumi ontdekt deze automatisch door in het actieve palet te zoeken naar de beste kandidaten:

- **Mixing White**: het primaire chromaniveau dat het dichtst bij puur wit ligt.
- **Mixing Black**: de laagste lichtheid Primary.
- **Neutraal**: de primaire die het dichtst bij achromatisch ligt (laagste chroma).

Deze kunnen handmatig worden overschreven door met de rechtermuisknop op een item in de Palette Editor te klikken.

## Een mix opslaanKlik op **Toevoegen aan palet** om het huidige mixerresultaat op te slaan als een **Opgeslagen mix** (aangepaste invoer). Voordat het systeem opslaat, past het systeem **Best-Match Relocation** toe: het zoekt in het palet naar het optimale recept dat dezelfde uiteindelijke kleur produceert met de beste ruimtelijke pasvorm op de Paletkaart. Als er een nauwkeuriger recept wordt gevonden, springen de mixerschuifregelaars om dit weer te geven, wat bevestigt dat het systeem een ​​betere oorsprong heeft gevonden en dat de positie van het opgeslagen item wordt uitgelijnd met de visuele stip op de kaart.

Opgeslagen mixen slaan hun volledige recept op (Ouder A/B UID's, mengfactor, toon, chroma), zodat ze exact kunnen worden gereproduceerd.

## Receptherstel

Door één keer op een aangepast item in de Palette Editor te klikken, wordt het recept van dat item in de Mixer hersteld:

- Parent A en Parent B worden opnieuw geladen.
- De overvloei-, toon- en chromaschuifregelaars keren terug naar hun oorspronkelijke posities.
- Elke waardevergrendeling of bandklem die actief was tijdens het maken, wordt opnieuw ingeschakeld.

Hierdoor is het eenvoudig om terug te keren naar een kleur en deze verder aan te passen, of om deze als uitgangspunt te gebruiken voor een nieuwe mix.