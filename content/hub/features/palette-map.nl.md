---
title: "Paletkaart"
type: docs
---
De Paletkaart beantwoordt een praktische vraag voor schilders: welke kleuren kunnen er, gegeven een set pigmenten, eigenlijk daaruit worden gemengd? Vertrekkend van de invoerpigmenten van het palet, onderzoekt het procedureel elke combinatie (mengsels van twee pigmenten, mengsels van drie richtingen, toonvariaties) en brengt de resultaten in kaart op een kleurenwiel. De uitvoer is een afbeelding van de bereikbare kleurruimte voor die specifieke set pigmenten.

De kaart is ook een op coördinaten gebaseerd navigatiehulpmiddel. Het organiseert elke gegenereerde mix op basis van tint en lichtheid in een cirkelvormig raster, zodat het hele palet in één oogopslag leesbaar is en elke kleur een stabiel thuisadres heeft.

## Rasterstructuur

De kaart is verdeeld in een raster van 36 × 15:

- **36 tintsectoren**: stappen van 10° rond het wiel, gecentreerd op de belangrijkste tintnamen.
- **15 helderheidscellen**: 3 cellen per waardeband × 5 banden (High Key, Upper Mid, Middle, Lower Mid, Deep), lopend van wit aan de buitenkant tot zwart in het midden.

Elke cel is een kleine wig in het wiel. Van een item dat in een cel wordt geplaatst, wordt gezegd dat deze cel de **oorsprong** heeft: het logische thuisadres op de kaart.

## Kleuren in cellen

Wanneer meerdere kleuren strijden om dezelfde cel, wordt slechts één **winnaar** prominent weergegeven:

1. **Primaire** deelnemers winnen altijd hun cel, ongeacht andere bewoners.
2. Als er geen Primary aanwezig is, wint de gegenereerde mix (Secondary of Tertiary) met de **hoogste chroma**.

Inzendingen die niet winnen zijn runner-ups en blijven toegankelijk via click cycling (zie hieronder).

Aangepaste items (opgeslagen mixen) worden weergegeven als vierkante stippen; gegenereerde mixen en primaire kleuren worden weergegeven als ronde stippen.

## Klik op Fietsen

Als u op een bezette cel klikt, wordt de winnaar als voorgrondkleur geselecteerd. Als u nogmaals op dezelfde cel klikt, gaat u naar de volgende bewoner (de op de tweede plaats gegenereerde mixen en vervolgens eventuele aangepaste vermeldingen die op dat netwerkadres zijn opgeslagen). Met elke klik gaat u één stap verder in de stapel.

**Klik met de linkermuisknop** leidt naar de voorgrond. Wanneer het kleurdoel is ingesteld op achtergrond (vanuit de gereedschapset), klikt u op route naar de achtergrond.

## Shift-Select: Mixer-eindpunten laden

Houd **Shift** ingedrukt om de eindpuntlaadmodus te openen:

- **Klik met de linkermuisknop** wijst het aangeklikte item toe als **Parent A (CCW)** in de Paletmixer.
- **Klik met de rechtermuisknop** wijst het toe als **Ouder B (CW)**.

Alleen Klasse A-inzendingen (primaire en aangepaste mixen met intacte herkomst) kunnen in deze modus worden geselecteerd. Tertiairen zijn verborgen en niet-Klasse-A-punten zijn gedimd. Een korte overlay bevestigt dat de modus actief is.

## Mixer-ouderhoogtepunten

Wanneer de Paletmixer actieve Parent A- en Parent B-eindpunten heeft, worden beide op de kaart gemarkeerd met **diamanten ringen** (een ruitvorm met een zwarte rand). Deze hoogtepunten blijven zichtbaar, zelfs als andere weergave-elementen worden omgeschakeld, zodat de actieve overvloei-ouders altijd herkenbaar zijn.

## Oorsprong versus visuele positie

Elke invoer heeft twee posities op de kaart:

- **Oorsprong (broncel)**: het logische rasteradres waartoe het item behoort, vast voor zijn levensduur.
- **Visuele puntpositie**: waar de kleur daadwerkelijk wordt weergegeven op basis van de perceptuele tint en lichtheid.

Met **Best-Match Relocation** berekent het systeem, wanneer een mix wordt opgeslagen, het optimale recept voor de uiteindelijke kleur en wordt de oorsprong ingesteld zodat deze overeenkomt met de visuele positie van de kleur. Hierdoor blijven opgeslagen kleuren dicht bij hun visuele locatie op het wiel en wordt de kaart ruimtelijk coherent.

## Opgeslagen mixen slepen

Aangepaste items (opgeslagen mixen) kunnen worden verplaatst door te slepen:1. Klik op een aangepast item (vierkante stip) en houd deze ingedrukt en sleep voorbij de drempel van 5 pixels.
2. De cursor verandert om de sleepmodus aan te geven. Hoogtepunten van ouders worden live bijgewerkt terwijl u over de kaart beweegt om de nieuwe gemengde ouders op elke kandidaatpositie te tonen.
3. De versleepte stip springt naar de dichtstbijzijnde geldige monsterpositie.
4. Laat los om te binden. De invoer neemt het recept van de doelcel over: de ouders, overvloeiing, toon en chroma worden bijgewerkt zodat ze overeenkomen, en de oorsprong wordt bijgewerkt zodat deze overeenkomt met de nieuwe visuele positie.

Sleepbewegingen kunnen ongedaan worden gemaakt via **Bewerken → Ongedaan maken**.

## Dubbelklikken: schakelen tussen de kaartwerkruimte

Als u in de **Paleteditor** dubbelklikt op een paletitem, wordt de weergave van de Paletkaart-werkruimte in- en uitgeschakeld. Dit is een snelle manier om te schakelen tussen het bladeren door opgeslagen kleuren en het mixen op de kaart zonder een menu te gebruiken. Het gedrag van één klik (het herstellen van het recept van het item in de Mixer) wordt niet beïnvloed.

## Canvas-overlay

De Paletkaart kan rechtstreeks op het afbeeldingsdoek worden opgeroepen als een overlay op volledig scherm door op het **Voorgrond-/Achtergrondstaal** in de gereedschapskist te klikken. Dit geeft een groot mengoppervlak zonder een permanent paneel aan de kaart te wijden.

## Centraal kleurstaal

Een cirkelvormig staal bevindt zich in het midden van het donutgat en weerspiegelt de kleur van de cel waar de cursor zich bevindt:

- **Hoverkleur**: wanneer de cursor op een kaartitem rust, wordt het staal onmiddellijk bijgewerkt om de kleur van dat item weer te geven.
- **Geselecteerde kleur als reserve**: als er geen cel zweeft, toont het staal het berekende resultaat van de Palette Mixer voor het momenteel geselecteerde item. Als de mixer nog geen oplossing heeft gevonden, wordt de basisweergavekleur van het item gebruikt, zodat de plek nooit leeg blijft.
- Een dunne, donkere rand omlijnt het staal te allen tijde.
- Nadat de cursor kort op het centrale staal blijft staan, verschijnt er een wit-zwarte buitenring om aan te geven dat het gebied interactief is.
- **Als u op het centrale staal klikt** wordt de canvasoverlay gesloten en keert u terug naar de normale afbeeldingsweergave (hetzelfde als klikken buiten de buitenste ring).

## Alt-toets: canvasvergelijkingsmodus

Wanneer de canvas-overlay van de Paletkaart geopend is, wordt door het ingedrukt houden van **Alt** tijdelijk de afbeelding eronder zichtbaar:

- De gehele gebruikersinterface van de paletkaart vervaagt naar onzichtbaar (de dekking daalt naar nul), waardoor het canvas zichtbaar wordt.
- Een cirkelvormig staal van 64 pixels volgt de cursor, gevuld met de huidige gesamplede kleur van de Palette Mixer, zodat u op de hoogte blijft van de actieve mix terwijl u de afbeelding inspecteert.
- Als u Alt loslaat, wordt de paletkaart op volledige dekking hersteld.

Een hintlabel, *"Houd de Alt-toets ingedrukt om de afbeelding te zien"*, wordt ter herinnering in de werkruimteweergave weergegeven.