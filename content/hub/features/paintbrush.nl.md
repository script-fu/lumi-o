---
title: "Penseel gereedschap"
type: docs
---
De Paintbrush is het belangrijkste tekengereedschap, ontworpen voor responsief, intelligent penseelwerk met volledige controle over de dynamiek van druk, snelheid, kanteling en afstand.

## Overzicht

Het gereedschap Penseel ondersteunt raster-, procedureel gegenereerde en geanimeerde penseeltypen. Slagen kunnen worden gestabiliseerd, afgevlakt en nabewerkt. De penseeldynamiek reageert op stylusinvoer en geeft nauwkeurige controle over de dekking, grootte, kleur, hoek en andere eigenschappen tijdens een streek.

## Penseeltypen

### Rasterpenselen (.raster)

Bitmappenseelafbeeldingen die alfatransparantie ondersteunen.

### Gegenereerde penselen (.param)

Procedureel weergegeven vormen (cirkel, vierkant, ruit, driehoek) met instelbare parameters: hardheid, beeldverhouding, hoek, rondheid en hoekradius. Gegenereerde penselen zijn lichtgewicht en schaalbaar.

### Geanimeerde penselen (.anim)

Opeenvolgende framesequenties die tijdens de slagen vooruitgaan. Frames kunnen stapsgewijs worden doorlopen (frame-voortgangen per dab), willekeurig per dab worden geselecteerd of worden geïndexeerd op basis van dynamiek (druk, snelheid, kanteling, hoek).

## Schildercursor

De cursor past zich aan de huidige gereedschapsstatus aan en geeft duidelijke, contextuele feedback:

- **Penseelomtrek**: de cursor volgt de exacte vorm en grootte van het penseel en geeft een live voorbeeld van waar de verf terechtkomt.
- **Wismodus**: wanneer wissen actief is, verandert de omtrek in een gestippelde cirkel om wisstreken visueel te onderscheiden van verfstreken.
- **Eenvoudige penseelgrens**: voor complexe of zeer grote penselen waarbij het weergeven van de nauwkeurige omtrek kostbaar is, schakelt u **Eenvoudige penseelgrens** in (in Extra opties) om in plaats daarvan een gewone cirkel te gebruiken.

## Gereedschapsopties

### Bediening op het hoogste niveau

Altijd aanwezig, buiten elke expander:
- **Modus**: Verfovervloeimodus (Normaal, Vermenigvuldigen, Scherm, etc.)
- **Dekking**: algehele dekking van de lijn (0–100).

### Penseelopties

In de uitbreiding **Penseelopties** (standaard uitgevouwen):
- **Grootte**: Penseeldiameter in pixels.
- **Verhouding**: De penseelvorm platdrukken of uitrekken (-1,0–1,0). 0 = ongewijzigd; negatieve waarden roteren de pompoen 90°.
- **Hoek**: Roteert de penseelstempel (-180–180°). Onafhankelijk van de dynamiek van de slagrichting.
- **Spatiëring**: afstand tussen geverfde klodders als percentage van de penseelgrootte. Lager = vloeiendere bewegingen; hoger = verspreid patroon.
- **Hardheid**: zachte vervaging (0,0) tot scherpe rand (1,0).
- **Kracht**: kracht bij het aanbrengen van de borstel (0,0–1,0). Verborgen voor het gereedschap Potlood.
- **Jitter**: Verschuift willekeurig elke schar-positie met maximaal dit aantal pixels (0-1024).
- **Gum**: Groottevermenigvuldiger wordt toegepast wanneer dit penseel als gum wordt gebruikt (0,1–10,0). Niet weergegeven op het gummetje zelf.

### Lijneffecten

In de uitbreiding **Streekeffecten**:
- **Postproces**: past stabilisatie, snelheidscompressie en herhalingscorrectie toe nadat de slag is voltooid, waardoor de consistentie zonder latentie wordt verbeterd.
  - **Draaidrempel**: Hoekdrempel (0–180°) voor richtingscorrectie bij scherpe hoeken. 0 = fix richting overslaan.
  - **Voorbeeldsnelheid**: onderdrukt het nabewerkingsvoorbeeld wanneer de slagsnelheid deze waarde overschrijdt (0 = altijd voorbeeld).
- **Opbouw**: wanneer deze optie is ingeschakeld, wordt bij elke schar de dekking opgebouwd in plaats van dat deze als één enkele streek wordt samengesteld.#### Kalligrafisch
Wanneer actief, wordt dab stamping vervangen door een doorlopende geometrische gang:
- **Breedte** en **Hoogte**: afmetingen van de kalligrafische gang.
- **Hoek**: puntoriëntatie (graden).
- **Dynamische dekking**: moduleert de dekking binnen de streek op basis van snelheids- en richtingsveranderingen. Werkt het beste bij fijne, gecontroleerde bewegingen; de resultaten zijn minder voorspelbaar bij snelle krabbels. Experimenteel.
- **Snelheidsgroei** (0–100%): Maximaal toegestane toename van de grootte per monster als percentage van de grootte van het vorige monster. Beperkt hoe snel een snelheidsgedreven maatdynamiek kan groeien, waardoor plotselinge sprongen worden voorkomen wanneer de slag versnelt.
- **Snelheidskrimp** (0–100%): Maximaal toegestane afname van de grootte per monster. Beperkt hoe snel de maat kan afnemen als de slag vertraagt.

#### Gladmaken

Maakt real-time invoerverzachting mogelijk die wordt toegepast op het lijnpad terwijl u tekent. Breidt uit om te onthullen:
  - **Diepte** (2–256): aantal eerdere invoermonsters die in aanmerking zijn genomen bij het berekenen van de afgevlakte positie. Hogere waarden produceren een langere, meer toegewijde vertraging.
  - **Positie** (0–100): Intensiteit van de verzachting toegepast op de penseelpositie. Hogere waarden ronden scherpe richtingsveranderingen af.
  - **Druk** (0–100): Afvlakking toegepast op het stylusdruksignaal, waardoor drukpieken en trillingen worden verminderd.
  - **Richting** (0–100): Afvlakking toegepast op de streekrichting, waardoor de hoekgevoelige dynamiek wordt gestabiliseerd.

#### Dynamiek

Wijs stylusinvoer of andere livewaarden toe aan schilderparameters:

- **Druk** (stylus): regelt de grootte, dekking, snelheid, hardheid, kleur en meer op basis van de stylusdruk.
- **Snelheid**: brengt de streeksnelheid in kaart aan de penseeleigenschappen.
- **Kantelen**: X- en Y-kantelhoeken van de stylus beïnvloeden de hoek en andere parameters.
- **Wiel**: invoer van muiswiel of styluswiel.
- **Richting**: hoek van de slagrichting.
- **Vervagen**: vervaag de dekking of grootte over een vast aantal scharren.

Elke dynamische invoer kan onafhankelijk aan meerdere eigenschappen worden toegewezen. Open **Toolopties** → **Dynamiek** om te configureren.

#### Vervagen en kleuren

In de uitbreiding **Fade and Colour** (genest in Lijneffecten; alleen zichtbaar wanneer **Dynamics System** is ingeschakeld):

- **Relatieve initiële hoek**: de **Initiële hoek**-waarde wordt geïnterpreteerd ten opzichte van de lijnrichting in plaats van als een absolute canvashoek.
- **Initiële hoek vervagen**: Vervaagt van de **Initiële hoek** bij het begin van de slag naar de live dynamische hoek in de loop van de streek. Als u dit inschakelt, wordt **Relatieve initiële hoek** ingeschakeld.
- **Initiële hoek** (-180–180°): De penseelhoek helemaal aan het begin van een streek, voordat de dynamiek het overneemt.
- **Angle Blend Factor** (0,0–1,0): bepaalt hoe snel de penseelhoek overgaat van de initiële hoek naar de dynamische hoek. 0 = houdt de beginhoek vast; 1 = gebruikt onmiddellijk de volledig dynamische hoek.
- **Richtingstabilisatie** (0–100 px): Vertraagt ​​richtinggevoelige dynamiek doordat de aanwijzer dit aantal pixels moet afleggen voordat de lijnrichting wordt bijgewerkt. Alleen actief als **Post Process** is uitgeschakeld (Post Process zorgt voor zijn eigen stabilisatie). 0 = uitgeschakeld (directe richting, kan springen bij het begin van de slag).
- **Vervagingslengte**: afstand in canvaseenheden waarover de vervaging zich afspeelt.
- **Herhalen**: hoe de fade wordt herhaald zodra de fade-lengte is uitgeput (Geen, Loop, Sawtooth, Triangle).


### OpzetborstelsOpzetborstels plaatsen meerdere onafhankelijke opzetborstels op een cirkelvormige **baanring** gecentreerd op het streekpad. Elke kop schildert een volledige schar op zijn eigen positie, elke keer dat de streek verder gaat, waardoor meerdere parallelle of uitwaaierende bewegingen tegelijk worden geproduceerd.

De baanradius wordt bepaald door de globale penseelgrootte minus de kopgrootte: grotere koppen zitten dichter bij het midden; kleinere hoofden draaien verder naar buiten. De hoofden hebben een gelijkmatige ruimte rond de ring. Met twee koppen krijg je er één aan elke kant van de streek, waardoor een symmetrische spreiding ontstaat die zich gedraagt ​​als een kalligrafiepunt. De schuifregelaar **Volgt richting** draait de hele ring zodat deze loodrecht op de lijn blijft staan, zodat de punt op natuurlijke wijze de richting volgt terwijl u schildert. Door meer koppen toe te voegen, worden ze geleidelijk rond de ring verspreid, tot een volledige spuitcirkel bij 16.

De bedieningselementen verschijnen in de uitbreiding **Borstelkoppen** in het paneel met gereedschapsopties.

- **Aantal**: aantal gelijktijdige opzetborstels (1–16).
- **Grootte**: weergegeven grootte van elk hoofd ten opzichte van de globale penseelgrootte (0,1–1,0).
- **Hoek** (0–360°): Statische oriëntatie van de formatiering, gebruikt wanneer **Volg richting** lager is dan 1,0.
- **Drukvariatie**: variatie in grootte per hoofd toegepast als een onafhankelijke drukafwijking via de dynamische curven.
- **Dekkingsvariatie**: De dekkingsvariatie per hoofd, onafhankelijk van de groottevariatie.
- **Stijfheid**: hoe strak de straal van de baan de dynamisch geschaalde penseelgrootte volgt. 0 = baan volgt de dynamische grootte; 1 = baan blijft gefixeerd op de basisgrootte.
- **Volgt richting** (0,0–1,0): Hoe sterk de formatiering de bewegingsrichting van de slag volgt. Bij 1,0 staat de ring altijd loodrecht op de rijrichting; bij 0,0 wordt de waarde vergrendeld op de statische **Hoek**-waarde.
- **Karakterzaad** (0–255): Vaste zaadwaarde voor karakter per hoofd (grootte, spreidingspositie, bereik). Hetzelfde zaad reproduceert elke slag dezelfde formatie. Gedesensibiliseerd wanneer **Random Head Character** is ingeschakeld.

#### Interpolatie

Verplaatst de koppen langs en rond het slagpad bij elke aanraking, waardoor uitstrijk- en sprayeffecten ontstaan.

- **Overshoot** (0–5): Scatters gaan naar voren in de rijrichting. Bij 1,0 worden de koppen gespreid tot één volledig dab-afstandsinterval vooruit; waarden boven 1,0 maken een groter bereik mogelijk met een sterke zeldzaamheidsbias.
- **Undershoot** (0–5): Hetzelfde als Overshoot, maar loopt achter op de huidige schar. Gecombineerd met Overshoot ontstaat er een leidende uitstrijkje of komeetstaart. Onderdrukt bij de eerste schar om retrograde artefacten te voorkomen.
- **Spuithoek** (0–90°): Waait elke kop vanuit de slagrichting naar buiten met een willekeurige hoek per kop tot deze waarde. Geklemd tot 90° zodat geen enkel hoofd ooit naar achteren wijst. Standaard: 10°.
- **Spray Seed** (0–255): Vast zaad voor spuithoeken per kop, onafhankelijk van Character Seed. Ongevoelig wanneer **Willekeurig spuitpatroon** is ingeschakeld.

#### Randomisatie

- **Willekeurig hoofdkarakter**: tekent de karakterwaarden per hoofd opnieuw (grootte, spreidingspositie, bereik) bij elke schar, zodat de formatie langs de slag volledig chaotisch is. Overschrijft **Karakterzaad**.
- **Willekeurig spuitpatroon**: Hertekent de spuithoeken bij elke druppel, zodat de ventilator continu langs de slag beweegt ("levende spray"). Overschrijft **Spuitzaad**.
- **Willekeurige animatieframes**: voor geanimeerde penselen: elk hoofd verplaatst zijn animatieframe onafhankelijk.

### Extra opties

In de uitbreiding **Aanvullende opties** (standaard samengevouwen):- **Vergrendelen om te bekijken**: Houdt de weergave van het penseel vast ten opzichte van de canvasweergave: wanneer u het canvas roteert, draait het penseel mee.
- **Eenvoudige penseelgrens**: gebruikt een gewone cirkel voor de penseelcursoromtrek in plaats van de volledige penseelvorm weer te geven. Handig voor complexe of grote penselen waarbij het moeilijk is om een ​​nauwkeurige grens te tekenen.
- **Uniforme Jitter**: Indien ingeschakeld, worden de schar-offsets van de **Jitter**-schuifregelaar uit een uniforme verdeling gehaald (elke offset is even waarschijnlijk binnen het bereik). Wanneer uitgeschakeld, is de verdeling Gaussiaans (de verschuivingen clusteren naar het midden).
- **Laatst gebruikte kleuren herstellen**: herstelt de voorgrond- en achtergrondkleuren van de vorige sessie bij het opstarten, in plaats van standaard zwart en wit te gebruiken.
- **Willekeurig horizontaal**: 50% kans om elke stempel van links naar rechts per schar te spiegelen.
- **Willekeurig verticaal**: 50% kans om elke stempel per keer ondersteboven te draaien.
- **Willekeurige rotatie**: Draait elke stempel willekeurig met 0°, 90°, 180° of 270° per schar.
- **Animatie opnieuw instellen**: voor geanimeerde penselen: indien ingeschakeld, begint de animatie bij elke nieuwe streek opnieuw vanaf frame 0; wanneer uitgeschakeld, gaat het verder vanaf het punt waar de vorige slag eindigde.