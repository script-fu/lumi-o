---
title: "Penseel gereedschap"
type: docs
---
De Paintbrush is het belangrijkste tekengereedschap, ontworpen voor responsief, intelligent penseelwerk met volledige controle over de dynamiek van druk, snelheid, kanteling en afstand.

## Overzicht

Het gereedschap Penseel ondersteunt raster-, procedureel gegenereerde en geanimeerde penseeltypen. Slagen kunnen worden gestabiliseerd, afgevlakt en nabewerkt. De penseeldynamiek reageert op stylusinvoer en geeft nauwkeurige controle over de dekking, grootte, kleur, hoek en andere eigenschappen tijdens een streek.

## Penseeltypen

### Rasterborstels
Bitmappenseelafbeeldingen geladen vanuit `.png`- of `.vbr`-bestanden. Ondersteunt alpha-transparantie en geanimeerde pijpframes.

### Gegenereerde penselen
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


### Opzetborstels
Verf met meerdere onafhankelijke penseelkoppen, gerangschikt in een formatiering rond het streekpad. De bedieningselementen verschijnen in de uitbreiding **Borstelkoppen** in het paneel met gereedschapsopties.- **Koppen**: aantal gelijktijdige opzetborstels (1–16).
- **Grootte**: weergegeven grootte van elk hoofd ten opzichte van de globale penseelgrootte (0,1–1,0).
- **Stijfheid**: hoe strak de straal van de baan de dynamisch geschaalde penseelgrootte volgt. 0 = baan volgt de dynamische grootte; 1 = baan blijft gefixeerd op de basisgrootte.
- **Volgt** (0,0–1,0): Hoe sterk de formatiering de bewegingsrichting van de slag volgt. Bij 1,0 (standaard) staat de ring altijd loodrecht op de rijrichting. Bij 0,0 is deze vergrendeld op de statische **Hoek**-waarde. Tussenliggende waarden vermengen zich tussen de twee oriëntaties. Dit is onafhankelijk van het Dynamics-systeem; er is geen hoekdynamiekconfiguratie vereist.
- **Hoek** (0–360°): Statische oriëntatie van de formatiering, gebruikt wanneer **Volgt** lager is dan 1,0. Wanneer **Vergrendelen om te bekijken** actief is, wordt de hoek automatisch gecompenseerd voor canvasrotatie.
- **Variatie**: variatie in grootte per hoofd en drukvooroordeel toegepast op de dynamiek.
- **Dekkingsvariatie**: De dekkingsvariatie per hoofd, onafhankelijk van de groottevariatie.
- **Zaad**: Vast willekeurig zaad voor variatie per kop. Geldt alleen wanneer **Random Bristles** is uitgeschakeld.
- **Willekeurige borstelharen**: Willekeurig borstelkarakter bij elke streek (negeer Zaad).
- **Onafhankelijke frames**: voor geanimeerde penselen: indien ingeschakeld, verplaatst elk hoofd zijn animatieframe onafhankelijk.

### Extra opties

In de uitbreiding **Aanvullende opties** (standaard samengevouwen):

- **Vergrendelen voor weergave**: houdt het uiterlijk van het penseel vast ten opzichte van de canvasweergave: wanneer u het canvas roteert, draait het penseel mee.
- **Eenvoudige penseelgrens**: gebruikt een gewone cirkel voor de penseelcursoromtrek in plaats van de volledige penseelvorm weer te geven. Handig voor complexe of grote penselen waarbij het moeilijk is om een ​​nauwkeurige grens te tekenen.
- **Uniforme Jitter**: Indien ingeschakeld, worden de schar-offsets van de **Jitter**-schuifregelaar uit een uniforme verdeling gehaald (elke offset is even waarschijnlijk binnen het bereik). Wanneer uitgeschakeld, is de verdeling Gaussiaans (de verschuivingen clusteren naar het midden).
- **Laatst gebruikte kleuren herstellen**: herstelt de voorgrond- en achtergrondkleuren van de vorige sessie bij het opstarten, in plaats van standaard zwart en wit te gebruiken.
- **Willekeurig horizontaal**: 50% kans om elke stempel van links naar rechts per schar te spiegelen.
- **Willekeurig verticaal**: 50% kans om elke stempel per keer ondersteboven te draaien.
- **Willekeurige rotatie**: Draait elke stempel willekeurig met 0°, 90°, 180° of 270° per schar.
- **Animatie opnieuw instellen**: voor geanimeerde penselen: indien ingeschakeld, begint de animatie bij elke nieuwe streek opnieuw vanaf frame 0; wanneer uitgeschakeld, gaat het verder vanaf het punt waar de vorige slag eindigde.