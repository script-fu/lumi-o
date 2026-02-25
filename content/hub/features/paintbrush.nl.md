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

Opeenvolgende framesequenties die tijdens de slagen vooruitgaan. Frames kunnen incrementeel worden doorlopen (frame-voortgangen per dab), willekeurig worden geselecteerd per dab, of worden geïndexeerd op basis van dynamiek (druk, snelheid, kanteling, hoek).

## Schildercursor

De cursor past zich aan de huidige gereedschapsstatus aan en geeft duidelijke, contextuele feedback:

- **Penseelomtrek**: de cursor volgt de exacte vorm en grootte van het penseel en geeft een live voorbeeld van waar de verf terechtkomt.
- **Wismodus**: wanneer wissen actief is, verandert de omtrek in een gestippelde cirkel om wisstreken visueel te onderscheiden van verfstreken.
- **Eenvoudige penseelgrens**: voor complexe of zeer grote penselen waarbij het weergeven van de nauwkeurige omtrek kostbaar is, schakelt u **Eenvoudige penseelgrens** in (in Extra opties) om in plaats daarvan een gewone cirkel te gebruiken.

## Gereedschapsopties

### Bediening op het hoogste niveau

Altijd aanwezig, buiten elke expander:
- **Modus**: Verfovervloeimodus (Normaal, Vermenigvuldigen, Scherm, enz.)
- **Dekking**: algehele dekking van de lijn (0–100).

### Penseeleigenschappen

In de uitbreiding **Penseeleigenschappen** (standaard uitgevouwen):
- **Grootte**: Penseeldiameter in pixels.
- **Beeldverhouding**: De penseelvorm platdrukken of uitrekken (-1,0–1,0). 0 = ongewijzigd; negatieve waarden roteren de pompoen 90°.
- **Hoek**: Roteert de penseelstempel (-180–180°). Onafhankelijk van de dynamiek van de slagrichting.
- **Hardheid**: zachte vervaging (0,0) tot scherpe rand (1,0).
- **Spatiëring**: afstand tussen geverfde klodders als percentage van de penseelgrootte. Lager = vloeiendere bewegingen; hoger = verspreid patroon.
- **Textuurafwijking**: verteken de respons van de stempeltextuur; 50 is neutraal. Lagere waarden bevorderen het uiteenvallen van de textuur en een afgestreken oppervlak door naar de teen van de waardecurve te trekken; hogere waarden klemmen zich vast in de richting van vaste vullingen door naar de schouder te duwen. Het zichtbare effect is afhankelijk van het toonbereik van de textuur.
- **Jitter**: Verschuift willekeurig elke schar-positie met maximaal dit aantal pixels (0-1024).
- **Gum**: Groottevermenigvuldiger wordt toegepast wanneer dit penseel als gum wordt gebruikt (0,1–10,0). Niet weergegeven op het gummetje zelf.

### Dynamiek

In de **Dynamics**-expander:
- **Dynamiek**: Master-inschakeling voor de actieve dynamiekvoorinstelling.
- **Dynamics Preset**: Selecteert welke invoertoewijzingen worden gebruikt.
- **Vermenigvuldigen met druk**: schakelaar voor extra drukvermenigvuldiging (weergegeven wanneer Dynamics is ingeschakeld).### Beroertegedrag
In de uitbreiding **Beroertegedrag**:
- **Opbouw**: wanneer deze optie is ingeschakeld, wordt bij elke schar de dekking opgebouwd in plaats van dat deze als één enkele streek wordt samengesteld.
- **Postproces**: past stabilisatie, snelheidscompressie en herhalingscorrectie toe nadat de slag is voltooid, waardoor de consistentie zonder latentie wordt verbeterd.
  - **Draaidrempel**: Hoekdrempel (0–180°) voor richtingscorrectie bij scherpe hoeken. 0 = fix richting overslaan.
  - **Voorbeelddrempel**: onderdrukt het nabewerkingsvoorbeeld wanneer de slagsnelheid deze waarde overschrijdt (0 = altijd voorbeeld).

#### Kalligrafisch

Wanneer actief, wordt dab stamping vervangen door een doorlopende geometrische gang:
- **Dynamische dekking**: moduleert de dekking binnen de streek op basis van snelheids- en richtingsveranderingen. Werkt het beste bij fijne, gecontroleerde bewegingen; de resultaten zijn minder voorspelbaar bij snelle krabbels. Experimenteel.
- **Snelheidsgroei** (0–100%): Maximaal toegestane toename van de grootte per monster als percentage van de grootte van het vorige monster. Beperkt hoe snel een snelheidsgedreven maatdynamiek kan groeien, waardoor plotselinge sprongen worden voorkomen wanneer de slag versnelt.
- **Snelheidskrimp** (0–100%): Maximaal toegestane afname van de grootte per monster. Beperkt hoe snel de maat kan afnemen als de slag vertraagt.

#### Stabilisatie en verzachting

- **Richtingsstabilisatieafstand** (0–100 px): minimale beweging van de wijzer voordat richtingsgevoelig gedrag begint, waardoor vroege hoeksprongen worden voorkomen.

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

### Slagmodulatie

In de **Stroke Modulation**-expander (alleen weergegeven als **Dynamics** is ingeschakeld):- **Relatieve initiële hoek**: de **Initiële hoek**-waarde wordt geïnterpreteerd ten opzichte van de lijnrichting in plaats van als een absolute canvashoek.
- **Initiële hoek vervagen**: Vervaagt van de **Initiële hoek** bij het begin van de slag naar de live dynamische hoek in de loop van de streek. Als u dit inschakelt, wordt **Relatieve initiële hoek** ingeschakeld.
- **Initiële penseelhoek** (-180–180°): De penseelhoek helemaal aan het begin van een streek, voordat de dynamiek het overneemt.
- **Initiële hoekovervloeiing** (0,0–1,0): bepaalt hoe snel de penseelhoek overgaat van de initiële hoek naar de dynamische hoek. 0 = houdt de beginhoek vast; 1 = gebruikt onmiddellijk de volledig dynamische hoek.
- **Vervagingslengte**: afstand in canvaseenheden waarover de vervaging zich afspeelt.
- **Herhalen**: hoe de fade wordt herhaald zodra de fade-lengte is uitgeput (Geen, Loop, Sawtooth, Triangle).


### Opzetborstels

Opzetborstels plaatsen meerdere onafhankelijke opzetborstels op een cirkelvormige **baanring** gecentreerd op het streekpad. Elke kop schildert een volledige schar op zijn eigen positie, elke keer dat de streek verder gaat, waardoor meerdere parallelle of uitwaaierende bewegingen tegelijk worden geproduceerd.

De baanradius wordt bepaald door de globale penseelgrootte minus de kopgrootte: grotere koppen zitten dichter bij het midden; kleinere hoofden draaien verder naar buiten. De hoofden hebben een gelijkmatige ruimte rond de ring. Met twee koppen krijg je er één aan elke kant van de streek, waardoor een symmetrische spreiding ontstaat die zich gedraagt ​​als een kalligrafiepunt. De schuifregelaar **Volg richting** draait de hele ring zodat deze loodrecht op de lijn blijft staan, zodat de punt op natuurlijke wijze de richting volgt terwijl u schildert. Door meer koppen toe te voegen, worden ze geleidelijk rond de ring verspreid, tot een volledige spuitcirkel bij 16.

De bedieningselementen verschijnen in de uitbreiding **Borstelkoppen** in het paneel met gereedschapsopties.

- **Aantal**: aantal gelijktijdige opzetborstels (1–16).
- **Kopgrootte**: weergegeven grootte van elke kop in verhouding tot de globale penseelgrootte (0,1–1,0).
- **Baan-aspectratio** (0,1–1,0): Vormt de formatiebaan van cirkel naar ellips. 1,0 = cirkelvormige baan; lagere waarden verpletteren de secundaire as.
- **Formatiehoek** (0–360°): Statische oriëntatie van de formatiering, gebruikt wanneer **Volgrichting** lager is dan 1,0.
- **Volg richting** (0,0–1,0): Hoe sterk de formatiering de slagrichting volgt. Bij 1,0 staat de ring altijd loodrecht op de rijrichting; bij 0,0 wordt de waarde vergrendeld op de statische **Formatiehoek**-waarde.
- **Drukvariatie**: variatie in grootte per hoofd toegepast als een onafhankelijke drukafwijking via de dynamische curven.
- **Dekkingsvariatie**: De dekkingsvariatie per hoofd, onafhankelijk van de groottevariatie.

#### Verstrooiing

Belangrijkste spreidingsregelaars in de uitbreiding **Opzetborstels**:

- **Verspreidingshoek** (0–360°, standaard 10°): Roteert alleen de willekeurige spreidingscomponent (niet de vulafstand). De hoeken per kop/per schar zijn naar buiten gericht met gecontroleerde crossover om stijve gespiegelde pluimen te voorkomen. Geklemd tot 360°.
- **Verspreidingsafstand** (0–10.000 px): willekeurige voorwaartse verplaatsing vanaf de vulafstandpositie van elke kop. Elke keer opnieuw gerold.
- **Scatter Size Balance** (0,0–1,0): regelt de onderdrukkingssteilheid voor heads boven de drempel. Bij 1,0 verspreiden alle hoofden zich gelijkmatig; lagere waarden onderdrukken in toenemende mate grotere koppen, terwijl koppen op/onder de drempel op volledige verstrooiingsafstand blijven.

### Extra opties

In de uitbreiding **Aanvullende opties** (standaard samengevouwen) zijn de bedieningselementen gegroepeerd als overloopsecties die minder vaak worden gewijzigd. Hierdoor blijven de belangrijkste expanders gefocust op vaak aangepaste verfbedieningen.#### Penseeleigenschappen (overloop)
- **Hoek aan schermruimte vergrendelen**: Vergrendelt de penseelhoek aan de schermruimte, zodat de hoek waterpas blijft terwijl het canvas draait/omkeert. Geen effect wanneer Dynamics de hoek regelt.
- **Random Flip Horizontal**: 50% kans om elke stempel van links naar rechts te spiegelen per schar.
- **Random Flip Vertical**: 50% kans om elke stempel ondersteboven te draaien per schar.
- **Willekeurige rotatie**: Draait elke stempel willekeurig met 0°, 90°, 180° of 270° per schar.
- **Uniforme Jitter**: Indien ingeschakeld, worden de schar-offsets van de **Jitter**-schuifregelaar uit een uniforme verdeling gehaald (elke offset is even waarschijnlijk binnen het bereik). Wanneer uitgeschakeld, is de verdeling Gaussiaans (de verschuivingen clusteren naar het midden).
- **Animatie opnieuw instellen**: voor geanimeerde penselen: indien ingeschakeld, begint de animatie bij elke nieuwe streek opnieuw vanaf frame 0; wanneer uitgeschakeld, gaat het verder vanaf het punt waar de vorige slag eindigde.

#### Opzetborstels (overloop)

Vorming:
- **Stijfheid borstelharen**: Hoe strak de baanradius de op dynamische schaal geschaalde penseelgrootte volgt. 0 = baan breidt zich uit en trekt samen met druk; 1 = baan blijft gefixeerd op de basisgrootte.
- **Vulafstand** (0,0–1,0): Verspreidt de hoofden over de opening tussen opeenvolgende scharposities. De stabiele karakterwaarde van elk hoofd bepaalt de richting waarin het hoofd leunt; bij 1,0 koppen vult u het volledige tussenruimte-interval. Karakter is stabiel per zaadje.

Verstrooien:
- **Verstrooiingsgroottedrempel** (0,01–100 px): drempelradius voor volledige verstrooiingsafstand. Koppen op of onder deze straal gebruiken de volledige verstrooiingsafstand; grotere koppen worden geleidelijk dichter bij de slag getrokken.

Randomisatie:
- **Karakterzaad** (0–255): Vaste zaadwaarde voor karakters per hoofd (grootte, vulafstand). Hetzelfde zaad reproduceert elke slag dezelfde formatie. Gedesensibiliseerd wanneer **Hoofdkarakter willekeurig maken** is ingeschakeld.
- **Hoofdkarakter willekeurig maken**: Tekenwaarden per hoofd (grootte, spreidingspositie) worden bij elke stempel opnieuw getekend, zodat de formatie langs de streek volledig chaotisch is. Overschrijft **Karakterzaad**.
- **Animatieframes willekeurig maken**: voor geanimeerde penselen: elk hoofd verplaatst zijn animatieframe onafhankelijk.

#### Beroertegedrag (overflow)

- **Laatst gebruikte kleuren herstellen**: herstelt de voorgrond- en achtergrondkleuren van de vorige sessie bij het opstarten, in plaats van standaard zwart en wit te gebruiken.
- **Eenvoudige penseelgrens**: gebruikt een gewone cirkel voor de penseelcursoromtrek in plaats van de volledige penseelvorm weer te geven. Handig voor complexe of grote penselen waarbij het moeilijk is om een ​​nauwkeurige grens te tekenen.