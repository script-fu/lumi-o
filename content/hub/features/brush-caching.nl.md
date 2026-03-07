---
title: "Borstelcaching"
type: docs
---
Brush caching is ontworpen om je favoriete penselen zo snel mogelijk snel te laten aanvoelen. In plaats van steeds opnieuw dezelfde getransformeerde penseelstempel te berekenen, kan Lumi een opgeslagen cache bewaren van de penseelvormen die u daadwerkelijk gebruikt en die cache later automatisch opnieuw laden.

## Overzicht

De functie is opgebouwd rond het idee dat veel expressieve penselen tijdens het schilderen nog steeds dezelfde praktische combinaties van grootte, hoek, hardheid en beeldverhouding gebruiken. Wanneer deze combinaties opnieuw worden gebruikt, kan Lumi de getransformeerde penseelstempel rechtstreeks vanuit de cache weergeven in plaats van deze opnieuw op te bouwen.

Het resultaat is:

- sneller opstarten nadat een cache is opgeslagen
- soepeler herhaald gebruik van favoriete presets
- minder verspilde herberekening tijdens lange schildersessies
- automatisch herstel van opgeslagen caches wanneer de preset opnieuw wordt gebruikt

## Intentie

Penseelcaching is bedoeld voor penselen waar u vaak naar terugkeert: basisvoorinstellingen voor schilderen, favoriete inktgereedschappen, droge penselen met textuur en andere penselen waarvan de getransformeerde stempels duur genoeg zijn om op te merken.

Het doel is niet om elke theoretische penseeltoestand voor te bakken. Het doel is om het echte schildergebruik eerst de meest waardevolle statussen te laten vullen en vervolgens de gevulde cache op te slaan, zodat het penseel de volgende keer dat u het gebruikt al warm is.

## Hoe het werkt

Brush-caching werkt samen met penseelkwantisering.

Wanneer kwantisering is ingeschakeld voor een dynamiekvoorinstelling, worden transformatie-beïnvloedende uitgangen omgezet in discrete stappen. Dat geeft Lumi een eindige reeks herbruikbare penseelstatussen. Terwijl je schildert:

1. Lumi controleert of de getransformeerde stempel al in de cache bestaat.
2. Als dit het geval is, wordt de stempel onmiddellijk opnieuw gebruikt.
3. Als dit niet het geval is, bouwt Lumi het eenmalig en slaat het op.
4. Na verloop van tijd vult de cache zich met de penseelstatussen die u daadwerkelijk gebruikt.

Als je die cache opslaat, kan Lumi deze later automatisch laden, zodat het penseel dichter bij een opgewarmde staat begint, in plaats van alles helemaal opnieuw op te bouwen.

## Typische workflow

1. Kies een penseelvoorinstelling die u vaak gebruikt.
2. Schakel kwantisering in voor de dynamiek ervan.
3. Schilder een tijdje normaal, zodat de cache organisch wordt gevuld.
4. Open de **Tool Preset Editor** en inspecteer de sectie **Preset Cache**.
5. Bekijk de live statistieken:
   - ** Hitpercentage **
   - **Dekking**
   - **Geheugen**
6. Klik op **Opslaan** als de cache de moeite waard lijkt.
7. Bij latere sessies laadt Lumi automatisch de opgeslagen cache wanneer de voorinstelling actief wordt.

Hierdoor voelt de preset sneller snel aan, vooral bij penselen met dure transformaties of grote stempels.

## Waar je het kunt vinden

### Dynamische editor

Gebruik de **Dynamics Editor** om de kwantisering te regelen:

- Kwantisering inschakelen
- kies het globale aantal stappen
- optioneel het aantal stappen per uitgangsas overschrijven

Kwantisering maakt de cache praktisch door de continue variatie in herbruikbare bakken te verminderen.

### Editor voor gereedschapsvoorinstellingen

Gebruik de **Tool Preset Editor** om de cache voor de huidige voorinstelling te beheren:

- **Opslaan** — bewaar de huidige cache in het geheugen op schijf
- **Laden** — herstel een eerder opgeslagen cache
- **Gratis geheugen** — maak de cache in het geheugen vrij zonder de opgeslagen kopie te verwijderen
- **Verwijderen** — verwijder de opgeslagen cache van schijf

De **Preset Cache**-expander toont ook het live-hitpercentage, de dekking en het geheugengebruik.

## Wat wordt in de cache opgeslagen

Penseelcaching richt zich op getransformeerde penseelstempels: de dure gerasterde resultaten nadat grootte, hoek, hardheid, beeldverhouding en gerelateerde transformatie-invoer zijn opgelost.

Het is het nuttigst wanneer:- de borstel heeft kostbaar transformatiewerk
- dezelfde preset wordt in veel sessies gebruikt
- het penseel bezoekt herhaaldelijk soortgelijke dynamische toestanden
- snelle opstartreacties zijn belangrijk

Het is minder nuttig voor penselen waarvan de transformatiestatus wild verandert en zelden wordt herhaald.

## Automatisch laden

Opgeslagen caches zijn bedoeld om vanaf het begin van een sessie te helpen, niet alleen nadat je al een tijdje aan het schilderen bent.

Wanneer er een opgeslagen cache bestaat voor de actieve voorinstelling, kan Lumi deze automatisch laden, zodat uw favoriete penseel begint met veel nuttige statussen die al beschikbaar zijn. Dat verkort de koudestartperiode en zorgt ervoor dat de borstel onmiddellijk dichter bij het maximale reactievermogen komt.

## Geheugenveiligheid

Brush caching is ontworpen om de snelheid te verbeteren zonder de machine over te nemen.

Lumi houdt het cachegeheugengebruik bij, maakt dit zichtbaar in de gebruikersinterface en past runtimelimieten toe onder geheugendruk. Als het systeem te weinig beschikbaar RAM-geheugen heeft, wordt de cachegroei automatisch beperkt.

## Beste gebruiksscenario's

Brush caching is vooral goed voor:

- favoriete dagelijkse chauffeursborstels
- getextureerde penselen die in een schilderij worden gebruikt
- grote expressieve penselen met hoge transformatiekosten
- penseelvoorinstellingen gedeeld in herhaalde illustratieworkflows
- presets waarvan u zich "klaar" wilt voelen zodra u ze selecteert

## In het kort

Met penseelcaching leert Lumi welke penseelstatussen u daadwerkelijk gebruikt, slaat u deze op en haalt u ze later automatisch terug. Het is een praktische snelheidsfunctie voor favoriete presets: schilder met het penseel, laat de cache vullen, sla deze op en toekomstige sessies starten sneller.