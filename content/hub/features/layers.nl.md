---
title: "Lagen en niet-destructieve bewerking"
type: docs
---
Het lagensysteem van Lumi maakt complexe, niet-destructieve workflows mogelijk met volledige controle over overvloeien, maskeren en compositie.

## Overzicht

Lagen vormen de basis van gestructureerde illustraties. Elke laag is onafhankelijk, met zijn eigen overvloeimodus, dekking en optioneel laagmasker. Groepen kunnen lagen hiërarchisch nesten met hun eigen overvloei- en uitknipeigenschappen.

## Toegang

**Panelen** → **Lagen**, of het standaardpaneel **Lagen** aan de rechterkant.

## Laagtypen

### Verflagen
Standaard rasterlagen voor geschilderde inhoud. Bewaar pixelgegevens als GEGL-buffers met optionele alfa-transparantie.

### Groepslagen
Hiërarchische containers voor het organiseren van gerelateerde lagen. Groepen kunnen hun eigen overvloeimodus, dekking en uitknipmaskers hebben. Groepsprojecties worden op aanvraag samengesteld.

### Laagmaskers
Grijswaardenmaskers die aan elke laag zijn bevestigd en de dekking per pixel regelen. Als u met wit op een masker schildert, worden pixels ondoorzichtig; zwart maakt ze transparant; grijs zorgt voor gedeeltelijke dekking.

## Overvloeimodi

Elke laag heeft een overvloeimodus die bepaalt hoe deze wordt gecombineerd met de onderliggende lagen:

- **Normaal**: Directe dekkingsovervloeiing.
- **Vermenigvuldigen**: donkerder maken door kleurwaarden te vermenigvuldigen.
- **Scherm**: lichter maken door omkeren, vermenigvuldigen en nog eens omkeren.
- **Overlay**: combinatie van vermenigvuldigen en scherm.
- **Toevoegen**: additiefmenging (sommeert kleurwaarden).
- **Aftrekken**: Subtractief overvloeien.
- **Kleur, tint, verzadiging, lichtheid**: vermenging van HSL-componenten.

## Knippen en maskeren

- **Samengestelde modus — Clip naar achtergrond**: Door de samengestelde modus van een laag in te stellen op **Clip naar achtergrond** wordt de compositie beperkt tot gebieden waar de verzamelde **Union**-lagen hieronder dekking hebben bereikt. De laag tekent alleen daar waar die lagen inhoud hebben; de alfavoetafdruk kan niet worden vergroot. Dit wordt per laag ingesteld in het dialoogvenster Laagkenmerken (vervolgkeuzelijst **Samengestelde modus**). Wanneer de effectieve samengestelde modus van een laag iets anders is dan Samenvoegen, wordt het oogpictogram in het deelvenster Lagen vervangen door een samengesteld pictogram om het niet-standaard compositiegedrag aan te geven.

  **Voorbeeld: gedeelde alfavorm:** In een groep bevat de onderste laag een gevulde cirkel op een transparante achtergrond, ingesteld op de standaard samengestelde modus **Union**. Elke laag erboven in dezelfde groep is ingesteld op **Clip to Background**. Die lagen kunnen alleen schilderen waar de cirkel dekking biedt: één vorm, veel lagen. Dit is een gebruikelijk patroon voor kleuren, schaduwen en details binnen een gedefinieerd silhouet zonder dat u zich zorgen hoeft te maken over morsen.
- **Laagmaskers**: pas een grijswaardenmasker toe om de zichtbaarheid van de laag pixel voor pixel te regelen. Wit schilderen op het masker onthult; zwarte huiden; grijs zorgt voor gedeeltelijke dekking.
- **Pure-Child Masks**: Maskers worden opgeslagen als onderliggende elementen in de tekenbare stapel, waardoor gegevensverlies tijdens transformaties wordt voorkomen.

## Lagen kiezen (Alt-toets)

Als u op **Alt** (Alt links) tikt terwijl u over het canvas zweeft, selecteert u de laag met zichtbare pixels onder de cursor - zonder van gereedschap te wisselen of te klikken.

### Hoe het werkt

- **Druk op Alt**: de cursor verandert in een draadkruis, wat aangeeft dat de selectiemodus actief is.
- **Alt loslaten**: Lumi kiest de bovenste niet-transparante laag op de cursorpositie (dekking > 25%) en selecteert deze. De laag wordt gemarkeerd in het deelvenster Lagen en de statusbalk toont **"Gekozen laag: 'laagnaam'"**.
- Er wordt een handvat getekend op het middelpunt van de gekozen laag op het canvas. Het handvat wordt kleiner en vervaagt naarmate de cursor weggaat.

### Fietsen door lagenElke volgende Alt-tap op dezelfde locatie kiest de **volgende laag naar beneden** in de stapel op dat punt. Lumi onthoudt de laatst gekozen laag en springt er voorbij naar de laag eronder. Zodra de onderkant van de stapel is bereikt, gaat de volgende tik terug naar de bovenste laag op die positie. Dit maakt het eenvoudig om geneste lagen in complexe scènes te bereiken door herhaaldelijk op Alt te tikken.

### Annuleringsregels

De keuze wordt geannuleerd (wordt niet geactiveerd bij het loslaten van Alt) als een van de volgende situaties zich voordoet terwijl Alt wordt vastgehouden:

- Er wordt een muisknop ingedrukt (links- of rechtsklikken).
- Er wordt op een andere toets gedrukt.

Dit zorgt ervoor dat Alt-sleepbewegingen (zoals het aanpassen van de penseelgrootte) en Alt-gewijzigde snelkoppelingen werken zonder per ongeluk de actieve laag te wijzigen.

### Beperkingen

- Het selecteren van lagen wordt niet geactiveerd tijdens bewerkingen met het gereedschap **Transformeren** — Alt heeft daar een andere betekenis.
- Er wordt niet gepickt als er een zwevende selectie aanwezig is.
- Alleen Alt links activeert het picken; right Alt wordt behandeld als een standaardmodificator.

## Operaties

In het deelvenster Lagen:

- **Laag maken**: klik met de rechtermuisknop → **Nieuwe laag**, of gebruik het menu **Laag**.
- **Dupliceren**: klik met de rechtermuisknop → **Dupliceren** of **Laag** → **Dupliceren**.
- **Verwijderen**: klik met de rechtermuisknop → **Verwijderen**, of selecteer en druk op **Verwijderen**.
- **Herordenen**: sleep lagen omhoog of omlaag om de stapelvolgorde te wijzigen.
- **Hernoemen**: dubbelklik op de laagnaam.
- **Omlaag samenvoegen**: klik met de rechtermuisknop → **Omlaag samenvoegen** om te combineren met de onderliggende laag.
- **Afbeelding afvlakken**: **Afbeelding** → **Afbeelding afvlakken** om alle zichtbare lagen samen te voegen.

## Laageigenschappen

- **Dekking**: 0–100%, bepaalt de algehele laagtransparantie.
- **Overvloeimodus**: vervolgkeuzemenu om te selecteren hoe de laag wordt gecombineerd met de onderliggende lagen.
- **Zichtbaar/Verborgen**: Oogpictogram schakelt de zichtbaarheid van lagen in of uit.

## Laagvergrendelingen

Vergrendelingspictogrammen worden weergegeven in de koprij van het deelvenster Lagen. Elk slot kan afzonderlijk worden geschakeld. Als u met de rechtermuisknop op een slotpictogram klikt, wordt dit exclusief ingesteld (vergrendelt alleen dat type en ontgrendelt alle andere op dezelfde laag).

- **Lock Alpha**: Voorkomt schilderen op transparante gebieden. Penseelstreken hebben alleen invloed op pixels die al dekking hebben; volledig transparante pixels worden niet gewijzigd. Handig voor het schilderen binnen bestaande vormen zonder erbuiten te morsen.

- **Masker vergrendelen**: Voorkomt het bewerken van het laagmasker. Het masker blijft zichtbaar en actief, maar kan niet worden beschilderd of gewijzigd zolang dit slot is ingeschakeld.

- **Vergrendelkleur**: vergrendelt het schilderen op een specifieke kleur: de huidige voorgrondkleur op het moment dat de vergrendeling wordt toegepast. Volgende streken op deze laag gebruiken die opgeslagen kleur, ongeacht de actieve voorgrondkleur. Bij het ontgrendelen wordt de opgeslagen kleur verwijderd.

- **Inhoud vergrendelen** (pixels vergrendelen): voorkomt alle pixelbewerkingen in de laag. De laag kan niet worden beschilderd, opgevuld, getransformeerd of anderszins gewijzigd. Handig voor het beschermen van afgewerkte lagen.

- **Vergrendelpositie**: Voorkomt dat de laag wordt verplaatst of getransformeerd. De laag kan nog steeds worden bewerkt; alleen positionele wijzigingen (gereedschap Verplaatsen, gereedschap Transformeren) worden geblokkeerd.

- **Zichtbaarheid vergrendelen**: voorkomt dat het oogpictogram de zichtbaarheid van de laag wijzigt. Beschermt lagen die tijdens het bewerken altijd zichtbaar (of verborgen) moeten blijven.

Alle vergrendelingen worden bij het project opgeslagen en blijven gedurende sessies bestaan.

## Laageffecten (fx)

Niet-destructieve GEGL-filters die via het **Filters**-menu worden toegepast, worden opgeslagen als vastgelegde effecten op de laag in plaats van dat pixels onmiddellijk worden gewijzigd. Wanneer een laag ten minste één vastgelegd effect heeft, verschijnt er een **fx**-pictogram in het deelvenster Lagen naast die laag.### Toegang tot de effectenpop-up

Klik op het pictogram **fx** in een laagrij in het deelvenster Lagen om de popover **Laageffecten** voor die laag te openen.

De popover toont de filterstapel voor de laag: elk vastgelegd effect wordt op naam vermeld met een zichtbaarheidsschakelaar ernaast.

### Bediening

- **Zichtbaarheidsoog** (bovenaan pop-up): schakelt alle effecten tegelijkertijd in of uit.
- **Zichtbaarheid per filter**: elke filterrij heeft zijn eigen oogpictogram om dat effect onafhankelijk in of uit te schakelen.
- **Bewerken**: Opent het instellingendialoogvenster voor het geselecteerde filter, waardoor de parameters niet-destructief kunnen worden aangepast.
- **Verhogen/verlagen**: verplaatst het geselecteerde filter omhoog of omlaag in de stapel, waarbij de volgorde wordt gewijzigd. De effecten worden toegepast.
- **Samenvoegen**: alle momenteel zichtbare effecten worden vastgelegd in de pixels van de laag, waardoor de wijzigingen permanent worden. Het fx-pictogram wordt verwijderd als alle effecten zijn samengevoegd. Samenvoegen is niet beschikbaar op groepslagen.
- **Verwijderen**: verwijdert het geselecteerde filter volledig. De popover sluit automatisch als er geen effecten meer zijn.

Als u dubbelklikt op een filter in de lijst, wordt ook het bijbehorende bewerkingsdialoogvenster geopend.

**Bewerken** en **Verwijderen** zijn geblokkeerd als Pixels vergrendelen actief is op de laag. Filters kunnen niet opnieuw worden gerangschikt terwijl er één actief wordt bewerkt.

### Effecten toevoegen

Pas een filter toe vanuit **Filters** → (elke categorie). Als de actieve laag het doelwit is en de bewerking niet-destructief wordt uitgevoerd, wordt het resultaat opgeslagen als een laageffect in plaats van ingebakken in de pixelgegevens. Het fx-pictogram verschijnt op de laag als er ten minste één effect aanwezig is.

## Dialoogvenster Laagkenmerken

Dubbelklik op een laag in het deelvenster Lagen om het dialoogvenster Laagkenmerken te openen.

### Identiteit

- **Kleurtag**: Kleurlabel voor visuele organisatie in het deelvenster Lagen.

### Samengestelde ruimte en modus

- **Samengestelde ruimte**: de kleurruimte die wordt gebruikt bij het samenstellen van deze laag met onderliggende lagen. Opties: Automatisch, Lineair (RGB), Perceptueel (RGB).
- **Samengestelde modus**: bepaalt hoe de alfa van de laag interageert met de achtergrond. Opties zijn onder meer Samenvoegen (van invloed op alle gebieden – de standaard voor de normale modus), Knippen naar achtergrond (is alleen van invloed op gebieden met bestaande inhoud – de standaard voor de meeste andere overvloeimodi) en Snijpunt.

### Grootte en offsets

Voor een bestaande laag toont **Afmetingen** de laagafmetingen en maskerafmetingen (als er een masker is gekoppeld) als alleen-lezen labels.

**Laagverschuivingen** — X- en Y-spinners bepalen de positie van de laag op het canvas. Wijzigingen zijn onmiddellijk van toepassing in plaats van bij het sluiten van het dialoogvenster.

Als de laag een masker heeft, worden hieronder **Maskeroffsets** — X- en Y-spinners voor de onafhankelijke positie van het masker — weergegeven.

Wanneer u een nieuwe laag maakt, vervangen de velden Breedte en Hoogte en een vervolgkeuzelijst **Vullen met** (Voorgrond, Achtergrond, Wit, Transparant) de alleen-lezen formaatweergave.

### Laagkenmerken (Persistente parasieten)

Het onderste gedeelte van het dialoogvenster bevat een schuifbare tabel Naam/Waarde voor persistente parasieten: willekeurige sleutelwaarde-metagegevens die aan de laag zijn gekoppeld. Deze waarden worden bij het project opgeslagen en zijn toegankelijk via de Scheme-scriptinterface.

- Klik op een cel in de kolom Naam of Waarde om deze inline te bewerken.
- **Toevoegen**: voegt een nieuwe lege rij toe.
- **Verwijderen**: verwijdert de geselecteerde rij en zijn parasiet uit de laag.

Als de laag geen persistente parasieten bevat, worden drie lege startrijen weergegeven.

### InhoudsstatusEen alleen-lezen inforegel onderaan toont de huidige inhoudsstatus van de laag (en het masker, indien aanwezig): **Helder**, **Uniform** of **Gemengd**. Een voorvoegsel `*` geeft aan dat de laag niet-opgeslagen wijzigingen bevat sinds de laatste opslag.

## Prestaties

- **Snelle modus**: bij het schilderen op een enkele laag die in een groep is genest, schakelt Lumi tijdelijk vooroudergroepen over naar pass-through-weergave voor de duur van de streek, waarbij de volledige hercompositie van de groepsprojectie wordt overgeslagen. Dit elimineert geneste vertraging bij het bijwerken van de projectie tijdens het inkten en schilderen. De volledige compositie wordt hervat wanneer de streek eindigt, de actieve laag verandert of voordat wordt opgeslagen.

  De snelle modus is uitgeschakeld wanneer een van de volgende voorwaarden van toepassing is op een vooroudergroep:
  - De groep heeft zichtbare niet-destructieve filters (filters hebben de projectiebuffer nodig).
  - De mengmodus van de groep is iets anders dan **Normaal** of **Pass-through**.
  - De groep heeft een direct kind dat de samengestelde modus **Clip to Background** of **Intersection** gebruikt (hiervoor zijn achtergrondgegevens uit de projectiebuffer vereist).

  De snelle modus wordt ook niet geactiveerd voor lagen op het hoogste niveau, zwevende selecties of wanneer meerdere lagen tegelijkertijd worden gebruikt.

  Door bestanden te structureren om deze omstandigheden in tekengroepen te vermijden, met behulp van de normale overvloeimodi op lagen, zorgt u ervoor dat de snelle modus actief blijft tijdens een inkt- of schildersessie.
- **Lazy Loading**: grote projecten worden snel geladen; laaggegevens worden alleen geladen wanneer dat nodig is (bijvoorbeeld wanneer ze zichtbaar worden gemaakt of erop worden geschilderd).

## Bestandsformaat

Alle lagen, maskers en eigenschappen worden opgeslagen in Lumi's open `.lum` formaat. Het bestand is een map die individuele laagbuffers en metagegevens bevat, waardoor compatibiliteit en toegankelijkheid op de lange termijn worden gegarandeerd.