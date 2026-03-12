---
title: "Bestandsherstel"
type: docs
---
Lumi onderhoudt twee onafhankelijke herstelsystemen (automatische opslag op de achtergrond en handmatige incrementele controlepunten), beide toegankelijk vanuit één enkel dialoogvenster.

## Toegang

**Bestand** → **Afbeelding herstellen**

Het dialoogvenster wordt geopend met vooraf ingevulde herstelstatussen voor het momenteel geopende bestand. Gebruik de bestandskiezer bovenaan om naar een ander `.lum` bestand te gaan.

---

## Automatisch opslaan

Lumi slaat tijdens het bewerken regelmatig een momentopname op de achtergrond van uw werk op. Autosaves worden naar een **afzonderlijke cachemap** geschreven, waardoor het werkende `.lum`-bestand onaangeroerd blijft:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

De padcodering gebruikt `~` als scheidingsteken om een unieke cachemap per bestand te maken. Dit betekent dat automatische opslag beschikbaar is, zelfs als het projectbestand zelf verloren of beschadigd is.

- **Frequentie**: configureerbaar in **Bewerken** → **Voorkeuren** → **Prestaties** → Interval voor automatisch opslaan.
- **Opslaglocatie**: ook ingesteld in Voorkeuren → Prestaties.
- **Doel**: Crashherstel. Het tabblad Automatisch opslaan in het dialoogvenster Afbeelding herstellen toont de beschikbare statussen voor automatisch opslaan met tijdstempels.

Wanneer u een bestand opent dat nieuwere gegevens voor automatisch opslaan bevat, waarschuwt Lumi u tijdens het openen.

---

## Incrementele besparingen

Incrementeel opslaan is een handmatig controlepuntsysteem dat **in het projectbestand** is opgeslagen onder `recovery/`. De structuur is:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+I)
      ├── delta-0001.lum/   (Ctrl+I checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Een nieuwe `primary-NN.lum/` basislijn wordt geschreven na **Bestand → Opslaan**. Volgende drukken op **Bestand → Stapgrootte opslaan** (`Ctrl+I`) creëren `delta-NNNN.lum/` submappen die alleen de buffers bevatten die sinds de laatste basislijn zijn gewijzigd. Delta's voor automatisch opslaan en delta's voor handmatig opslaan gebruiken afzonderlijke tellers, zodat ze elkaars geschiedenis niet verstoren.

Opslagverhoging is **altijd beschikbaar** voor opgeslagen `.lum` bestanden:

1. Gebruik **Bestand** → **Opslaan** (`Ctrl+S`) om het hoofdprojectbestand te maken of bij te werken.
2. Gebruik **Bestand** → **Save Increment** (`Ctrl+I`) om een ​​herstelcontrolepunt te maken.
3. Na nog een volledig **Bestand** → **Opslaan**, schrijft de volgende `Ctrl+I` een nieuwe `primary-NN.lum/` basislijn voordat nieuwe delta's worden gemaakt.

Herstelde bestanden met de naam `RECOVERED_` moeten eerst normaal worden opgeslagen voordat Save Increment voor hen beschikbaar wordt.

Wanneer u een `.lum`-bestand opent dat nieuwere incrementele opslagbewerkingen heeft dan de primaire opslagbewerking, toont Lumi een **Incrementele opslagdetectie**-prompt die aanbiedt om het meest recente controlepunt te laden.

---

## Dialoogvenster Afbeelding herstellen

Het dialoogvenster heeft drie tabbladen en twee actieknoppen.

### Tabblad Automatisch opslaan

Geeft een overzicht van alle beschikbare statussen voor automatisch opslaan voor het geselecteerde bestand, met tijdstempels en miniaturen (indien beschikbaar). Selecteer een staat en klik op **Herstellen** om deze te openen.

Gebruik dit tabblad om:
- Herstellen na een crash.
- Terugkeren naar een eerdere status van dezelfde sessie.

### Incrementeel tabblad

Geeft een overzicht van alle controlepuntstatussen die in het projectbestand zijn opgeslagen. Bij elke invoer wordt het tijdstempel van het controlepunt weergegeven. Selecteer een controlepunt en klik op **Herstellen** om het te openen.

Gebruik dit tabblad om:
- Keer terug naar een eerder punt in een sessie zonder afzonderlijke bestanden te hebben opgeslagen.
- Blader door de versiegeschiedenis van een project.

### Nieuwste tabblad

Het standaardtabblad wanneer het dialoogvenster wordt geopend. Identificeert automatisch de nieuwste beschikbare herstelstatus voor zowel automatische opslag als incrementele controlepunten, en toont de tijdstempel ervan. Klik op **Herstellen** om het onmiddellijk te laden zonder door afzonderlijke staten te bladeren.

---

## Knoppen

| Knop | Actie |
|--------|--------|
| **Herstellen** | Opent de geselecteerde herstelstatus als een nieuwe afbeelding. |
| **Sluiten** | Sluit het dialoogvenster af zonder te herstellen. |
| **Oude staten opruimen…** | Opent een opruimprompt (zie hieronder). |

---

## Oude staten opruimen

Het accumuleren van herstelstatussen in de loop van de tijd kan aanzienlijke schijfruimte in beslag nemen. De knop **Oude staten opschonen…** (linksonder in het dialoogvenster) opent een opschoonprompt voor het actieve tabblad (Automatisch opslaan of Incrementeel).

De prompt toont:
- Hoeveel volledige opslagen er zijn voor het bestand.
- De totale schijfruimte die ze in beslag nemen.
- Een draaiknop **Bewaar meest recente** om te selecteren hoeveel saves je wilt behouden.

Door **Keep most recent** in te stellen op `0` worden alle herstelstatussen verwijderd. De volgende `Ctrl+I` na een volledige opschoning zal een nieuwe primaire opslag schrijven.

---

## Opstartherstel

Als Lumi bij het opstarten detecteert dat het meest recent geopende bestand nieuwere automatisch opgeslagen gegevens bevat dan de laatste volledige opslag, wordt er vóór het laden een herstelprompt weergegeven. U kunt accepteren (de automatische opslag laden) of afwijzen (de primaire opslag openen zoals normaal).