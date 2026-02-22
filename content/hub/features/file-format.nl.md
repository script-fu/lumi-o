---
title: "Bestandsformaat (.lum)"
type: docs
---
Lumi gebruikt een open, op directory's gebaseerd bestandsformaat (`.lum`) dat is ontworpen voor prestaties, betrouwbaarheid en langdurige toegankelijkheid.

## Overzicht

Een `.lum` bestand is eigenlijk een directory met daarin:
- **Metadata** (lagen, overvloeimodi, eigenschappen).
- **Laagbuffers** (individuele pixelgegevens voor elke laag).
- **Maskers** (grijswaardengegevens voor laagmaskers).
- **Herstelgeschiedenis** (incrementele snapshots).

Deze structuur maakt snel opslaan, lui laden van grote bestanden en herstel van werk mogelijk, zelfs na een crash.

## Belangrijkste eigenschappen

### Open en leesbaar

Het `.lum`-formaat maakt gebruik van XML-metagegevens en gecomprimeerde binaire buffers. U kunt de laagstructuur, eigenschappen en overvloeimodi in platte tekst inspecteren. Geen eigen codec; pixelgegevens worden opgeslagen in het standaard GEGL-bufferformaat.

### Incrementele besparing

Incrementeel opslaan moet per project worden ingeschakeld in het dialoogvenster **Opslaan als** (een selectievakje **Incrementeel opslaan** en een draaiknop **Max. opslag**). Eenmaal ingeschakeld, schrijft Ctrl+S alleen de gewijzigde lagen in plaats van het hele project te herschrijven, waardoor de tijdwinst drastisch wordt verminderd. De instelling wordt bij het project opgeslagen en blijft gedurende sessies behouden.

### Lui laden

Grote projecten gaan snel open. Laagpixels worden alleen van schijf geladen wanneer:
- De laag wordt zichtbaar gemaakt.
- Je schildert op de laag.
- De laag wordt geëxporteerd of samengesteld.

Zeer grote projecten (meer dan 500 lagen, meerdere gigabytes aan gegevens) blijven responsief. Lui laden is standaard ingeschakeld en kan worden gewijzigd in **Bewerken → Voorkeuren → Prestaties → Geheugenbronnen**.

### Automatisch opslaan

Lumi slaat wijzigingen automatisch met regelmatige tussenpozen op een **afzonderlijke cachelocatie** (`~/.cache/lumi/autosave/`) op. Automatisch opslaan is onafhankelijk van het werkbestand en wijzigt dit niet. Het interval en de cachelocatie kunnen worden geconfigureerd in **Bewerken → Voorkeuren → Prestaties**.

## Toegang

### Opslaan en opslaan als

- **Bestand** → **Opslaan** (Ctrl+S): Opslaan in de huidige map `.lum`.
- **Bestand** → **Opslaan als** (Shift+Ctrl+S): Opslaan in een nieuw `.lum` bestand. Het dialoogvenster Opslaan als bevat opties voor het compressietype en een schakelaar **Incrementeel opslaan** (met een limiet **Max. opslag**) om incrementeel opslaan voor dit project in of uit te schakelen.

Niet-opgeslagen wijzigingen worden aangegeven met een asterisk (*) in de venstertitel.

### Exporteren

- **Bestand** → **Exporteren als** (Shift+Ctrl+E): Exporteren naar PNG, JPEG, TIFF of andere formaten.
- **Bestand** → **Overschrijven** (Ctrl+E): Opnieuw exporteren naar het laatst geëxporteerde bestand.

Bij het exporteren worden zichtbare lagen afgevlakt en wordt de spectrale kleurruimte naar sRGB geconverteerd.

### Importeren

- **Bestand** → **Open** (Ctrl+O): Laad een `.lum` project.
- **Bestand** → **Openen als lagen** (Shift+Ctrl+O): Importeer `.lum`-, XCF- of PSD-bestanden als nieuwe lagen.
- **Bestand** → **Recente bestanden**: snelle toegang tot recent geopende projecten.

PSD- en XCF-bestanden worden bij het importeren geconverteerd naar het oorspronkelijke Lumi-formaat.

## Import- en exportcompatibiliteit

### Ondersteunde importformaten
- **.lum**: eigen Lumi-formaat.
- **.xcf**: eigen GIMP-indeling (lagen en basiseigenschappen blijven behouden).
- **.psd**: Photoshop-formaat (lagen en overvloeimodi behouden).
- **PNG, JPEG, TIFF, enz.**: Import van afgeplatte afbeeldingen.

### Ondersteunde exportformaten
- **PNG**: verliesvrij, met alfatransparantie.
-**JPEG**: verliesgevend, afgeplat.
- **TIFF**: verliesvrij of LZW-gecomprimeerd.
- **XCF**: GIMP-compatibiliteitsformaat. Alleen exporteren; lagen en basiseigenschappen behouden.

## ProjectherstelLumi onderhoudt automatische opslag op de achtergrond en handmatige incrementele controlepunten, beide toegankelijk via **Bestand** → **Afbeelding herstellen**. Zie de [File Recovery](../recovery) pagina voor volledige details.

## Organisatie

Een `.lum` bestand is een directory met een vaste structuur:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Laagbuffers worden genoemd naar de laag (`layer-Background.geglbuf`), en zijn niet opeenvolgend genummerd. Spaties in laagnamen worden opgeslagen als onderstrepingstekens; groepslagen krijgen het achtervoegsel `-GROUP`. Maskers delen de laagnaam (`mask-Background.geglbuf`).

Elke `recovery/primary-NN.lum/` is een volledige basislijnopslag. Als u vervolgens op Ctrl+S drukt, worden `delta-NNNN.lum/` submappen toegevoegd die alleen de gewijzigde buffers bevatten sinds de laatste basislijn, waardoor de controlepuntopslag snel blijft, ongeacht de projectgrootte.

Automatische opslag volgt dezelfde structuur, maar wordt afzonderlijk opgeslagen in `~/.cache/lumi/autosave/`, waardoor het werkbestand onaangetast blijft.
- **Zeer grote projecten**: een project met meer dan 1000 lagen en terabytes aan gegevens zal het meeste profiteren van lazyloading; De uiteindelijke export naar een plat afbeeldingsformaat kan echter enige tijd duren.
- **Netwerkschijven**: opslaan in op het netwerk gekoppelde mappen wordt ondersteund, maar langzamer dan lokale opslag vanwege I/O-latentie.