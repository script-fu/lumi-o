---
title: "Filformat (.lum)"
type: docs
---
Lumi använder ett öppet, katalogbaserat filformat (`.lum`) designat för prestanda, tillförlitlighet och långsiktig tillgänglighet.

## Översikt

En `.lum`-fil är faktiskt en katalog som innehåller:
- **Metadata** (lager, blandningslägen, egenskaper).
- **Lagerbuffertar** (individuella pixeldata för varje lager).
- **Masker** (gråskaledata för lagermasker).
- **Återställningshistorik** (inkrementella ögonblicksbilder).

Denna struktur möjliggör snabb lagring, lat inläsning av stora filer och återställning av arbete även efter en krasch.

## Nyckelegenskaper

### Öppen och läsbar

`.lum`-formatet använder XML-metadata och komprimerade binära buffertar. Du kan inspektera lagerstruktur, egenskaper och blandningslägen i vanlig text. Ingen proprietär codec; pixeldata lagras i standard GEGL-buffertformat.

### Inkrementell besparing

Inkrementell lagring måste vara aktiverad per projekt i **Spara som-dialogen** (en **Inkrementell besparing** kryssruta och en **Max Saves**-snurrknapp). När det är aktiverat skriver Ctrl+S bara de modifierade lagren istället för att skriva om hela projektet, vilket drastiskt minskar tidsbesparingen. Inställningen lagras med projektet och kvarstår över sessioner.

### Lat laddning

Stora projekt öppnar snabbt. Lagerpixlar laddas endast från disk när:
– Lagret görs synligt.
– Man målar på lagret.
- Lagret är exporterat eller sammansatt.

Mycket stora projekt (500+ lager, flera gigabyte data) förblir responsiva. Lazy loading är aktiverat som standard och kan växlas i **Redigera → Inställningar → Prestanda → Minnesresurser**.

### Autospara

Lumi sparar automatiskt ändringar till en **separat cacheplats** (`~/.cache/lumi/autosave/`) med jämna mellanrum. Autosave är oberoende av arbetsfilen och ändrar den inte. Intervallet och cacheplatsen kan konfigureras i **Redigera → Inställningar → Prestanda**.

## Åtkomst

### Spara och spara som

- **Arkiv** → **Spara** (Ctrl+S): Spara i den aktuella `.lum`-katalogen.
- **Fil** → **Spara som** (Skift+Ctrl+S): Spara till en ny `.lum` fil. Dialogrutan Spara som innehåller alternativ för komprimeringstyp och en **Inkrementell spara**-växling (med en **Max Saves**-gräns) för att aktivera eller inaktivera inkrementell lagring för detta projekt.

Osparade ändringar indikeras med en asterisk (*) i fönstrets titel.

### Exportera

- **Arkiv** → **Exportera som** (Skift+Ctrl+E): Exportera till PNG, JPEG, TIFF eller andra format.
- **Fil** → **Skriv över** (Ctrl+E): Exportera om till den senast exporterade filen.

Exporterar plattar ut synliga lager och konverterar från spektral till sRGB-färgrymd.

### Importera

- **Fil** → **Öppna** (Ctrl+O): Ladda ett `.lum`-projekt.
- **Fil** → **Öppna som lager** (Shift+Ctrl+O): Importera `.lum`, XCF eller PSD-filer som nya lager.
- **Fil** → **Senaste filer**: Snabb åtkomst till nyligen öppnade projekt.

PSD- och XCF-filer konverteras till Lumis ursprungliga format vid import.

## Import- och exportkompatibilitet

### Importformat som stöds
- **.lum**: Lumi inbyggt format.
- **.xcf**: Inbyggt GIMP-format (lager och grundläggande egenskaper bevarade).
- **.psd**: Photoshop-format (lager och blandningslägen bevarade).
- **PNG, JPEG, TIFF, etc.**: Import av tillplattad bild.

### Exportformat som stöds
- **PNG**: Förlustfri, med alfatransparens.
- **JPEG**: Förlustig, tillplattad.
- **TIFF**: Förlustfri eller LZW-komprimerad.
- **XCF**: GIMP-kompatibilitetsformat. Endast export; lager och grundläggande egenskaper bevarade.

## ProjektåterställningLumi upprätthåller automatiska bakgrundssparningar och manuella inkrementella kontrollpunkter, båda tillgängliga från **File** → **Återställ bild**. Se [File Recovery](../recovery) sidan för fullständig information.

## Organisation

En `.lum` fil är en katalog med en fast struktur:

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

Lagerbuffertar är namngivna efter lagret (`layer-Background.geglbuf`), inte numrerade i följd. Mellanslag i lagernamn lagras som understreck; grupplager får ett `-GROUP` suffix. Masker delar lagernamnet (`mask-Background.geglbuf`).

Varje `recovery/primary-NN.lum/` är en fullständig baslinjesparning. Efterföljande Ctrl+S-tryckningar lägg till `delta-NNNN.lum/` underkataloger som endast innehåller de modifierade buffertarna sedan den senaste baslinjen, vilket gör att kontrollpunkter sparas snabbt oavsett projektstorlek.

Autosparas följer samma struktur men lagras separat i `~/.cache/lumi/autosave/`, och lämnar arbetsfilen orörd.
- **Mycket stora projekt**: Ett projekt med 1000+ lager och terabyte data kommer att dra mest nytta av lat inläsning; den slutliga exporten till platt bildformat kan dock ta tid.
- **Nätverksenheter**: Spara i nätverksmonterade kataloger stöds men långsammare än lokal lagring på grund av I/O-latens.