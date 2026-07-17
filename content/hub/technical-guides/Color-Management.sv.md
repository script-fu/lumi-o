---
title: "Färghantering"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
---

Lumi-o är konfigurerad för att fungera direkt. Så länge du arbetar med en bild i **16-bitars eller högre precision** är programvaran redan inställd för att använda standardpaketerad soft proof (CMYK) och inbyggda sRGB-profiler; allt bör fungera utan någon konfiguration.

För dig som behöver mer kontroll förklarar den här guiden Lumis kärnmodell för färghantering, skillnaden mellan en bildprofil och en soft-proof-profil, var kontrollerna finns och exakt hur standardprofilerna levereras med programmet.

## Snabb sammanfattning

Lumi använder tre olika profilroller:

1. **Bildens arbetsprofil**
   - Definierar vad bildens RGB- eller gråskalenummer betyder.
   - Används för tilldelnings- och konverteringsåtgärder.
   - Typiska exempel: inbyggd sRGB, Adobe RGB.

2. **Skärmprofil**
   - Beskriver din bildskärm.
   - Används för att visa bilden korrekt på skärmen.
   - Tillhandahålls vanligtvis av systemet eller väljs i Inställningar.

3. **Soft-proof-profil**
   - Simulerar en annan utenhet eller utskriftsvillkor.
   - Omdefinierar **inte** bildens pixelvärden.
   - Typiska exempel: CMYK-pressprofiler som `CoatedFOGRA39`.

## Bildprofil kontra soft-proof-profil

### Bildprofil

Använd detta när du vill berätta för Lumi vilket färgrymd bilden faktiskt befinner sig i.

Två vanliga åtgärder:

- **Tilldela profil**
  - Ändrar profiletiketten som är kopplad till bilden.
  - Konverterar **inte** pixelvärden.
  - Använd endast när pixelvärdena redan finns i profilens färgrymd.

- **Konvertera till profil**
  - Konverterar pixelvärden från den aktuella bildprofilen till en ny.
  - Använd när du vill att bilden verkligen ska flyttas till en annan arbetsrymd.

**Menyplatser:**
- Bild > Färghantering > Tilldela färgprofil...
- Bild > Färghantering > Konvertera till färgprofil...

### Soft-proof-profil

Använd detta när du vill förhandsgranska hur bilden skulle reproduceras på en målenhet eller under ett utskriftsvillkor.

Soft proof:
- lämnar bildens arbetsrymd orörd
- ändrar förhandsgranskningspipelinen
- kan markera färger utanför gamut
- är avsett för förhandsgranskning, inte omtilldelning av bilddata

**Menyplatser:**
- Bild > Färghantering > Soft-proof-inställningar > Välj soft-proof-profil...
- Bild > Färghantering > Soft-proof-inställningar > Renderingsavsikt
- Bild > Färghantering > Soft-proof-inställningar > Svartpunktskompensation
- Visa > Färghantering > Aktivera soft-proof-förhandsgranskning
- Visa > Färghantering > Markera färger utanför gamut

## Så här visar du soft-proof-förhandsgranskningen

Det finns två huvudsakliga ingångspunkter för att växla soft proof.

### 1. Menyn Visa

Använd:
- Visa > Färghantering > Aktivera soft-proof-förhandsgranskning

Detta slår på eller av förhandsgranskningssimuleringen för den aktuella skärmen.

### 2. Växling i statusfältet

Lumi exponerar soft proofing även direkt i det nedre statusfältet.

- **Vänsterklick** (växla): aktivera eller inaktivera proof-färger
- **Högerklick**: öppna soft-proof-popovern där du kan justera:
  - aktuell profil
  - profilväljare
  - renderingsavsikt
  - svartpunktskompensation
  - markering utanför gamut

{{< callout type="warning" >}}
**Viktig anmärkning om precision**
Soft-proof-förhandsgranskning är endast aktiverad för **16-bitars och 32-bitars** bilder.
För **8-bitars** bilder är växlingen inaktiverad och Lumi uppmanar dig att konvertera precisionen till högre bitdjup innan du kan förhandsgranska färgerna korrekt.
{{< /callout >}}

## Inställningar och standardvärden

Globala standardvärden finns i:
- Redigera > Inställningar > Färghantering

Relevanta avsnitt:
- **Manuell skärmprofil**
- **Föredragen RGB-profil**
- **Föredragen gråskaleprofil**
- **Soft proofing**

### Aktuella Lumi-standardinställningar

#### Arbetsrymder

Paketerade ICC-profiler för arbetsrymder som för närvarande erbjuds från den delade datamappen:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

För standard sRGB-arbete tillhandahåller Lumi även en **inbyggd sRGB-arbetsprofil**.

#### Soft-proof-standardinställningar

Paketerade soft-proof-profiler som för närvarande är installerade:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

När tillgänglig används `CoatedFOGRA39.icc` som standard paketerad soft-proof-/CMYK-referensprofil.

## Praktiska arbetsflöden

### För målning och normalt skärmarbete

- Behåll bilden i inbyggd sRGB eller en annan giltig RGB-arbetsrymd.
- Låt Lumi använda systemets skärmprofil om tillgänglig.

### För utskriftsförhandsgranskning

- Behåll bilden i standard RGB-arbetsrymd.
- Välj en soft-proof-profil som matchar målutskriftsvillkoret (t.ex. FOGRA39).
- Aktivera soft-proof-förhandsgranskning.
- Aktivera valfritt gamut-varningar för att se avklippta renderingsavsikter.
