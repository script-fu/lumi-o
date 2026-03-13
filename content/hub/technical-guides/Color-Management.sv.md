---
title: "Färghantering"
type: docs
weight: 15
---
Lumi-o är konfigurerad för att fungera direkt. Så länge du arbetar på en bild med **16-bitars eller högre precision**, är programvaran redan inställd för att använda standard buntade mjuksäkring (CMYK) och inbyggda sRGB-profiler; allt borde bara fungera utan någon konfiguration.

För dem som behöver djupare kontroll, förklarar den här guiden Lumis kärnfärghanteringsmodell, skillnaden mellan en bildprofil och en mjukbeständig profil, var kontrollerna finns och exakt hur standardprofilerna buntas ihop med applikationen.

## Snabb sammanfattning

Lumi använder tre olika profilroller:

1. **Bild arbetsprofil**
   - Definierar vad bildens RGB- eller gråskalenummer betyder.
   - Används för att tilldela/konvertera operationer.
   - Typiska exempel: inbyggd sRGB, Adobe RGB.

2. **Visa profil**
   - Beskriver din bildskärm.
   - Används för att visa bilden korrekt på din skärm.
   - Vanligtvis tillhandahålls av systemet eller väljs i Inställningar.

3. **Mjuktät profil**
   - Simulerar en annan utenhet eller utskriftsvillkor.
   - Omdefinierar **inte** bildens pixelvärden.
   - Typiska exempel: CMYK-pressprofiler som `CoatedFOGRA39`.

## Bildprofil vs mjuksäker profil

### Bildprofil

Använd detta när du vill berätta för Lumi vilken färgrymd bilden faktiskt befinner sig i.

Två vanliga operationer:

- **Tilldela profil**
  - Ändrar profiletiketten som är kopplad till bilden.
  - Konverterar **inte** pixelvärden.
  - Använd endast när pixelnumren redan finns i profilens utrymme.

- **Konvertera till profil**
  - Konverterar pixelvärden från den aktuella bildprofilen till en ny.
  - Använd när du vill att bilden verkligen ska flytta in i en annan arbetsyta.

**Menyplatser:**
- Bild > Färghantering > Tilldela färgprofil...
- Bild > Färghantering > Konvertera till färgprofil...

### Mjuksäker profil

Använd detta när du vill förhandsgranska hur bilden skulle reproduceras på en målenhet eller utskriftsvillkor.

Mjuktätning:
- lämnar bildens arbetsutrymme ifred
- ändrar förhandsgranskningens pipeline
- kan markera färger utanför omfånget
- är avsedd för förhandsgranskning, inte omtilldelning av bilddata

**Menyplatser:**
- Bild > Färghantering > Soft-Proof Settings > Välj Soft-Proof Profile...
- Bild > Färghantering > Soft-Proof Settings > Rendering Intent
- Bild > Färghantering > Soft-Proof Settings > Black Point Compensation
- Visa > Färghantering > Aktivera Soft-Proof Preview
- Visa > Färghantering > Markera utanför färgomfånget

## Så här ser du förhandsgranskningen av mjukt

Det finns två huvudsakliga ingångspunkter för att växla mjuka korrektur.

### 1. Visa-menyn

Använd:
- Visa > Färghantering > Aktivera Soft-Proof Preview

Detta slår på eller av förhandsvisningssimuleringen för den aktuella skärmen.

### 2. Växla statusfält

Lumi exponerar även mjukskydd direkt i den nedre statusraden.

- **Vänsterklicka** (växla): aktivera eller inaktivera korrekturfärger
- **Högerklicka**: öppna soft-proofing popover där du kan justera:
  - aktuell profil
  - profilväljare
  - återgivningsuppsåt
  - Svartpunktskompensation
  - markering utanför omfånget

{{< callout type="warning" >}}
**Viktig anmärkning om precision**
Mjuksäker förhandsgranskning är endast aktiverad för **16-bitars och 32-bitars**-bilder.
För **8-bitars** bilder är växlingen inaktiverad och Lumi kommer att uppmana dig att konvertera precision till ett högre djup först innan du förhandsgranskar färgerna korrekt.
{{< /callout >}}

## Inställningar och standardinställningar

Globala standardvärden finns i:
- Redigera > Inställningar > FärghanteringRelevanta avsnitt:
- **Manuell monitorprofil**
- **Önskad RGB-profil**
- **Rekommenderad gråskaleprofil**
- **Mjuktätning**

### Aktuella Lumi-standardinställningar

#### Arbetsytor

Buntade ICC:er för arbetsutrymmen som för närvarande erbjuds från mappen med delad data:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

För standard sRGB-arbete tillhandahåller Lumi även en **inbyggd sRGB-arbetsprofil internt**.

#### Soft-Proof Defaults

Medföljande mjuktäta profiler installerade för närvarande:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

När tillgängligt används `CoatedFOGRA39.icc` som standard medföljande mjukbevis/CMYK-referensprofil.

## Praktiska arbetsflöden

### För målning och normalt skärmarbete

- Behåll bilden i inbyggd sRGB eller annan giltig RGB-arbetsyta.
- Låt Lumi använda systemmonitorprofilen om tillgänglig.

### För förhandsgranskning

- Behåll bilden i dess vanliga RGB-arbetsutrymme.
- Välj en mjukbeständig profil som matchar målutskriftsvillkoret (t.ex. FOGRA39).
- Aktivera mjuksäker förhandsgranskning.
- Aktivera omfångsvarningar om du vill se klippta renderingsavsikter.