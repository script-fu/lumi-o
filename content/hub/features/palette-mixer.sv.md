---
title: "Palettmixer"
type: docs
---
Palettmixern hämtar nya färger från par av palettposter med hjälp av en fast trestegspipeline. Eftersom blandning sker i den spektrala domänen snarare än i RGB, beter sig resultaten som fysiska pigment: blått och gult producerar gröna, mättade färger förskjuts mot neutrala när de blandas.

## Pipeline

Varje färg som produceras av mixern går igenom tre steg i en fast ordning:

1. **Blandning**: Spektral WGM mellan förälder A (CCW) och förälder B (CW).
2. **Chroma**: Blanda mot palettens neutrala spektrum, vilket minskar mättnaden.
3. **Ton**: Blanda mot att blanda vitt (ton) eller blanda svart (nyans).

Tonen appliceras alltid sist. Detta gör ljusheten dominerande: en tonjustering landar på exakt den avsedda ljushetsnivån utan att spädas ut av kromajusteringen som föregår den.

## Välja föräldrar

Förälder A och Förälder B är de två poster som blandningsreglaget blandar mellan. De laddas från palettkartan:

- Håll ned **Skift** på palettkartan och **vänsterklicka** för att ställa in förälder A (CCW).
- Håll ned **Skift** och **högerklicka** för att ställa in Parent B (CW).

Endast **Klass A**-bidrag (primärer och anpassade blandningar med intakt härkomst) accepteras som föräldrar. Tertiärer och poster med förlorade anor är undantagna.

Mixerns överordnade A- och överordnade B-positioner visas på kartan som **diamantring**-höjdpunkter så att du alltid kan se vilka poster som är laddade.

## Sliders

| Skjutreglage | Effekt |
| :--- | :--- |
| **Blandning** | Flyttar mellan förälder A (moturs ände) och förälder B (moturs ände). Vid 0,0 matchar resultatet förälder A; vid 1.0 matchar det förälder B. |
| **Chroma** | Avmättar blandningen mot palettens neutrala. Högre värden ger mer dämpade, jordnära resultat. |
| **Ton** | Ändrar ljusheten mot att blanda vitt (nyansriktning) eller blanda svart (nyansriktning). |

## Värdekontroller

**Värdelås** fryser den perceptuella ljusheten (CIE L\*) på sin nuvarande nivå medan de andra reglagen rör sig. Använd detta för att utforska färg- eller nyansvariationer utan att ändra värdet på en mix.

**Band Clamp** begränsar resultatet till att hålla sig inom gränserna för dess nuvarande värdeband (t.ex. inom Lower Mid). Tonreglaget är fortfarande dragbart men utmatningens lätthet är fastspänd.

Tonreglaget återspeglar också alla värdegap som konfigurerats i Palett Editor. Ljushetsintervall som faller inuti ett mellanrum visas som halvtransparenta gråa band på skjutrännan. Skjuthandtaget hoppar automatiskt över dessa luckor: genom att dra genom ett grått område hoppar du till närmaste giltiga bandgräns på andra sidan.

## Blanda slutpunkter (vit, svart, neutral)

Ton- och färgstadierna kräver referensslutpunkter: en blandningsvit, en blandningssvart och en neutral. Lumi upptäcker dessa automatiskt genom att söka i den aktiva paletten efter de bästa kandidaterna:

- **Mixing White**: den primära med högst kromatografi som är närmast ren vit.
- **Mixing Black**: den primära med lägsta lätthet.
- **Neutral**: Primären närmast akromatisk (lägst kromatisk).

Dessa kan åsidosättas manuellt genom att högerklicka på en post i palettredigeraren.

## Spara en blandningKlicka på **Lägg till i palett** för att spara det aktuella mixerresultatet som en **Sparad blandning** (anpassad post). Innan du sparar använder systemet **Best-Match Relocation**: det söker igenom paletten efter det optimala receptet som ger samma slutliga färg med den bästa rumsliga passningen på palettkartan. Om ett närmare recept hittas, hoppar mixerreglagen för att reflektera det, vilket bekräftar att systemet hittat ett bättre ursprung och den sparade postens position kommer att vara i linje med dess visuella punkt på kartan.

Sparade blandningar lagrar det fullständiga receptet (förälder A/B UID, blandningsfaktor, ton, färg) så att de kan reproduceras exakt.

## Receptåterställning

Genom att en gång klicka på en anpassad post i palettredigeraren återställs postens recept i mixern:

- Förälder A och förälder B laddas om.
- Reglagen för blandning, ton och färg återgår till sina ursprungliga positioner.
- Alla värdelås eller bandklämmor som var aktiva under skapandet återaktiveras.

Detta gör det enkelt att återgå till en färg och justera den ytterligare, eller att använda den som utgångspunkt för en ny blandning.