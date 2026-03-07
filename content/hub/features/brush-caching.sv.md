---
title: "Caching av borstar"
type: docs
---
Brush caching är utformad för att få dina favoritborstar att kännas snabba så tidigt som möjligt. Istället för att räkna om samma transformerade borststämpel om och om igen, kan Lumi behålla en sparad cache över de borstformer du faktiskt använder och ladda om cachen automatiskt senare.

## Översikt

Funktionen är uppbyggd kring idén att många uttrycksfulla penslar fortfarande återvänder till samma praktiska kombinationer av storlek, vinkel, hårdhet och bildförhållande under målning. När dessa kombinationer återanvänds kan Lumi servera den transformerade borststämpeln direkt från cachen istället för att bygga om den.

Resultatet är:

- snabbare slagstart efter att en cache har sparats
- smidigare upprepad användning av favoritförinställningar
- mindre bortkastad omräkning under långa målningssessioner
- automatisk återställning av sparade cachar när förinställningen används igen

## Avsikt

Penselcaching är avsedd för penslar som du ofta återvänder till: förinställningar för kärnmålning, favoritfärgverktyg, torra texturpenslar och andra penslar vars transformerade stämplar är dyra nog att lägga märke till.

Målet är inte att förbaka varje teoretiskt borsttillstånd. Målet är att låta verklig målningsanvändning befolka de mest värdefulla tillstånden först, och sedan spara den fyllda cachen så att borsten redan är varm nästa gång du använder den.

## Hur det fungerar

Brush caching fungerar tillsammans med borstkvantisering.

När kvantisering är aktiverad för en dynamikförinställning, knäpps transformationspåverkande utsignaler till diskreta steg. Det ger Lumi en ändlig uppsättning återanvändbara borsttillstånd. När du målar:

1. Lumi kontrollerar om den transformerade stämpeln redan finns i cachen.
2. Om den gör det återanvänds stämpeln omedelbart.
3. Om den inte gör det bygger Lumi den en gång och lagrar den.
4. Med tiden fylls cachen med de borsttillstånd du faktiskt använder.

Om du sparar den cachen kan Lumi autoladda den senare så att borsten börjar närmare ett uppvärmt tillstånd istället för att bygga om allt från grunden.

## Typiskt arbetsflöde

1. Välj en förinställning för borsten som du använder ofta.
2. Aktivera kvantisering för dess dynamik.
3. Måla normalt ett tag så fylls cachen organiskt.
4. Öppna **Tool Preset Editor** och inspektera avsnittet **Preset Cache**.
5. Titta på livestatistiken:
   - **Träfffrekvens**
   - **Täckning**
   - **minne**
6. Klicka på **Spara** när cachen ser värdefull ut.
7. Vid senare sessioner laddar Lumi automatiskt den sparade cachen när förinställningen blir aktiv.

Detta gör att förinställningen känns snabbare, speciellt för penslar med dyra transformationer eller stora stämplar.

## Var du kan hitta den

### Dynamics Editor

Använd **Dynamics Editor** för att kontrollera kvantisering:

- möjliggör kvantisering
- välj det globala stegräkningen
- Valfritt åsidosätt stegräkningar per utgående axel

Kvantisering är det som gör cachen praktisk genom att minska kontinuerlig variation i återanvändbara fack.

### Tool Preset Editor

Använd **Tool Preset Editor** för att hantera cachen för den aktuella förinställningen:

- **Spara** — behåll den aktuella minnescachen på disken
- **Ladda** — återställ en tidigare sparad cache
- **Fritt minne** — frigör cacheminnet i minnet utan att radera den sparade kopian
- **Ta bort** — radera den sparade cachen från disken

Expanderaren **Preset Cache** visar också live träfffrekvens, täckning och minnesanvändning.

## Vad som cachelagras

Borstcaching riktar sig mot transformerade borststämplar: de dyra rastrerade resultaten efter storlek, vinkel, hårdhet, bildförhållande och relaterade transformationsingångar har lösts.

Det är mest användbart när:- borsten har kostsamt förvandlingsarbete
- samma förinställning används över många sessioner
- borsten återbesöker liknande dynamiska tillstånd upprepade gånger
- snabb uppstartslyhördhet är viktig

Det är mindre användbart för penslar vars transformationstillstånd förändras vilt och sällan upprepas.

## Automatisk laddning

Sparade cacher är avsedda att hjälpa till från början av en session, inte bara efter att du redan har målat ett tag.

När en sparad cache finns för den aktiva förinställningen kan Lumi ladda den automatiskt så att din favoritpensel börjar med många användbara tillstånd som redan är tillgängliga. Det minskar kallstartsperioden och tar borsten närmare maximal respons direkt.

## Minnessäkerhet

Borstcaching är utformad för att förbättra hastigheten utan att ta över maskinen.

Lumi spårar användningen av cacheminnet, exponerar det i användargränssnittet och tillämpar körtidsgränser under minnestryck. Om systemet har ont om tillgängligt RAM, begränsas cachetillväxt automatiskt.

## Bästa användningsfall

Brush caching är särskilt bra för:

- favoritborstar för dagliga förare
- texturerade penslar som används genomgående i en målning
- stora uttrycksfulla borstar med hög transformationskostnad
- Penselförinställningar delas över upprepade illustrationsarbetsflöden
- förinställningar du vill ska kännas "klar" så fort du väljer dem

## Kort sagt

Med penselcache kan Lumi lära sig de borsttillstånd du faktiskt använder, spara dem och ta tillbaka dem automatiskt senare. Det är en praktisk hastighetsfunktion för favoritförinställningar: måla med penseln, låt cachen fyllas, spara den och framtida sessioner startar snabbare.