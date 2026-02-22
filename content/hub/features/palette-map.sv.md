---
title: "Palettkarta"
type: docs
---
Palettkartan svarar på en praktisk fråga för målare: givet en uppsättning pigment, vilka färger kan egentligen blandas från dem? Med utgångspunkt från palettens ingående pigment, utforskar den varje kombination (två-pigmentblandningar, trevägsblandningar, tonala variationer) och kartlägger resultaten på ett färghjul. Utdata är en bild av den färgrymd som kan nås för den specifika uppsättningen av pigment.

Kartan är också ett koordinatbaserat navigeringsverktyg. Den organiserar varje genererad blandning efter nyans och ljushet i ett cirkulärt rutnät, så att hela paletten är läsbar med en blick och varje färg har en stabil hemadress.

## Rutnätsstruktur

Kartan är uppdelad i ett 36 × 15 rutnät:

- **36 nyanssektorer**: 10° steg runt hjulet, centrerad på de stora nyansnamnen.
- **15 ljushetsceller**: 3 celler per värdeband × 5 band (High Key, Upper Mid, Middle, Lower Mid, Deep), som går från vitt på utsidan till svart i mitten.

Varje cell är en liten kil på hjulet. En post som placeras i en cell sägs ha den cellen som dess **ursprung**: dess logiska hemadress på kartan.

## Färger i celler

När flera färger tävlar om samma cell, visas endast en **vinnare** tydligt:

1. **Primära** bidrag vinner alltid deras cell, oavsett andra passagerare.
2. Om ingen primär är närvarande vinner den genererade mixen (sekundär eller tertiär) med **högst färg**.

Bidrag som inte vinner är tvåa och förblir tillgängliga via klickcykling (se nedan).

Anpassade poster (Sparade mixar) återges som fyrkantiga punkter; genererade mixar och primära renderingar som runda prickar.

## Klicka på Cykling

Genom att klicka på en upptagen cell väljs vinnaren som förgrundsfärg. Genom att klicka på samma cell igen växlas till nästa passagerare (nästan genererade mixar, sedan eventuella anpassade poster sparade på den rutnätsadressen). Varje klick går ett steg genom stapeln.

** Vänsterklicka** leder till förgrunden. När färgmålet är inställt på bakgrund (från verktygslådan), klickar du på väg till bakgrunden istället.

## Skift-Välj: Laddar mixerslutpunkter

Håll ned **Skift** för att gå in i slutpunktsladdningsläge:

- **Vänsterklicka** tilldelar den klickade posten som **Förälder A (CCW)** i Palettmixern.
- **Högerklicka** tilldelar den som **Förälder B (CW)**.

Endast klass A-poster (primärer och anpassade blandningar med intakt härkomst) är valbara i detta läge. Tertiärer är dolda och icke-klass-A-punkter är nedtonade. En kort överlagring bekräftar att läget är aktivt.

## Mixer Parent Highlights

När palettmixern har aktiva slutpunkter för föräldra A och föräldra B, är båda markerade på kartan med **diamantringar** (en diamantform med en svart ram). Dessa höjdpunkter förblir synliga även när andra visningselement växlas, så de aktiva blandningsföräldrarna är alltid identifierbara.

## Ursprung vs visuell position

Varje post har två positioner på kartan:

- **Ursprung (Källcell)**: Den logiska rutnätsadressen som posten tillhör, fast för dess livstid.
- **Visuell punktposition**: Där färgen faktiskt återges baserat på dess perceptuella nyans och ljushet.

Med **Best-Match Relocation**, när en blandning sparas beräknar systemet det optimala receptet för den slutliga färgen och ställer in ursprunget för att matcha färgens visuella position. Detta håller sparade färger nära deras visuella plats på hjulet och gör kartan rumsligt koherent.

## Dra sparade mixar

Anpassade poster (Sparade mixar) kan flyttas genom att dra:1. Klicka och håll på en anpassad post (fyrkantig prick) och dra förbi tröskeln på 5 pixlar.
2. Markören ändras för att indikera dragläge. Föräldrahöjdpunkter uppdateras live när du flyttar över kartan för att visa de nya blandningsföräldrarna vid varje kandidatposition.
3. Den släpade punkten fäster till närmaste giltiga provposition.
4. Släpp för att begå. Posten antar destinationscellens recept: dess föräldrar, blandning, ton och chroma uppdateras för att matcha, och dess ursprung uppdateras för att matcha den nya visuella positionen.

Dra drag kan ångras via **Redigera → Ångra**.

## Dubbelklicka: Växla kartarbetsytan

I **Palettredigeraren**, dubbelklickar du på valfri palettpost för att växla arbetsytan för palettkarta på och av. Detta är ett snabbt sätt att växla mellan att bläddra i sparade färger och att blanda på kartan utan att använda en meny. Enkla klickbeteende (återställning av postens recept i mixern) påverkas inte.

## Canvas Overlay

Palettkartan kan framkallas direkt på bildduken som en helskärmsöverlagring genom att klicka på **Förgrunds-/Bakgrundsrutan** i verktygslådan. Detta ger en stor blandningsyta utan att dedikera en permanent panel till kartan.

## Central Color Swatch

En cirkulär färgruta sitter i mitten av munkhålet och reflekterar färgen på vilken cell som markören är över:

- **Svävarfärg**: när markören vilar på en kartpost uppdateras färgrutan omedelbart för att visa den postens färg.
- **Vald färg som reserv**: när ingen cell förs över, visar färgrutan Palettmixerns beräknade resultat för den för närvarande valda posten. Om mixern inte har löst sig ännu, använder den postens basdisplayfärg så att platsen aldrig blir tom.
- En tunn mörk kant omsluter färgprovet hela tiden.
- Efter att markören hållit sig över den centrala färgrutan en kort stund, visas en vit-svart yttre ring för att signalera att området är interaktivt.
- **Om du klickar på den centrala färgrutan** stängs dukens överlägg och återgår till den normala bildvyn (samma som att klicka utanför den yttre ringen).

## Alt-tangent: Canvas Comparison Mode

När palettkartans duköverlägg är öppet, visar du tillfälligt bilden nedan om du håller **Alt** nedtryckt:

- Hela palettkartans användargränssnitt bleknar till osynligt (dess opacitet sjunker till noll), vilket avslöjar duken.
- En cirkulär färgruta med 64 pixlar följer markören, fylld med Palette Mixers aktuella samplade färg, så att du håller dig medveten om den aktiva mixen medan du inspekterar bilden.
- Genom att släppa Alt återställs palettkartan med full opacitet.

En tipsetikett, *"Håll ned Alt-tangenten för att se bilden"*, visas inuti arbetsytan som en påminnelse.