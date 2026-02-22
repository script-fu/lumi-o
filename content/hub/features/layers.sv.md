---
title: "Lager & icke-förstörande redigering"
type: docs
---
Lumis lagersystem möjliggör komplexa, oförstörande arbetsflöden med full kontroll över blandning, maskering och sammansättning.

## Översikt

Lager är grunden för strukturerad illustration. Varje lager är oberoende, med sitt eget blandningsläge, opacitet och valfri lagermask. Grupper kan kapsla lager hierarkiskt med sina egna blandnings- och klippegenskaper.

## Åtkomst

**Paneler** → **Lager**, eller standardpanelen **Lager** till höger.

## Lagertyper

### Måla lager
Standardrasterlager för målat innehåll. Lagra pixeldata som GEGL-buffertar med valfri alfatransparens.

### Grupplager
Hierarkiska behållare för att organisera relaterade lager. Grupper kan ha sina egna blandningsläge, opacitet och klippmasker. Gruppprognoser är sammansatta på begäran.

### Lagermasker
Gråskalemasker fästa på vilket lager som helst och kontrollerar opaciteten per pixel. Att måla på en mask med vitt gör pixlar ogenomskinliga; svart gör dem genomskinliga; grått ger partiell opacitet.

## Blandningslägen

Varje lager har ett blandningsläge som avgör hur det kombineras med lager nedan:

- **Normal**: Direkt opacitetsblandning.
- **Multiplicera**: Gör mörkare genom att multiplicera färgvärden.
- **Skärm**: Gör ljusare genom att invertera, multiplicera och invertera igen.
- **Overlay**: Kombination av Multiply och Screen.
- **Lägg till**: Additiv blandning (summar färgvärden).
- **Subtrahera**: Subtraktiv blandning.
- **Färg, nyans, mättnad, ljushet**: HSL-komponentblandning.

## Klippning och maskering

- **Kompositläge — Klipp till bakgrund**: Att ställa in ett lagers sammansatta läge till **Klippa till bakgrund** begränsar kompositionen till områden där de ackumulerade **Union**-lagren nedan har etablerat opacitet. Lagret målas bara där dessa lager har innehåll – det kan inte utöka alfaavtrycket. Detta ställs in per lager i dialogrutan Lagerattribut (**Kompositläge** rullgardinsmenyn). När ett lagers effektiva sammansatta läge är något annat än Union, ersätts ögonikonen på panelen Lager med en sammansatt ikon för att indikera det icke-standardiserade sammansättningsbeteendet.

  **Exempel — delad alfaform:** I en grupp innehåller det nedre lagret en fylld cirkel på en genomskinlig bakgrund, inställd på standardläget **Union**. Varje lager ovanför det i samma grupp är inställt på **Klipp till bakgrund**. Dessa lager kan bara måla där cirkeln ger opacitet - en form, många lager. Detta är ett vanligt mönster för färgning, skuggning och detaljering inom en definierad siluett utan att oroa dig för spill.
- **Lagermasker**: Använd en gråskalemask för att kontrollera lagrets synlighet pixel för pixel. Att måla vitt på masken avslöjar; svarta döljer; grått ger partiell opacitet.
- **Pure-Child Masks**: Masker lagras som barn i den ritbara stacken, vilket förhindrar dataförlust under transformationer.

## Lagerval (Alt-tangent)

Om du trycker på **Alt** (vänster Alt) medan du håller muspekaren över duken väljer du lagret med synliga pixlar under markören – utan att byta verktyg eller klicka.

### Hur det fungerar

- **Tryck på Alt**: Markören ändras till ett hårkors, vilket indikerar att plockningsläget är aktivt.
- **Release Alt**: Lumi väljer det översta icke-transparenta lagret vid markörpositionen (opacitet > 25%) och väljer det. Lagret är markerat i lagerpanelen och statusfältet visar **"Layer picked: 'Layer name'"**.
- Ett handtag ritas i mitten av det plockade lagret på duken. Handtaget minskar och bleknar när markören flyttas bort.

### Cykla genom lagerVarje efterföljande Alt-tryck på samma plats väljer **nästa lager ned** i stacken vid den punkten. Lumi kommer ihåg det senast valda lagret och hoppar förbi det till det under. När botten av stapeln är nådd, cyklar nästa tryck tillbaka till det översta lagret vid den positionen. Detta gör det enkelt att nå kapslade lager i komplexa scener genom att trycka på Alt upprepade gånger.

### Avbokningsregler

Valet avbryts (utlöses inte när Alt släpps) om något av följande inträffar medan Alt hålls inne:

- En musknapp trycks ned (vänster eller högerklick).
- Valfri annan knapp trycks ned.

Detta säkerställer att Alt-draggester (som justering av penselstorlek) och Alt-modifierade genvägar fungerar utan att av misstag ändra det aktiva lagret.

### Begränsningar

- Lagerplockning aktiveras inte under **Transform**-verktygsoperationer — Alt har en annan betydelse där.
- Plockning sker inte om ett flytande urval finns.
- Endast vänster Alt utlöser plockning; höger Alt behandlas som en standardmodifierare.

## Operationer

I panelen Lager:

- **Skapa lager**: Högerklicka på → **Nytt lager**, eller använd menyn **Lager**.
- **Duplicera**: Högerklicka på → **Duplicera** eller **Lager** → **Duplicera**.
- **Ta bort**: Högerklicka på → **Ta bort**, eller välj och tryck på **Radera**.
- **Ordna om**: Dra lager uppåt eller nedåt för att ändra staplingsordning.
- **Byt namn**: Dubbelklicka på lagrets namn.
- **Slå samman ned**: Högerklicka på → **Slå samman ned** för att kombinera med lagret nedan.
- **Flätta ut bild**: **Bild** → **Fläta ut bild** för att slå samman alla synliga lager.

## Lageregenskaper

- **Opacitet**: 0–100 %, kontrollerar övergripande lagertransparens.
- **Blandningsläge**: Rullgardinsmeny för att välja hur lagret kombineras med lager nedan.
- **Synlig/Dold**: Ögonikon växlar lagersynlighet.

## Lagerlås

Låsikoner visas i rubrikraden på panelen Lager. Varje lås kan växlas oberoende av varandra. Högerklicka på en låsikon ställer in den exklusivt (låser bara den typen, låser upp alla andra på samma lager).

- **Lås Alpha**: Förhindrar målning på transparenta ytor. Penseldrag påverkar bara pixlar som redan har opacitet; helt genomskinliga pixlar ändras inte. Användbar för att måla i befintliga former utan att spilla utanför dem.

- **Låsmask**: Förhindrar redigering av lagermasken. Masken förblir synlig och aktiv men kan inte målas på eller modifieras medan detta lås är på.

- **Låsfärg**: Låser målningen till en specifik färg — den aktuella förgrundsfärgen i det ögonblick som låset appliceras. Efterföljande streck på detta lager använder den lagrade färgen oavsett den aktiva förgrundsfärgen. Upplåsning kasserar den lagrade färgen.

- **Lås innehåll** (Låsa pixlar): Förhindrar alla pixelredigeringar i lagret. Lagret kan inte målas på, fyllas, transformeras eller på annat sätt modifieras. Användbar för att skydda färdiga lager.

- **Låsposition**: Förhindrar att lagret flyttas eller transformeras. Lagret kan fortfarande redigeras; endast positionsändringar (flytta verktyg, transformeringsverktyg) blockeras.

- **Lås synlighet**: Förhindrar att ögonikonen växlar lagrets synlighet. Skyddar lager som alltid ska vara synliga (eller dolda) under redigering.

Alla lås sparas med projektet och kvarstår över sessioner.

## Lagereffekter (fx)

Icke-förstörande GEGL-filter som appliceras via menyn **Filter** lagras som committerade effekter på lagret istället för att omedelbart ändra pixlar. När ett lager har minst en bekräftad effekt visas en **fx**-ikon i panelen Lager bredvid det lagret.### Åtkomst till Effects Popup

Klicka på ikonen **fx** på en lagerrad i panelen Lager för att öppna popover-fönstret **Lagereffekter** för det lagret.

Popover-fönstret visar filterstapeln för lagret - varje begången effekt listad efter namn med en synlighetsomkopplare bredvid den.

### Kontroller

- **Visibility eye toggle** (överst i popup): Växlar alla effekter på eller av samtidigt.
- **Synlighetsväxling per filter**: Varje filterrad har sin egen ögonikon för att aktivera eller inaktivera den effekten oberoende.
- **Redigera**: Öppnar inställningsdialogrutan för det valda filtret, så att dess parametrar kan justeras oförstörande.
- **Höj/sänk**: Flyttar det valda filtret uppåt eller nedåt i stacken, ändrande ordningseffekter tillämpas.
- **Slå samman**: Överför alla synliga effekter till lagrets pixlar, vilket gör ändringarna permanenta. Fx-ikonen tas bort om alla effekter slås samman. Sammanfogning är inte tillgängligt på grupplager.
- **Ta bort**: Tar bort det valda filtret helt. Popover-fönstret stängs automatiskt om inga effekter kvarstår.

Dubbelklicka på ett filter i listan öppnar också dess redigeringsdialog.

**Redigera** och **Ta bort** är blockerade om Lock Pixels är aktivt på lagret. Filter kan inte ordnas om medan ett aktivt redigeras.

### Lägger till effekter

Använd ett filter från **Filter** → (valfri kategori). Om det aktiva lagret är inriktat och operationen körs oförstörande, lagras resultatet som en lagereffekt snarare än inbakad i pixeldata. Fx-ikonen visas på lagret när minst en effekt finns.

## Dialog för lagerattribut

Dubbelklicka på ett lager i panelen Lager för att öppna dialogrutan Lagerattribut.

### Identitet

- **Färgtagg**: Färgetikett för visuell organisation i panelen Lager.

### Sammansatt utrymme och läge

- **Kompositutrymme**: Färgrymden som används vid sammansättning av detta lager med lager under. Alternativ: Auto, Linjär (RGB), Perceptuell (RGB).
- **Kompositläge**: Styr hur lagrets alfa interagerar med bakgrunden. Alternativen inkluderar Union (påverkar alla områden – standard för normalt läge), Clip to Backdrop (påverkar endast områden med befintligt innehåll – standard för de flesta andra blandningslägen) och Intersection.

### Storlek och förskjutningar

För ett befintligt lager visar **Storlekar** lagerdimensionerna och maskdimensionerna (om en mask är ansluten) som skrivskyddade etiketter.

**Layer Offsets** — X- och Y-spinnare som styr lagrets position på duken. Ändringar gäller omedelbart snarare än när dialogrutan stängs.

Om lagret har en mask visas **Mask Offsets** — X- och Y-spinnare för maskens oberoende position — nedan.

När du skapar ett nytt lager, ersätter fälten för bredd och höjd och en rullgardinsmeny **Fyll med** (Förgrund, Bakgrund, Vit, Transparent) den skrivskyddade storleksvisningen.

### Lagerattribut (ihållande parasiter)

Den nedre delen av dialogrutan innehåller en rullningsbar Namn/Värdetabell för ihållande parasiter – godtycklig nyckel-värde-metadata kopplad till lagret. Dessa värden lagras med projektet och är tillgängliga från Scheme-skriptgränssnittet.

- Klicka på valfri cell i kolumnen Namn eller Värde för att redigera den inline.
- **Lägg till**: Lägger till en ny tom rad.
- **Ta bort**: Tar bort den markerade raden och dess parasit från lagret.

Om lagret inte har några ihållande parasiter visas tre tomma startrader.

### InnehållstillståndEn skrivskyddad informationsrad längst ned visar det aktuella innehållsläget för lagret (och masken, om den finns): **Clear**, **Uniform** eller **Mixed**. Ett `*` prefix indikerar att lagret har osparade ändringar sedan den senaste lagringen.

## Prestanda

- **Snabbläge**: När du målar på ett enstaka lager som är kapslat inuti en grupp, byter Lumi tillfälligt förfädergrupper till genomgångsrendering under linjens varaktighet, och hoppar över hela gruppprojektionsomkompositionen. Detta eliminerar kapslad projektionsuppdateringsfördröjning under färgning och målning. Fullständig sammansättning återupptas när strecket slutar, det aktiva lagret ändras eller innan en lagring.

  Snabbläget är inaktiverat när något av följande villkor gäller för en förfadergrupp:
  - Gruppen har synliga oförstörande filter (filter behöver projektionsbufferten).
  - Gruppens blandningsläge är allt annat än **Normal** eller **Pass-through**.
  - Gruppen har ett direkt underordnat läge som använder **Clip to Backdrop** eller **Intersection** kompositläge (dessa kräver bakgrundsdata från projektionsbufferten).

  Snabbläge aktiveras inte heller för toppnivålager, flytande markeringar eller när flera lager riktas in samtidigt.

  Genom att strukturera filer för att undvika dessa förhållanden i målningsgrupper, med hjälp av normala blandningslägen på lager, säkerställs att snabbläget förblir aktivt under en färg- eller målningssession.
- **Lat laddning**: Stora projekt laddas snabbt; lagerdata laddas endast när det behövs (t.ex. när det görs synligt eller målas på).

## Filformat

Alla lager, masker och egenskaper lagras i Lumis öppna `.lum`-format. Filen är en katalog som innehåller enskilda lagerbuffertar och metadata, vilket säkerställer kompatibilitet och långsiktig tillgänglighet.