---
title: "Varpverktyg"
type: docs
---
The Warp Tool pushes, pulls, and flows pixels freely across the canvas. I Lumi går det längre än de flesta implementeringar: det kan förvränga en hel lagergrupp – oavsett hur många kapslade lager och masker den innehåller – som ett enda enhetligt objekt, utan att platta till eller förlora någon struktur.

## Översikt

Select a layer and drag across it to displace pixels in any direction. Varpen är oförstörande medan du arbetar: du kan ångra och göra om enskilda drag, ändra penselstorleken eller beteendet mellan dragen och fortsätta förfina tills du begår. Committing tillämpar den ackumulerade förskjutningskartan destruktivt på lagrets pixeldata.

När ett **grupplager** är valt, fungerar verktyget på gruppen som helhet. Du ser och interagerar med en liveförhandsvisning av hela den sammansatta gruppen. Vid commit appliceras samma varp exakt och oberoende på varje underordnat lager och mask i gruppen, vilket bevarar hela lagerstrukturen.

## Gruppvarp

Warping a group is the primary capability that sets Lumi's warp tool apart.

### Problemet det löser

I de flesta målarprogram kräver förvrängning av en illustration med flera lager antingen att man plattar ut gruppen först (förstör lagerstrukturen) eller att varje lager förvrängs separat och försöker matcha dem med ögat (tråkigt och oprecist). Inget av tillvägagångssätten bevarar den ursprungliga strukturen för ytterligare oförstörande redigering.

Lumi warps the entire group as one item and then distributes the exact same transformation to every layer inside it.

### Hur det fungerar

When you select a group and begin a warp stroke, Lumi builds a **floating preview layer** from the group's composited projection. Om gruppen har en mask, bakas masken in i förhandsgranskningen så att förhandsgranskningen exakt representerar det slutliga utseendet. Du målar dina varpdrag direkt på den här förhandsvisningen - det du ser är precis vad du får.

På commit, Lumi:

1. Tillämpar förskjutningen på varje grundläggande lager i gruppen (inklusive djupt kapslade lager i undergrupper), expanderar varje lagers duk precis tillräckligt för att fånga hela varpområdet.
2. Applicerar samma förskjutning på varje mask i gruppen i samma pass.
3. Återupptar gruppens automatiska gränsberäkning så att gruppen ändrar storlek för att passa sina nyligen skeva barn.
4. Beskär varje skevt lager till dess faktiska målade innehåll för att hålla filstorlekarna kompakta.
5. Tar bort förhandsgranskningsskiktet och återskapar gruppprojektionen från de uppdaterade barnen.

Allt detta sker inom ett enda ångra steg. Efter att ha begått ser gruppen ut exakt som den gjorde i förhandsgranskningen, med varje lager och mask intakta.

### Masker

Alternativet **Warp Masks** (aktiverat som standard) gör att masker på varje lager och grupp inuti varpmålet får samma förskjutningstransformation. Lagermasker rör sig med sina lager: en mask som klippte bort en karaktärs kontur fortsätter att klippa ut samma kontur efter skevning.

När **Warp Masks** är avstängd förskjuts endast lagerinnehåll; masker behåller sina ursprungliga positioner.

## Verktygsalternativ

### Beteende

| Läge | Effekt |
| :--- | :--- |
| **Flytta** | Trycker pixlar i riktningen för linjen. Det primära läget för de flesta skevningsarbeten. |
| **Väx** | Expanderar pixlar utåt från borstens mitt. |
| **Skrymp** | Dra pixlar inåt mot borstens mitt. |
| **Snurra medurs** | Roterar pixlar medurs runt borstens mitt. |
| **Snurra moturs** | Roterar pixlar moturs runt borstens mitt. |
| **Radera** | Tar bort varpförskjutning och återställer pixlar till sina ursprungliga positioner. |
| **Smidig** | Diffuserar förskjutning, mjukar upp abrupta övergångar mellan skeva och oförvridna områden. |

### Borstkontroller

- **Storlek**: Varpborstens diameter i pixlar. Larger brushes displace broader areas with a softer falloff; mindre borstar ger exakt, lokaliserad kontroll.
- **Hårdhet**: Falloff från mitten till kanten. Hög hårdhet ger en jämn förskjutning över hela borstområdet; låg hårdhet koncentrerar effekten i centrum.
- **Styrka**: Hur långt pixlar förskjuts per slag. Lägre styrka tillåter subtil, gradvis formning; higher strength produces dramatic, fast movement.

### Stroke Timing

- **Stroke under rörelse** (endast rörelseläge): Tillämpar warp kontinuerligt när musen rör sig, snarare än bara på en timerpuls. Använd för flytande, penselliknande drag där du vill att förskjutningen ska följa markören direkt.
- **Stroke periodiskt**: Tillämpar varp vid ett fast tidsintervall medan musknappen hålls nedtryckt. Använd för Grow, Shrink och Swirl lägen där kontinuerlig cirkulär applicering är avsikten.
- **Rate**: Frekvensen av periodisk strokeapplicering.

### Kvalitet

- **Interpolation**: Den samplingsmetod som används vid commit. Linjär är snabb och smidig för de flesta arbeten; Cubic och Nohalo ger högre trohet för fina detaljer.
- **Högkvalitetsförhandsgranskning**: Använder commit-kvalitetsprovtagaren under den interaktiva förhandsgranskningen. Långsammare, men förhandsvisningen matchar det engagerade resultatet exakt.

### Gruppalternativ

- **Utöka förvrängningsområde** (endast gruppförvrängning): Antalet pixlar som lagts till som en genomskinlig marginal runt gruppförhandsvisningen på alla sidor. Detta ger förskjutet innehåll utrymme att flytta in i. Standardvärdet 256 px är tillräckligt för de flesta arbeten; minska det för stora bilder där minnet är viktigt, eller öka det för mycket stora förskjutningsslag.
- **Warp Masks**: Om samma varp ska appliceras på lager- och gruppmasker. På som standard.

## Ångra och gör om

Varje slag är ett diskret ångrasteg inom varpsessionen. **Ctrl+Z** tar bort det sista strecket och återställer förskjutningskartan till dess tidigare tillstånd. **Ctrl+Y** (eller **Ctrl+Skift+Z**) tillämpar det igen. You can walk back through the entire stroke history before committing.

Om du trycker på **Escape** eller växlar verktyg förkastas alla oengagerade streck och återställer lagret/lagren till sitt ursprungliga tillstånd. No changes are written until you explicitly commit.

## Engagerar sig

Klicka på knappen **Commit** (eller tryck på **Enter**) för att applicera den ackumulerade warpen destruktivt. For group warps, this triggers the full multi-layer application described above. The undo history for the committed warp is then a single entry in the image undo stack, reversible with the standard **Edit → Undo**.