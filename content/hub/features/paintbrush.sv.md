---
title: "Penselverktyg"
type: docs
---
Penseln är det primära målningsverktyget, designat för responsivt, intelligent penselarbete med full kontroll över tryck, hastighet, lutning och avståndsdynamik.

## Översikt

Penselverktyget stöder raster, procedurgenererade och animerade penseltyper. Stroke kan stabiliseras, utjämnas och efterbehandlas. Penseldynamik svarar på penninmatning, vilket ger exakt kontroll över opacitet, storlek, färg, vinkel och andra egenskaper under ett slag.

## Borsttyper

### Rasterborstar (.raster)

Bitmappenselbilder som stöder alfatransparens.

### Genererade borstar (.param)

Procedurmässigt återgivna former (cirkel, kvadrat, diamant, triangel) med justerbara parametrar: hårdhet, bildförhållande, vinkel, rundhet och hörnradie. Genererade borstar är lätta och skalbara.

### Animerade borstar (.anim)

Sekventiella bildsekvenser som avancerar under drag. Ramar kan växlas inkrementellt (framflyttning per dab), slumpmässigt väljas per dab eller indexeras av dynamik (tryck, hastighet, lutning, vinkel).

## Målningsmarkör

Markören anpassar sig till det aktuella verktygsläget för att ge tydlig, kontextuell feedback:

- **Penselkontur**: Markören spårar penselns exakta form och storlek, vilket ger en liveförhandsvisning av var färgen kommer att landa.
- **Radera läge**: När radering är aktiv växlar konturen till en streckad cirkel för att visuellt skilja raderingsdrag från målardrag.
- **Enkel borstgräns**: För komplexa eller mycket stora penslar där det är kostsamt att återge den exakta konturen, aktivera **Enkel borstgräns** (i ytterligare alternativ) för att använda en vanlig cirkel istället.

## Verktygsalternativ

### Kontroller på toppnivå

Närvarande hela tiden, utanför alla expanderare:
- **Läge**: Färgblandningsläge (Normal, Multiplicera, Screen, etc.)
- **Opacitet**: Total slagopacitet (0–100).

### Borstalternativ

I expandern **Brush Options** (expanderad som standard):
- **Storlek**: Borstens diameter i pixlar.
- **Ratio**: Squash eller sträck ut penselformen (-1,0–1,0). 0 = oförändrad; negativa värden roterar squashen 90°.
- **Vinkel**: Roterar borststämpeln (-180–180°). Oberoende av slagriktningens dynamik.
- **Avstånd**: Avstånd mellan målade dubbar i procent av penselstorleken. Lägre = jämnare slag; högre = spritt mönster.
- **Hårdhet**: Mjuk blekning (0,0) till skarp kant (1,0).
- **Kraft**: Kraft för applicering av borsten (0,0–1,0). Dolt för pennverktyget.
- **Jitter**: Förskjuter slumpmässigt varje dab-position med upp till så många pixlar (0–1024).
- **Suddgummi**: Storleksmultiplikator tillämpas när denna borste används som suddgummi (0,1–10,0). Visas inte på själva suddgummiverktyget.

### Strokeeffekter

I expandern **Stroke Effects**:
- **Efterprocess**: Tillämpar stabilisering, hastighetskompression och replay-korrigering efter att slaget är klart, vilket förbättrar konsistensen utan latens.
  - **Vridtröskel**: Vinkeltröskel (0–180°) för riktningskorrigering vid skarpa hörn. 0 = hoppa över riktningsfix.
  - **Förhandsgranskningshastighet**: Undertrycker förhandsgranskningen efter bearbetning när slaghastigheten överskrider detta värde (0 = alltid förhandsgranskning).
- **Uppbyggnad**: När den är på, ackumulerar varje dab opacitet snarare än att vara sammansatt som ett enda slag.#### Kalligrafisk
När den är aktiv ersätts dab-stämpling med en kontinuerlig geometrisk korridor:
- **Bredd** och **Höjd**: Mått på den kalligrafiska korridoren.
- **Vinkel**: Spetsorientering (grader).
- **Dynamisk opacitet**: Modulerar opaciteten inom slaget baserat på hastighets- och riktningsändringar. Fungerar bäst på fina, kontrollerade slag; resultaten är mindre förutsägbara på snabba klotter. Experimentell.
- **Velocity Growth** (0–100%): Maximal tillåten storleksökning per prov i procent av föregående provs storlek. Begränsar hur snabbt en hastighetsdriven storleksdynamik kan växa, vilket förhindrar plötsliga hopp när slaget accelererar.
- **Velocity Shrink** (0–100%): Maximal tillåten storleksminskning per prov. Begränsar hur snabbt storleken kan sjunka när slaget bromsar in.

#### Utjämning

Möjliggör realtidsinmatningsutjämning som tillämpas på linjebanan när du målar. Expanderar för att avslöja:
  - **Djup** (2–256): Antal tidigare ingångssamplingar som beaktas vid beräkning av den utjämnade positionen. Högre värden ger en längre, mer engagerad fördröjning.
  - **Position** (0–100): Utjämningsintensitet som appliceras på borstens position. Högre värden avrundar skarpa riktningsförändringar.
  - **Tryck** (0–100): Utjämning appliceras på pennans trycksignal, vilket minskar tryckspikar och jitter.
  - **Riktning** (0–100): Utjämning applicerad på slagriktningen, stabiliserar vinkelkänslig dynamik.

#### Dynamik

Tilldela penninmatning eller andra levande värden till målningsparametrar:

- **Tryck** (penna): Styr storlek, opacitet, hastighet, hårdhet, färg och mer baserat på pennans tryck.
- **Hastighet**: Kartlägger slaghastighet till penselegenskaper.
- **Tilt**: X- och Y-lutningsvinklarna för pennan påverkar vinkeln och andra parametrar.
- **Hjul**: Ingång för mushjul eller pennhjul.
- **Riktning**: Slagriktningsvinkel.
- **Tona**: Tona opacitet eller storlek över ett fast antal dabs.

Varje dynamisk ingång kan mappas till flera egenskaper oberoende av varandra. Öppna **Verktygsalternativ** → **Dynamics** för att konfigurera.

#### Tona och färg

I expandern **Tona och färg** (kapslade inuti Stroke Effects; visas endast när **Dynamics System** är aktiverat):

- **Relativ initialvinkel**: Värdet för **Initialvinkel** tolkas i förhållande till slagriktningen snarare än som en absolut arbetsvinkel.
- **Initial vinkel för tona**: Tonar från **inledande vinkel** vid slagstart mot den dynamiska vinkeln under slaget. Aktivering av detta tvingar på **Relativ initial vinkel**.
- **Initial Angle** (-180–180°): Borstvinkeln i början av ett slag, innan dynamiken tar över.
- **Angle Blend Factor** (0,0–1,0): Styr hur snabbt borstvinkeln övergår från den initiala vinkeln till den dynamiska vinkeln. 0 = håller den initiala vinkeln; 1 = använder omedelbart den helt dynamiska vinkeln.
- **Riktningsstabilisering** (0–100 px): Fördröjer riktningskänslig dynamik genom att kräva att pekaren förflyttar sig så många pixlar innan den uppdaterar slagriktningen. Endast aktiv när **Post Process** är avstängd (Post Process ger sin egen stabilisering). 0 = avaktiverad (omedelbar riktning, kan hoppa vid slagstart).
- **Fade Length**: Avstånd i dukenheter över vilket toningen utspelar sig.
- **Repeat**: Hur toningen upprepas när toningslängden är slut (Ingen, Loop, Sawtooth, Triangle).


### BorsthuvudenBorsthuvuden placerar flera oberoende borsthuvuden på en cirkulär **omloppsring** centrerad på linjebanan. Varje huvud målar en hel klick i sin egen position varje gång slaget avancerar, vilket ger flera parallella eller fläktade drag samtidigt.

Banradien bestäms av den globala borststorleken minus huvudstorleken: större huvuden sitter närmare mitten; mindre huvuden kretsar längre ut. Heads space jämnt runt ringen. Med två huvuden får du ett på varje sida av slaget, vilket skapar en symmetrisk spridning som beter sig som en kalligrafi-nib. Reglaget **Följer riktning** roterar hela ringen för att förbli vinkelrätt mot slaget, så att spetsen följer riktningen naturligt när du målar. Genom att lägga till fler huvuden fläktar de dem gradvis runt ringen, upp till en hel spraycirkel vid 16.

Kontroller visas i expandern **Brush Heads** i verktygsalternativpanelen.

- **Antal**: Antal samtidiga borsthuvuden (1–16).
- **Storlek**: Återgiven storlek på varje huvud i förhållande till den globala borststorleken (0,1–1,0).
- **Vinkel** (0–360°): Statisk orientering av formationsringen, används när **Följer riktning** är under 1,0.
- **Tryckvariation**: Storleksvariation per huvud applicerad som en oberoende tryckförspänning genom dynamikkurvorna.
- **Opacitetsvariation**: Opacitetsvariation per huvud, oberoende av storleksvariation.
- **Styvhet**: Hur stelt omloppsradien följer den dynamiskt skalade borststorleken. 0 = omloppsbana spårar dynamikens storlek; 1 = omloppsbana förblir fixerad till basstorleken.
- **Följer riktning** (0,0–1,0): Hur starkt formationsringen följer rörelseriktningen. Vid 1,0 är ringen alltid vinkelrät mot färdriktningen; vid 0,0 låser den till det statiska **Vinkel**-värdet.
- **Teckenfrö** (0–255): Fast frö för karaktär per huvud (storlek, spridningsposition, räckvidd). Samma frö reproducerar samma formation varje slag. Desensibiliseras när **Random Head Character** är på.

#### Interpolation

Förskjuter huvuden längs och runt slagbanan varje dab, vilket skapar utsmetnings- och sprayeffekter.

- **Overshoot** (0–5): Sprider huvuden framåt längs färdriktningen. Vid 1,0 huvuden spridda upp till ett helt dab-mellanrum framåt; värden över 1,0 tillåter större räckvidd med en brant sällsynthetsbias.
- **Undershoot** (0–5): Samma som Overshoot men släpar efter nuvarande dab. I kombination med Overshoot skapar detta ett ledande utstryk eller komet-svans. Undertryckt vid första dubben för att undvika retrograda artefakter.
- **Sprayvinkel** (0–90°): Fläktar pekar var och en utåt från slagriktningen med en slumpmässig vinkel per huvud upp till detta värde. Spänd till 90° så att inget huvud någonsin är vänt bakåt. Standard: 10°.
- **Spray Seed** (0–255): Fast utsäde för per-head sprayvinklar, oberoende av Character Seed. Desensibiliseras när **Random Spray Pattern** är på.

#### Randomisering

- **Slumpmässigt huvudkaraktär**: Ritar om karaktärsvärden per huvud (storlek, spridningsposition, räckvidd) varje dab så att formationen är helt kaotisk längs slaget. Åsidosätter **Carakter Seed**.
- **Slumpmässigt spraymönster**: Ritar om sprayvinklarna varje klick så att fläkten växlar kontinuerligt längs slaget ("living spray"). Åsidosätter **Spray Seed**.
- **Slumpmässiga animationsramar**: För animerade penslar: varje huvud flyttar fram sin animeringsram oberoende av varandra.

### Ytterligare alternativ

I expandern **Ytterligare alternativ** (komprimerad som standard):- **Lås för att visa**: Håller borstens utseende fixerat i förhållande till arbetsytan: när du roterar duken roterar borsten med den.
- **Enkel borstgräns**: Använder en vanlig cirkel för penselmarkörens kontur istället för att återge hela penselformen. Användbar för komplexa eller stora penslar där den exakta gränsen är dyr att dra.
- **Uniform Jitter**: När den är på, dras dab-förskjutningar från **Jitter**-reglaget från en enhetlig fördelning (varje offset lika sannolikt inom intervallet). När den är avstängd är fördelningen Gaussisk (förskjuter klustret mot mitten).
- **Återställ senast använda färger**: Återställer förgrunds- och bakgrundsfärgerna från föregående session vid uppstart, istället för att förinställa svartvitt.
- **Slumpmässig horisontell**: 50 % chans att spegla varje stämpel från vänster till höger per klick.
- **Slumpmässig vertikal**: 50 % chans att vända varje stämpel upp och ner per klick.
- **Slumpmässig rotation**: Roterar varje stämpel slumpmässigt med 0°, 90°, 180° eller 270° per klick.
- **Återställ animering**: För animerade penslar: när den är på startar animeringen om från bildruta 0 vid varje nytt slag; när den är av, fortsätter den där det föregående slaget slutade.