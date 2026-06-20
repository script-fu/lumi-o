---
title: "Filter"
type: docs
---
Lumis filtermeny samlar korrigerande justeringar, stiliserade linseffekter, procedurbildande texturgeneratorer, utskriftsinspirerade behandlingar och analysverktyg på ett och samma ställe. Menyordningen är praktisk snarare än akademisk: oskärpa och förbättringsverktyg sitter bredvid varandra, förvrängnings- och ljuseffekter grupperas efter utseende, och textur- eller mönstergeneratorer hålls samman när målet är att bygga källmaterial snarare än att modifiera en befintlig bild.

Filterdialoger följer samma allmänna arbetsflöde. Förinställningar, förhandsgranskning, delad vy och opacitets- eller blandningskontroller låter en effekt ställas in snabbt, och på lager kan resultatet stanna som ett redigerbart icke-förstörande filter istället för att slås samman direkt. Lumi har också en ny historik av filteranvändning, så att upprepa den senaste effekten eller öppna den sista dialogrutan är en del av den normala målningsrytmen snarare än en separat uppgift.

## Oskärpa

### Gaussisk oskärpa

Gaussian Blur är Lumis standarduppmjukningsfilter: en ren, jämn oskärpa med separata horisontella och vertikala storlekskontroller, kanthantering och kärnalternativ. Det är det allmänna valet för mjukt fokus, mjukade masker, atmosfäriskt djup och alla arbetsflöden där själva oskärpan ska förbli neutral.

### Pixelisera

Pixelize reducerar detaljer till avsiktliga blockstrukturer istället för en mjuk oskärpa. Eftersom dialogrutan exponerar blockbredd, blockhöjd, förskjutningar, pixelform och fyllningsbeteende, fungerar den både som en grov censureringseffekt och en kontrollerbar mosaik eller lågupplöst grafisk behandling.

### Selektiv Gaussisk oskärpa

Selektiv Gaussisk oskärpa mjuknar inom områden samtidigt som man försöker bevara starkare kanter. Det är användbart när en bild behöver en lugnare struktur eller minskat chatt utan att förlora de större formgränserna som fortfarande måste läsas tydligt.

### Linsoskärpa

Lens Blur är ett av Lumis mer illustrationsfokuserade oskärpa filter. Dess kontroller är uppbyggda kring polygon irisform, bladets krökning, anamorfisk sträckning, högdagerförstärkning och ett konfigurerbart fokusområde, så den beter sig mindre som en generisk mjukgörare och mer som ett stiliserat skärpedjupsverktyg med formad bokeh.

### Tilt-shift

Tilt-shift håller ett kontrollerbart fokusband skarpt samtidigt som bilden blir suddig över och under den. Dialogrutans bandvinkel, fjäder, perspektivförspänning, irisform och miniatyrförstärkning gör den väl lämpad för scener med miniatyrlook, arkitektoniska vyer och alla kompositioner där fokus bör läsas som en designad rand snarare än en cirkulär djupsignal.

### Cirkulär rörelseoskärpa

Circular Motion Blur smetar ut detaljer runt en mittpunkt och förvandlar kanter till roterande spår. Det är det naturliga valet för snurrande motiv, turbinliknande energi eller illustrationer som behöver en känsla av orbital rörelse.

### Linjär rörelseoskärpa

Linjär rörelseoskärpa sträcker ut detaljer i en riktning och simulerar resor, kamerarörelser eller snabba gester över ramen. Det är särskilt användbart när rörelsen behöver kännas riktad och grafisk snarare än diffus.

### Zooma rörelseoskärpa

Zoom Motion Blur utstrålar detaljer utåt från ett centrum, vilket ger en känsla av en rusning mot eller bort från betraktaren. Det fungerar bra för stötögonblick, hastighetslinjer och kompositioner som behöver en kamera-zoomenergi utan att måla om hela bilden.

## Förbättra

### HögpassHigh Pass isolerar fin lokal kontrast snarare än bred tonal förändring. Med endast skala och kontrast att hantera är det ett enkelt verktyg för att extrahera kantdetaljer, bygga skarpa överlägg eller förbereda skärpningspass som bör betona struktur mer än färg.

### Brusreducering

Brusreducering är det motsatta draget: det undertrycker oönskade fina variationer så att större former läser tydligare. Det är användbart när skannat material, komprimerade texturer eller överarbetade passager behöver förenklas innan ytterligare målning eller filtrering.

### Skärpa

Sharpen använder en oskarp maskmodell, med radie, mängd och tröskel som styr hur starkt lokal kontrast trycks in. I praktiken gör det den lämplig för att återställa klarhet efter oskärpa, exportstorleksändring eller subtila efterbehandlingspass där detaljer behöver komma fram utan att förvandla varje pixel till brus.

## Förvräng

### Kromatisk aberration

Kromatisk aberration separerar färgkanaler utåt från ett valt centrum, med kontroller för radiell eller tangentiell riktning, bias mellan kanalpar, falloff och bevarande av luminans. Koden och dialogrutan behandlar det båda som ett tvåvägsverktyg: det kan lägga till stiliserade linskanter för energi eller vända på tecknet för att korrigera mild aberration i källmaterialet.

### Linsförvrängning

Linsförvrängning omformar bilden genom kurvatur eller nålkuddeliknande kurvatur, kanttermer, zoomkompensation, mittförskjutningar och hörnljusning. Det gör den användbar både för att korrigera en bild som känns optiskt böjd och för att medvetet trycka en mot en vidvinkel- eller retroobjektiv.

## Belysning

### Bloom

Bloom förvandlar ljusa områden till kontrollerad glöd, med tröskel, mjukhet, radie och styrka som definierar hur långt ljuset sprids och hur starkt det lyfter bilden. Den extra exponeringsbegränsande kontrollen håller den användbar som en highlight-effekt snarare än en automatisk tvättning.

### Himmel

Himmel är mer än en nyans eller övertoning: den återger en analytisk himmel med modellerna Preetham, Hosek/Wilkie eller Nishita. Eftersom dialogrutan exponerar projektion, solvinkel, grumlighet, atmosfärisk densitet, höjd över havet, solskivor och exponering, kan den bygga allt från en enkel klar bakgrund till en mer fysiskt jordad solnedgång eller skymningshimmel.

### Vinjett

Vinjetten mörknar, färgas eller till och med raderas mot bildkanterna, med kontroller för form, radie, mjukhet, gamma, proportioner, squeeze, rotation och positionering på duken. Den fungerar som en klassisk fotografisk kantbehandling, men den är tillräckligt flexibel för att fungera som en inramningsmask eller en oregelbunden kompositionsspotlight.

## Buller

### HSV-ljud

HSV Noise randomiserar nyans, mättnad och värde oberoende. Det gör det användbart när en bild behöver färglivlighet eller analog instabilitet utan att helt bryta isär den lokala strukturen.

### Släng

Hurl är den extrema versionen av brus: den ersätter pixlar med helt slumpmässiga färger. Det är bäst att tänka på som en destruktiv kaoskälla för glitcharbete, bekymrade texturer eller masker som behöver aggressiv upplösning.

### Välj

Pick ersätter varje pixel med en slumpmässigt vald granne, så bilden förblir relaterad till sin källa istället för att bli ren statisk. Resultatet är en blandad, granulär variation som kan kännas mer organisk än helt slumpmässigt brus.

### SpridaSprid spridningspixlar genom att slumpmässigt förskjuta dem inom en radie. Det är användbart när du vill ha orörlig störning: en trasig yta, en utsmetad kant eller en distresserad textur som fortfarande bär källbildens färgförhållanden.

### Fraktal

Fractal genererar beläggningsbart fraktalt Perlin-brus, vilket gör det särskilt värdefullt som en återanvändbar källa för masker, moln, pappersstruktur, terrängliknande uppdelning och proceduröverlägg. Eftersom den kakel kan den mata större arbetsflöden utan att skapa uppenbara sömmar.

### Blåbruskorn

Blue Noise Grain är Lumis monokroma korngenerator för film och tryck. Dialogrutans förinställningar för kornstorlek, maskering av blått brus, mellantonsbias, skuggbias och frökontroller visar att den är utformad för att placera korn jämnt och kontrollerbart, inte bara för att spraya slumpmässiga monokroma fläckar över bilden.

### Risograf korn

Risograph Grain bygger på samma kornlogik men förvandlar den till en tvåplåtstryckeffekt. Separata bläckfärger, plåtbalans, avsiktlig felregistrering och seedad variation gör att den passar bra för affischarbete, indietrycksestetik och illustrationer som ska kännas fysiskt övertryckta snarare än digitalt perfekta.

### Halvton (FM)

Halvton (FM) skapar en stokastisk, frekvensmodulerad halvton med hjälp av blått brus eller relaterade tröskelmetoder. Med färglägen för monokrom, duotone och CMYK, plus kontroller för punktförstärkning och plåtdekorrelation, är den inriktad på utskriftsliknande struktur som förblir oregelbunden och livlig istället för att falla in i ett styvt rutnät.

## Kanter

### Gaussernas skillnad

Difference of Gaussians upptäcker kanter genom att subtrahera två suddiga versioner av bilden från varandra. Det är en kompakt, användbar operatör för kantkartor, stiliserad linjeextraktion och att hitta strukturella övergångar utan att förbinda sig till en fullständig tröskelkontur.

## Morfologi

### Median

Median ersätter varje pixel med medianvärdet från dess grannskap, vilket tenderar att ta bort isolerat brus samtidigt som starkare gränser bevaras bättre än en enkel oskärpa. Det är ett praktiskt rengöringsfilter för att platta till små visuella prat utan att omedelbart mjuka upp hela bilden.

### Utvidga

Dilate växer ljusare regioner utåt med samma formmedvetna grannskapslogik. När det gäller bildskapande kan den göra ljusa märken tjockare, utöka ljusa former eller stänga små mörka luckor.

### Erodera

Erodera gör det kompletterande draget, växer mörkare områden och drar tillbaka ljusare. Det är användbart för att förtunna ljusa detaljer, förstora mörka massor eller dra åt masker och grafiska former.

## Mönster

### Schackbräde

Schackbräde genererar ett vanligt alternerande brickmönster. Det är enkelt, men den enkelheten gör det användbart för att testa transparens, bygga masker, blockera grafiska bakgrunder eller skapa rent geometriskt källmaterial.

### Rutnät

Grid ritar upprepade horisontella och vertikala indelningar, vilket gör det användbart för layoutguider, designbakgrunder, teknisk illustration och procedurmaskering. Eftersom det genereras som ett filter kan avståndet och utseendet justeras utan att mönstret måste byggas för hand.

### Voronoi

Voronoi genererar en beläggningsbar cellulär textur från sådda punkter, med kontroller för funktionstyp, avståndsmetrik, slumpmässighet, fraktaldetaljer och sömlös inpackning. I praktiken kan det gå från rena spruckna cellstrukturer till mer organiska sten-, hud-, kart- eller abstrakta nätverksmönster.

### VågaWave producerar bandade eller ringade mönster som formas av vågformsprofil, geometriskt arrangemang, distorsion, fraktal detalj och fasförskjutning. Det gör det till mer än ett enkelt stripe-verktyg: det kan generera kontrollerade krusningar, topografiska band, moaréliknande grafik eller bullriga koncentriska mönsterfält.

### Halvton (AM)

Halvton (AM) tillämpar en klassisk amplitudmodulerad punktskärm, med kontroller för frekvens, punktform, skärpa, färgläge och CMYK-vinkel för utskriftsstruktur i rosettstil. Jämfört med FM-halvtoning är det det mer ordnade, igenkännbart mekaniska alternativet när det önskade utseendet är tidningspapper, offsetlitografi eller medvetet synlig skärmgeometri.