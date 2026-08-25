---
title: "Filformat (.lum)"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
url: "hub/features/file-format"
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Lumis egna `.lum`-format är en projektkatalog, inte en enda sluten fil. Det är avsett för lagerillustration: djupa lagerträd, stora dukar, masker, oförstörande effekter och kontrollpunkter som inte behöver duplicera hela målningen.

Formatets uppgift är att hålla den arbetsstrukturen intakt — så att ett projekt kan öppnas igen så som det sparades, granskas när något går fel och återställas från en färsk kontrollpunkt, utan att behandla verket som en ogenomskinlig klump.

## Separata delar, med avsikt

Ett `.lum`-projekt är en mapp. Lagerträdet och bildegenskaperna ligger i läsbar XML. Varje lager och varje mask har en egen pixelbuffert, namngiven efter verket i stället för efter ett internt ID. Vektorbanor sparas som vanlig SVG. Tunga filterinställningar ligger i egna filer intill bilden. ICC-profiler sparas en gång i projektmappen, så att återställningsögonblicksbilder kan hänvisa till dem i stället för att kopiera dem.

Den uppdelningen är det som gör resten av formatet möjligt. Oförändrade lager kan lämnas orörda på disk. En skadad buffert påverkar bara sig själv i stället för att dra med sig hela filen. Saknade lagerpixlar blir tomma lager som fortfarande har namn, positioner och blandningsinställningar; en saknad gruppsammansättning byggs upp från barnen. Projektet förblir en karta över hur målningen byggdes.

Pigmentpaletter hör till Lumis färgverktyg. Ett projekt kan komma ihåg vilken palett som hörde till bilden, men palettbiblioteket självt ligger utanför `.lum`.

## Redigerbart tillstånd, inte tillplattning

Filen lagrar målningen som den är under arbete. Lager förblir lager, grupper förblir grupper och masker förblir masker, inklusive förskjutningar, lås, blandningsbeteende och filterstaplar. Oförstörande filter sparas som operationer och parametrar, inte som färdigberäknade pixlar. Ett lager som är en enda enfärgad yta behöver ingen pixelfil alls.

Hopfällda grupper behåller också en sammansatt vy av sig själva. Den sparade gruppbilden är det som syns på duken när en grupp är stängd, så att barnen inte behöver rekonstrueras bara för att titta på bilden. Inspektionslägen som bara är till för visning hålls utanför den cachen: att visa en mask eller alfa för redigering återställs som metadata, inte inbakat i den sparade gruppen.

## Stora filer kan ligga kvar delvis på disk

Att öppna en `.lum` innebär inte att varje pixel måste laddas. Innehåll i hopfällda grupper kan ligga kvar på disk medan gruppens sparade sammansättning visas genast. När en grupp fälls ut kommer de lagren, maskerna och nästlade grupperna in i minnet. Grupper som förblir stängda förblir lätta.

Filen noterar också vilka grupper som faktiskt var i bruk. Grupper på vägen till den aktiva markeringen kan öppnas utfällda; andra mappar lagras som hopfällda även om de råkade vara öppna i den senaste sessionen. Då behöver en djup fil inte ladda varje oanvänd gren in i minnet i samma ögonblick den öppnas.

Gruppering är därför ett prestandaval lika mycket som ett organisationsval. Stora bakgrunder, arkiverade experiment och oanvända varianter kan ligga i stängda grupper utan att ta samma minne som lagren som målas. Sparandet följer samma regel: fortfarande dolda buffertar kopieras eller utelämnas som filer, inte blåses upp i minnet bara för att skrivas igen.

## Kontrollpunkter som bara skriver det som ändrats

Fil → Spara uppdaterar arbetsprojektet. Inkrementella sparningar och autosparning skriver till ett återställningsträd, och de skriver bara ändrade data — förändrade lagerbuffertar, inte en andra kopia av hela bilden. Varje kontrollpunkt bär ändå en fullständig beskrivning av lagerträdet, så att vilken punkt som helst på den stigen kan öppnas genom att fylla i oförändrade pixlar från äldre kontrollpunkter och, om det behövs, från arbetsfilen själv.

Autospara använder samma mönster i en separat cache, så att automatiskt skydd inte behöver skriva om filen på disk. Om ett projekt öppnas när det finns nyare kontrollpunkter än den senaste fullständiga sparningen kan Lumi erbjuda dem i stället för att tyst kasta det nyare arbetet. Återställda bilder öppnas under ett eget namn så att en snabb sparning inte kan skriva över originalet.

## Ett arbetsformat

`.lum` är till för att fortsätta en målning i Lumi. Tillplattade format eller kompatibilitetsformat är till för publicering, leverans och andra program. Eftersom ett projekt är en katalog med många filer bör det arkiveras om det ska flyttas.

Arbetsfilen förblir rik och redigerbar. Export är sättet en färdig eller delad bild lämnar den strukturen.
