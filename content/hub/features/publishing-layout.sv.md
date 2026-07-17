---
title: "Publiceringslayout"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
url: "hub/features/publishing-layout"
translation_source_sha256: dc0367028ed8f6b4e1508c309384967daa43a4148f8d70f00880173a0a1fca7d
---
Illustration för tryck och publicering behöver ofta mer än en canvasstorlek. Sidorna har trimningskanter, uppslag har mittsömmar och viktigt innehåll kan behöva hållas borta från områden som kommer att skäras bort eller bindas in i en ränna. Lumis verktyg för publiceringslayout håller dessa problem synliga medan du målar, utan att platta till dem i konstverket.

Layoutgränser lagras per bild, sparas med projektet och kan stängas av när de inte behövs. Målet är att ge bok-, serie- och tryckarbetsflöden en tydlig känsla av sidstruktur medan den skiktade bilden förblir fullt redigerbar undertill.

## Utfall och beskärning

Utfall (bleed) anger hur långt motivet sträcker sig bortom sidans slutliga kant. Lumi visar beskärningsytan som sidans gräns inuti duken, med utfallsmarginalen som en skuggad överlagring runt den. Det gör det enklare att måla bakgrunder och kantdetaljer som måste överleva beskärning utan att gissa var den färdiga sidan slutar.

Måtten kan ställas in i de enheter som passar jobbet, så utfall kan tänkas i tum, millimeter eller en annan bekant utskriftsenhet snarare än bara i pixlar.

## Fals och uppslag

För dubbelsidiga uppslag markerar falsen (gutter) det skyddade området runt mittsömmen där viktigt innehåll bör undvikas. När den är aktiverad visar Lumi falsband över uppslaget så att ansikten, text och fokuspunkter hålls borta från bindningsområdet medan hela uppslaget fortfarande är en sammanhängande duk.

Detta är särskilt användbart för serier, bilderböcker och alla konstverk som kommer att skrivas ut som motstående sidor snarare än isolerade ark.

## Kompositionsguider

Valfria kantstöd markerar det beskurna sidområdet med subtila kompositionsmarkeringar. Guider kan följa indelningar per sida eller en fullständig läsning, och kan använda tredjedelar, gyllene sektioner eller femtedelar beroende på hur layouten ska bedömas.

Guiderna är avsedda som tyst referens under layout och efterarbete. De hjälper placeringen att läsa mot sidan som faktiskt kommer att skrivas ut, inte bara mot hela den digitala duken.

## Visa layout på duken

Layoutöverlägg styrs från menyn Visa. Blödnings-, ränna- och guideområden kan visas var för sig eller tillsammans, så att en konstnär kan fokusera på den del av publiceringsstrukturen som är viktig i det ögonblicket.

Bild > Aktivera layout aktiverar eller inaktiverar layoutgränser för den aktuella bilden. När layouten är inaktiverad döljs överlagringar och vyväxlingarna går åt sidan, men gränsinställningarna förblir sparade med filen för senare användning.

## Sparad med projektet

Layoutinställningarna följer med `.lum`-projektet. Om du öppnar bilden senare återställs utfall, ränna, överlagringsutseende, guideval och om layouten är aktiverad för den filen. Detta behåller publiceringsmedvetna inställningar en del av konstverkets arbetsläge snarare än en tillfällig visningspreferens.

För konstnärer som rör sig mellan att skissa, måla och förbereda tryck, stannar arbetsflödet på ett ställe: samma skiktade bild, med publiceringsstruktur tillgänglig när sidan behöver det.