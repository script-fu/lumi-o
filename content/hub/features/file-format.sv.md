---
title: "Filformat (.lum)"
type: docs
---
Lumis ursprungliga filformat är byggt för skiktade målningsprojekt som måste förbli tillförlitliga, inspekterbara och återställbara över tid. Den är designad kring verkligheten i illustrationsarbete: många lager, stora dukar, inbäddad färginformation, masker, effekter och återställningsdata.

I stället för att behandla ett projekt som en enda ogenomskinlig blob, håller formatet strukturen på konstverket synlig för applikationen. Detta gör att Lumi kan spara, ladda och återställa stora bilder på ett mer intelligent sätt samtidigt som den bevarar organisationen artister är beroende av.

## Öppen projektstruktur

Ett Lumi-projekt håller konstverkets delar åtskilda: bildstruktur, lagerinnehåll, masker, färgdata, metadata och återställningsinformation har alla en tydlig roll. Detta gör formatet lättare att resonera kring och bättre lämpat för långtidsaccess än en sluten, monolitisk behållare.

Målet är inte bara att lagra pixlar, utan att lagra arbetstillståndet för en illustration. Lager förblir lager, masker förblir masker och filen fortsätter att spegla hur konstverket byggdes.

## Designad för stora målningar

Stora lagerbilder kan snabbt bli tunga. Lumis format stöder arbetsflöden där inte varje bit bilddata behöver dras in i minnet på en gång. Projekt kan förbli lyhörda genom att ladda de delar av bilden som faktiskt behövs för visning, redigering, sammansättning eller export.

Detta tillvägagångssätt hjälper komplexa filer att kännas hanterbara, särskilt när ett konstverk innehåller många dolda, arkiverade, experimentella eller grupperade lager.

## Spara utan att bryta flödet

Filformatet stöder både normal projektsparning och lätta ögonblicksbilder i återställningsstil. Detta ger konstnärer ett sätt att skydda verk ofta utan att förvandla varje kontrollpunkt till en fullständig kopia av hela bilden.

Eftersom återställningsinformation hör till projektstrukturen kan Lumi hålla användbar historik nära teckningen samtidigt som den tillåter automatiska säkerhetslagringar att leva separat från arbetsfilen.

## Utbyte och export

Det ursprungliga formatet är avsett för pågående Lumi-arbete, medan exportformat används för att dela tillplattade eller kompatibilitetsfokuserade resultat. Importstöd hjälper till att föra in befintliga konstverk till Lumis skiktade miljö, och exportstöd låter färdiga bitar lämna projektformatet när de är redo för publicering, leverans eller vidare bearbetning.

Distinktionen håller arbetsfilen rik och redigerbar samtidigt som slutbilderna kan produceras i vanliga externa format.

## Långsiktig tillförlitlighet

Kort sagt är formatet `.lum` en praktisk behållare för seriöst målningsarbete: tillräckligt öppet för att inspektera, tillräckligt strukturerat för att återhämta sig och tillräckligt flexibelt för att hantera komplexa skiktade bilder ekonomiskt.