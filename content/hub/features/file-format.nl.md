---
title: "Bestandsformaat (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Het eigen `.lum`-formaat van Lumi is een projectmap, geen enkel gesloten bestand. Het is ontworpen voor gelaagde illustratie: diepe lagenbomen, grote doeken, maskers, niet-destructieve effecten en controlepunten die niet het hele schilderij hoeven te dupliceren.

De taak van het formaat is die werkstructuur intact te houden — zodat een project getrouw opnieuw kan worden geopend, geïnspecteerd wanneer iets misgaat, en hersteld vanaf een recent controlepunt, zonder het kunstwerk als één ondoorzichtig geheel te behandelen.

## Aparte onderdelen, bewust

Een `.lum`-project is een map. De lagenboom en beeldeigenschappen staan in leesbare XML. Elke laag en elk masker houdt een eigen pixelbuffer, genoemd naar het kunstwerk in plaats van naar een intern ID. Vectorpaden worden als gewoon SVG opgeslagen. Zware filterinstellingen staan in eigen bestanden naast de afbeelding. ICC-profielen worden één keer in de hoofdmap van het project bewaard, zodat herstelmomenten ernaar kunnen verwijzen in plaats van ze te kopiëren.

Die splitsing maakt de rest van het formaat mogelijk. Ongewijzigde lagen kunnen op schijf met rust worden gelaten. Een beschadigde buffer faalt op zichzelf, in plaats van het hele bestand mee te sleuren. Ontbrekende laagpixels worden lege lagen die nog steeds namen, posities en overvloeinstellingen hebben; een ontbrekende groepsweergave wordt opnieuw opgebouwd uit de kinderen. Het project blijft een kaart van hoe het schilderij is opgebouwd.

Pigmentpaletten horen bij de kleurtools van Lumi. Een project kan onthouden welk palet bij de afbeelding hoorde, maar de paletbibliotheek zelf staat buiten het `.lum`.

## Bewerkbare status, geen afvlakking

Het bestand bewaart het schilderij in bewerking. Lagen blijven lagen, groepen blijven groepen en maskers blijven maskers, inclusief verschuivingen, vergrendelingen, overvloeigedrag en filterstapels. Niet-destructieve filters worden opgeslagen als bewerkingen en parameters, niet als vastgezette pixels. Een laag die één vlakke kleur is, heeft helemaal geen pixelbestand nodig.

Ingeklapte groepen bewaren ook een samengestelde weergave van zichzelf. Die opgeslagen groepsweergave is wat op het canvas verschijnt wanneer een groep dicht is, zodat kinderen niet hoeven te worden gereconstrueerd alleen om naar de afbeelding te kijken. Inspectiemodi die alleen voor weergave zijn, blijven buiten die cache: een masker of alfa tonen om te bewerken wordt als metadata hersteld, niet in de opgeslagen groep vastgelegd.

## Grote bestanden kunnen deels op schijf blijven

Een `.lum` openen hoeft niet elke pixel te laden. Inhoud in ingeklapte groepen kan op schijf blijven terwijl de opgeslagen groepsweergave meteen wordt getoond. Bij het uitklappen komen die lagen, maskers en geneste groepen in het geheugen. Groepen die dicht blijven, blijven licht.

Het bestand legt ook vast welke groepen daadwerkelijk in gebruik waren. Groepen op het pad van de actieve selectie kunnen uitgeklapt opnieuw worden geopend; andere mappen worden als ingeklapt opgeslagen, ook als ze in de vorige sessie toevallig open stonden. Zo hoeft een diep bestand niet elke ongebruikte tak meteen in het geheugen te laden.

Groeperen is daarom zowel een prestatie- als een organisatiekeuze. Grote achtergronden, gearchiveerde experimenten en ongebruikte varianten kunnen in gesloten groepen zitten zonder hetzelfde geheugen in te nemen als de lagen waarop wordt geschilderd. Opslaan volgt dezelfde regel: nog verborgen buffers worden als bestanden gekopieerd of overgeslagen, niet weer in het geheugen gehesen alleen om opnieuw te worden weggeschreven.

## Controlepunten die alleen schrijven wat er is veranderd

Bestand → Opslaan werkt het werkproject bij. Incrementele opslagen en automatisch opslaan schrijven naar een herstelboom, en schrijven alleen gewijzigde gegevens — veranderde laagbuffers, geen tweede kopie van de hele afbeelding. Elk controlepunt draagt nog steeds een volledige beschrijving van de lagenboom, zodat elk punt in dat spoor kan worden geopend door ongewijzigde pixels aan te vullen vanuit oudere controlepunten en, indien nodig, vanuit het werkbestand zelf.

Automatisch opslaan gebruikt hetzelfde patroon in een aparte cache, zodat automatische bescherming het bestand op schijf niet hoeft te herschrijven. Als een project wordt geopend terwijl er nieuwere controlepunten bestaan dan de laatste volledige opslag, kan Lumi die aanbieden in plaats van het recentere werk stilzwijgend weg te gooien. Herstelde afbeeldingen openen onder een aparte naam, zodat een snelle opslag het origineel niet kan overschrijven.

## Een werkformaat

`.lum` is bedoeld om een schilderij in Lumi voort te zetten. Afgevlakte of compatibiliteitsformaten zijn voor publicatie, levering en andere toepassingen. Omdat een project een map met veel bestanden is, moet het worden gearchiveerd als het moet reizen.

Het werkbestand blijft rijk en bewerkbaar. Via export verlaat een afgerond of gedeeld beeld die structuur.
