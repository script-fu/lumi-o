---
title: "Bestandsformaat (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

Het native bestandsformaat van Lumi is ontworpen voor gelaagde schilderprojecten die in de loop van de tijd betrouwbaar, inspecteerbaar en herstelbaar moeten blijven. Het is opgebouwd rond de realiteit van illustratiewerk: veel lagen, grote doeken, ingebedde kleurinformatie, maskers, effecten en herstelgegevens.

In plaats van een project als één ondoorzichtige blob te behandelen, houdt het formaat de structuur van het kunstwerk zichtbaar voor de applicatie. Zo kan Lumi grote afbeeldingen slimmer opslaan, laden en herstellen, terwijl de organisatie waar kunstenaars op vertrouwen behouden blijft.

## Open projectstructuur

Een Lumi-project houdt de onderdelen van het kunstwerk gescheiden: beeldstructuur, laaginhoud, maskers, kleurgegevens, metadata en herstelinformatie hebben elk een duidelijke rol. Dat maakt het formaat overzichtelijker en beter geschikt voor langdurige toegang dan een gesloten, monolithische container.

Het doel is niet alleen pixels opslaan, maar de werkstatus van een illustratie bewaren. Lagen blijven lagen, maskers blijven maskers, en het bestand blijft de manier weerspiegelen waarop het kunstwerk is opgebouwd.

## Ontworpen voor grote schilderijen

Grote gelaagde afbeeldingen worden snel zwaar. Het Lumi-formaat ondersteunt workflows waarbij niet alle beeldgegevens tegelijk in het geheugen hoeven te worden geladen. Projecten blijven responsief door alleen de delen van de afbeelding te laden die nodig zijn voor weergave, bewerking, compositie of export.

Die aanpak maakt complexe bestanden beheersbaar, vooral wanneer een kunstwerk veel verborgen, gearchiveerde, experimentele of gegroepeerde lagen bevat.

## Opslaan zonder de flow te breken

Het bestandsformaat ondersteunt zowel normale projectopslag als lichte herstelmomenten. Zo kunnen kunstenaars hun werk regelmatig beschermen zonder van elk controlepunt een volledige kopie van de hele afbeelding te maken.

Omdat herstelinformatie bij de projectstructuur hoort, kan Lumi nuttige geschiedenis dicht bij het kunstwerk houden, terwijl automatische veiligheidsopslagen apart van het werkbestand kunnen blijven.

## Uitwisseling en export

Het native formaat is bedoeld voor doorlopend Lumi-werk; exportformaten delen afgevlakte of compatibiliteitsgerichte resultaten. Import helpt bestaande illustraties in Lumi's gelaagde omgeving te brengen; export laat afgeronde stukken het projectformaat verlaten wanneer ze klaar zijn voor publicatie, levering of verdere verwerking.

Zo blijft het werkbestand rijk en bewerkbaar, terwijl eindbeelden in gangbare externe formaten kunnen worden geproduceerd.

## Betrouwbaarheid op lange termijn

Kortom: het `.lum`-formaat is een praktische container voor serieus schilderwerk — open genoeg om te inspecteren, gestructureerd genoeg om te herstellen en flexibel genoeg om complexe gelaagde afbeeldingen efficiënt aan te kunnen.
