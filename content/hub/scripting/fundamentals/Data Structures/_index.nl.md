---
title: "Gegevensstructuren"
type: "docs"
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 352594bbda9977488d773240c50663f63fd432a17483772a9cbf8d59dab378be
url: "hub/scripting/fundamentals/Data Structures"
---
In Scheme zijn **datastructuren** essentiële hulpmiddelen voor het organiseren, opslaan en manipuleren van gegevens. Ze stellen ontwikkelaars in staat efficiënte, leesbare en herbruikbare scripts te bouwen. Door de juiste datastructuur voor een specifiek probleem te kiezen, kunt u zowel de prestaties als de duidelijkheid van uw code optimaliseren.

## Belangrijkste gegevensstructuren in Scheme

Scheme biedt verschillende krachtige en veelzijdige datastructuren, elk geschikt voor specifieke taken. De primaire datastructuren omvatten:

### Lijsten

Lijsten zijn geordende verzamelingen van elementen die dynamisch kunnen groeien of krimpen. Ze zijn ideaal voor sequentiële of hiërarchische gegevens en worden veel gebruikt bij functioneel programmeren.

Belangrijkste kenmerken:
- Dynamisch formaat.
- Elementen kunnen van gemengde typen zijn.
- Vaak gebruikt voor recursieve algoritmen en het weergeven van boomachtige structuren.

Voorbeelden van gebruik:
- Beheer van collecties van artikelen.
- Vertegenwoordigt reeksen of hiërarchieën.

---

### vectoren

Vectoren zijn verzamelingen elementen van vaste grootte, geïndexeerd voor snelle toegang. Ze zijn het meest geschikt voor scenario's waarin prestaties en positionele toegang van cruciaal belang zijn.

Belangrijkste kenmerken:
- Vaste grootte bij creatie.
- Elementen zijn toegankelijk via hun index.
- Sneller dan lijsten voor bepaalde bewerkingen zoals willekeurige toegang.

Voorbeelden van gebruik:
- Opslaan van configuraties of gegevens met een vaste grootte.
- Snelle zoekopdrachten en updates op basis van positie.

---

### De juiste gegevensstructuur kiezen

De beslissing om een **lijst** of een **vector** te gebruiken, hangt af van de specifieke behoeften van uw script. Hier zijn enkele richtlijnen:

| Kenmerk | Lijsten | Vectoren |
|----------------------|----------------------------|------------------------------|
| **Maatflexibiliteit** | Dynamisch | Vast |
| **Toegangssnelheid** | Langzamer (sequentiële toegang) | Sneller (geïndexeerde toegang) |
| **Gemak van wijziging**| Makkelijker | Moeilijker (vereist herverdeling)|
| **Gebruiksscenario's** | Dynamische gegevens, recursie | Statische gegevens, snelle zoekopdrachten |

---