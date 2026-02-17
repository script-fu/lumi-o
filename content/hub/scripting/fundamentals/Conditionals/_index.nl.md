---
title: "Voorwaardelijke voorwaarden"
type: docs
weight: 2
---
Conditionals vormen een fundamenteel onderdeel van programmeren, waardoor scripts beslissingen kunnen nemen en hun stroom kunnen controleren op basis van specifieke criteria. In Scheme, dat is gebaseerd op de programmeertaal Scheme, kunt u met conditionals dynamische en intelligente scripts maken die zich aanpassen aan veranderende invoer, omgevingen of gebruikersacties.

### De rol van voorwaardelijke waarden in schema's

Voorwaardelijke functies dienen verschillende belangrijke doelen in uw scripts:
- **Directing Logic:** Hiermee kunt u verschillende stukjes code uitvoeren, afhankelijk van of bepaalde voorwaarden waar of onwaar zijn.
- **Verbetering van de flexibiliteit:** Door dynamisch te reageren op invoer of statussen helpen conditionals uw script om een ​​verscheidenheid aan scenario's aan te kunnen.
- **Vereenvoudiging van de complexiteit:** Ze splitsen de besluitvorming op in beheersbare structuren, waardoor code gemakkelijker te lezen, te debuggen en te onderhouden is.

### Soorten voorwaardelijke waarden beschikbaar

Scheme biedt verschillende voorwaardelijke constructies, elk geschikt voor verschillende logische behoeften:
- **`if`:** Voor het nemen van eenvoudige binaire beslissingen, waarbij één blok code wordt uitgevoerd als een voorwaarde waar is, en een ander blok als deze onwaar is.
- **`cond`:** Een krachtige constructie met meerdere vertakkingen voor het op een duidelijke, gestructureerde manier omgaan met meerdere aandoeningen.
- **`and` / `or`:** Logische operatoren die combinaties van voorwaarden evalueren, waardoor complexere besluitvorming mogelijk wordt.
- **`else`:** Een verzamelnaam die fallback-gedrag definieert wanneer aan geen van de opgegeven voorwaarden wordt voldaan.

### Hoe voorwaardelijke waarden werken

Voorwaardelijke bepalingen omvatten doorgaans:
1. **Een voorwaarde evalueren:** Een testexpressie bepaalt of een voorwaarde waar of onwaar is.
2. **Vertakkingsuitvoering:** Op basis van de evaluatie selecteert het script welk codeblok moet worden uitgevoerd.
3. **Een waarde retourneren (optioneel):** In sommige gevallen kunnen conditionals ook een waarde produceren die andere delen van het script kunnen gebruiken.