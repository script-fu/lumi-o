---
title: "Conditionals"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a6a08e6af8a8a31688dabd4434bee5da3ff07ec61763f636fb5c2029da03f472
---
Conditionals vormen een fundament onder programmeren: ze laten scripts beslissingen nemen en hun verloop sturen op basis van specifieke criteria. In Scheme, gebaseerd op de Scheme-programmeertaal, helpen conditionals u dynamische, intelligente scripts te schrijven die zich aanpassen aan veranderende invoer, omgevingen of gebruikersacties.

### De rol van conditionals in Scheme

Conditionals vervullen in uw scripts verschillende kernfuncties:
- **Logica sturen:** Ze voeren verschillende codestukken uit, afhankelijk van of bepaalde voorwaarden waar of onwaar zijn.
- **Meer flexibiliteit:** Door dynamisch te reageren op invoer of toestand helpen ze uw script een breed scala aan scenario's aan.
- **Complexiteit vereenvoudigen:** Ze splitsen besluitvorming op in behapbare structuren, waardoor code makkelijker te lezen, debuggen en onderhouden is.

### Beschikbare conditionele constructies

Scheme biedt verschillende conditionele constructies, elk geschikt voor andere logische behoeften:
- **`if`:** Voor eenvoudige binaire beslissingen — het ene codeblok als een voorwaarde waar is, het andere als die onwaar is.
- **`cond`:** Een krachtige constructie met meerdere vertakkingen voor meerdere voorwaarden op een duidelijke, gestructureerde manier.
- **`and` / `or`:** Logische operatoren die combinaties van voorwaarden evalueren voor complexere besluitvorming.
- **`else`:** Een fallback die het gedrag definieert wanneer geen van de opgegeven voorwaarden geldt.

### Hoe conditionals werken

Conditionals omvatten doorgaans:
1. **Een voorwaarde evalueren:** Een testexpressie bepaalt of een voorwaarde waar of onwaar is.
2. **Vertakte uitvoering:** Op basis van de evaluatie kiest het script welk codeblok wordt uitgevoerd.
3. **Een waarde teruggeven (optioneel):** In sommige gevallen leveren conditionals ook een waarde op die elders in het script gebruikt kan worden.