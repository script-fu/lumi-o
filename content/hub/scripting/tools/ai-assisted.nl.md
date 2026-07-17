---
title: "AI-ondersteunde ontwikkeling"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Moderne AI-tools kunnen de ontwikkeling van Lumi-plug-ins aanzienlijk versnellen door op te treden als samenwerkende codeerpartner.

## VS Code in Agent-modus

Door Visual Studio Code te gebruiken met een AI-assistent in **Agent-modus** (zoals de Agent-modus van GitHub Copilot of andere assistenten met hulpmiddelen) kunt u complexe, uit meerdere stappen bestaande taken uitvoeren met behulp van natuurlijke taal.

In plaats van slechts één regel code in te vullen, kan een agent:
- de hele werkruimte lezen om de context te begrijpen
- nieuwe bestanden en mappen aanmaken
- terminalopdrachten uitvoeren om scripts te testen of te valideren
- naar bestaande patronen in de codebase zoeken

## Toegang tot opslagplaatsen

AI-hulp is het meest effectief wanneer de agent toegang heeft tot **lumi-dev** of uw specifieke projectrepository. Met inzicht in de bestaande codebase kan de Agent:
- de **[Utility Libraries]({{< ref "/hub/scripting/reference/utility-browser" >}})** als referentie voor helperfuncties gebruiken
- bestaande patronen voor GEGL-bewerkingen en laagbeheer volgen
- boilerplate-code van bestaande plug-ins hergebruiken

## Voorbeeldworkflow

U kunt de Agent direct vragen een volledige plug-in te genereren door het gewenste functionele resultaat te beschrijven:

> "Schrijf met behulp van de beschikbare Scheme-hulpprogramma's en voorbeelden in de werkruimte een nieuwe plug-in die een 50% horizontale hulplijn op de actieve afbeelding creëert en deze 'Middengids' noemt."

De agent zoekt uit hoe hulplijnen worden aangemaakt, identificeert de juiste utility-functie (zoals `lumi-image-add-hguide-percent` uit `common.scm`) en genereert het volledige `.scm`-bestand met de juiste registratie-boilerplate.

## Beste praktijken

- **Wees specifiek**: beschrijf precies wat u wilt dat de plug-in doet.
- **Referentiehulpprogramma's**: moedig de agent aan om naar de map `share/lumi/scripts/` te kijken voor helpers op hoog niveau.
- **Beoordelen en testen**: Test altijd de plug-in die door de AI wordt gegenereerd, het is vaak een iteratief en creatief proces.
