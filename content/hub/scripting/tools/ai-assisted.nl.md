---
title: "AI-ondersteunde ontwikkeling"
type: docs
---
Moderne AI-tools kunnen de ontwikkeling van Lumi-plug-ins aanzienlijk versnellen door op te treden als samenwerkende codeerpartner.

## VS-code in Agent-modus

Door Visual Studio Code te gebruiken met een AI-assistent in **Agent-modus** (zoals de Agent-modus van GitHub Copilot of andere assistenten met hulpmiddelen) kunt u complexe, uit meerdere stappen bestaande taken uitvoeren met behulp van natuurlijke taal.

In plaats van slechts één regel code in te vullen, kan een agent:
- Lees uw hele werkruimte om de context te begrijpen.
- Maak nieuwe bestanden en mappen.
- Voer terminalopdrachten uit om scripts te testen of te valideren.
- Zoek naar bestaande patronen in uw codebase.

## Toegang tot opslagplaatsen

AI-hulp is het meest effectief wanneer de agent toegang heeft tot **lumi-dev** of uw specifieke projectrepository. Met inzicht in de bestaande codebase kan de Agent:
- Gebruik **[Utility Libraries](@@LUMI_TOKEN_4@@)** als referentie voor helperfuncties.
- Volg bestaande patronen voor GEGL-bewerkingen en laagbeheer.
- Hergebruik standaardcode van bestaande plug-ins.

## Voorbeeldworkflow

U kunt de Agent direct vragen een volledige plug-in te genereren door het gewenste functionele resultaat te beschrijven:

> "Schrijf met behulp van de beschikbare Schema-hulpprogramma's en voorbeelden in de werkruimte een nieuwe plug-in die een 50% horizontale hulplijn op de actieve afbeelding creëert en deze 'Middengids' noemt."

De Agent zoekt naar manieren om handleidingen te maken, identificeert de juiste nutsfunctie (zoals `lumi-image-add-hguide-percent` van `common.scm`) en genereert het volledige `.scm` bestand met de juiste registratieboilerplate.

## Beste praktijken

- **Wees specifiek**: beschrijf precies wat u wilt dat de plug-in doet.
- **Referentiehulpprogramma's**: moedig de agent aan om naar de map `share/lumi/scripts/` te kijken voor helpers op hoog niveau.
- **Beoordelen en testen**: Test altijd de plug-in die door de AI wordt gegenereerd, het is vaak een iteratief en creatief proces.