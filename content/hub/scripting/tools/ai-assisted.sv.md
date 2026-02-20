---
title: "AI-assisterad utveckling"
type: docs
---
Moderna AI-verktyg kan avsevärt påskynda utvecklingen av Lumi plug-in genom att fungera som en kodningspartner.

## VS-kod i agentläge

Genom att använda Visual Studio Code med en AI-assistent i **Agentläge** (som GitHub Copilots Agentläge eller andra verktygsaktiverade assistenter) kan du utföra komplexa, flerstegsuppgifter med naturligt språk.

Istället för att bara fylla i en enda kodrad kan en agent:
- Läs hela din arbetsyta för att förstå sammanhanget.
- Skapa nya filer och kataloger.
- Kör terminalkommandon för att testa eller validera skript.
- Sök efter befintliga mönster i din kodbas.

## Repository Access

AI-assistans är mest effektiv när agenten har tillgång till **lumi-dev** eller ditt specifika projektarkiv. Med insyn i den befintliga kodbasen kan agenten:
- Använd **[Utility Libraries](@@LUMI_TOKEN_4@@)** som referens för hjälpfunktioner.
- Följ befintliga mönster för GEGL-drift och lagerhantering.
- Återanvänd boilerplate-kod från etablerade plug-ins.

## Exempel på arbetsflöde

Du kan direkt be agenten att generera en fullständig plug-in genom att beskriva det önskade funktionsresultatet:

> "Med hjälp av de tillgängliga Scheme-verktygen och exemplen i arbetsytan, skriv ett nytt plugin-program som skapar en 50 % horisontell guide på den aktiva bilden och kallar den 'Center Guide'."

Agenten kommer att söka efter hur man skapar guider, identifiera rätt verktygsfunktion (som `lumi-image-add-hguide-percent` från `common.scm`) och generera den fullständiga `.scm`-filen med rätt registreringsskylt.

## Bästa metoder

- **Var specifik**: Beskriv exakt vad du vill att plugin-programmet ska göra.
- **Referensverktyg**: Uppmuntra agenten att titta på `share/lumi/scripts/`-katalogen för hjälpmedel på hög nivå.
- **Granska och testa**: Testa alltid plugin-programmet som genereras av AI, det är ofta en iterativ och kreativ process.