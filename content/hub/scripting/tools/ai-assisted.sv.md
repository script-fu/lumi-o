---
title: "AI-assisterad utveckling"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Moderna AI-verktyg kan avsevärt påskynda utvecklingen av Lumi plug-in genom att fungera som en kodningspartner.

## VS Code i Agent-läge

Genom att använda Visual Studio Code med en AI-assistent i **Agentläge** (som GitHub Copilots Agentläge eller andra verktygsaktiverade assistenter) kan du utföra komplexa, flerstegsuppgifter med naturligt språk.

Istället för att bara fylla i en enda kodrad kan en agent:
- läsa hela arbetsytan för att förstå sammanhanget
- skapa nya filer och kataloger
- köra terminalkommandon för att testa eller validera skript
- söka efter befintliga mönster i kodbasen

## Åtkomst till repository

AI-assistans är mest effektiv när agenten har tillgång till **lumi-dev** eller ditt specifika projektarkiv. Med insyn i den befintliga kodbasen kan agenten:
- använda **[Utility Libraries]({{< ref "/hub/scripting/reference/utility-browser" >}})** som referens för hjälpfunktioner
- följa befintliga mönster för GEGL-operationer och lagerhantering
- återanvända boilerplate från etablerade plug-ins

## Exempel på arbetsflöde

Du kan direkt be agenten att generera en fullständig plug-in genom att beskriva det önskade funktionsresultatet:

> "Med hjälp av de tillgängliga Scheme-verktygen och exemplen i arbetsytan, skriv ett nytt plugin-program som skapar en 50 % horisontell guide på den aktiva bilden och kallar den 'Center Guide'."

Agenten kommer att söka efter hur man skapar guider, identifiera rätt verktygsfunktion (som `lumi-image-add-hguide-percent` från `common.scm`) och generera den fullständiga `.scm`-filen med rätt registreringsskylt.

## Bästa metoder

- **Var specifik**: Beskriv exakt vad du vill att plugin-programmet ska göra.
- **Referensverktyg**: Uppmuntra agenten att titta på `share/lumi/scripts/`-katalogen för hjälpmedel på hög nivå.
- **Granska och testa**: Testa alltid plugin-programmet som genereras av AI, det är ofta en iterativ och kreativ process.
