---
title: "Iteration"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: df3e2118b9a580de4eed6ac56d9717aa3cbf555ab66bb49fabb4164b2994af91
url: "hub/scripting/fundamentals/Iteration/_index"
---
Iteration är en grundsten i programmering: den låter skript upprepa handlingar och bearbeta datasamlingar effektivt. I Scheme ger iteration verktyg för att automatisera repetitiva uppgifter, manipulera datastrukturer och skapa sofistikerade körningsmönster.

### Iterationens roll i Scheme

Iteration fyller flera viktiga syften:
- **Automatisera upprepning:** utför samma handling flera gånger utan att duplicera kod.
- **Öka effektiviteten:** bearbeta datastrukturer iterativt för systematiska storskaliga operationer.
- **Effektivisera koden:** iteration tar bort redundans och gör koden mer koncis, läsbar och underhållbar.

### Typer av iteration

Scheme erbjuder flera konstruktioner:
- **map:** tillämpar en funktion på varje element i en lista och returnerar en ny lista med resultat.
- **for-each:** liknar `map`, men används för att köra en funktion på varje element utan att returnera ett resultat.
- **do:** en allmän loopkonstruktion för många typer av iterativa processer.
- **rekursion:** en kraftfull teknik där funktioner anropar sig själva.

### Hur iteration fungerar

Typiskt involverar det:
1. **Definiera upprepning:** ange handlingen som ska upprepas och data eller intervall att bearbeta.
2. **Köra i sekvens:** upprepa handlingen för varje element, steg eller villkor tills det är klart.
3. **Returnera resultat (valfritt):** beroende på konstruktion kan iteration ge ett resultat eller ändra tillstånd.

Dessa konstruktioner hjälper dig att skriva anpassningsbara, effektiva och eleganta skript för komplexa uppgifter.
