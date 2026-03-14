---
title: "Procedur webbläsare"
type: docs
---
Procedurbläddraren är det primära referensverktyget för att upptäcka de hundratals funktioner som finns tillgängliga i Lumis procedurdatabas (PDB). Eftersom alla verktyg, filter och skript i Lumi måste registreras i PDB för att kunna anropas, är den här webbläsaren faktiskt en komplett PDB-utforskare.

## Öppna procedurbläddraren

Gå till **Hjälp → Programmering → Procedurbläddrare**.

Du kan också komma åt den från Scheme Console via **Bläddra**.

## Vad den visar

Procedurbläddraren kan lista alla procedurer som för närvarande är registrerade i det preliminära budgetförslaget, oavsett deras ursprung. Det är standard att söka efter "internt", för att visa de internt registrerade kärnprocedurerna.

- **Interna procedurer**: Kärnfunktioner för bildmanipulering, lagerhantering och verktygskontroll.
- **Externa insticksprogram**: Procedurer som tillhandahålls av kompilerade C/C++-pluginprogram eller beständiga tillägg.

## Sökning och filtrering

- **Sökruta**: Filtrerar procedurer efter namn, beskrivning eller författare. Om du rensar sökfältet visas alla tillgängliga procedurer.
- **Söktyp**: Sökrullgardinsmenyn låter dig filtrera efter specifika fält. Om du ställer in den till **efter typ** och söker efter "internt", kommer listan att begränsas till att endast visa de internt registrerade kärnprocedurerna.
- **Detaljerad vy**: Genom att klicka på en procedur visas dess parametrar, returvärden, författare, datum och en beskrivning av vad den gör.

Detta är viktigt för att hitta det exakta namnet och argumentsignaturen för en funktion du vill anropa från ditt skript.