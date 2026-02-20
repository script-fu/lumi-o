---
title: "Plug-in webbläsare"
type: docs
---
Plug-In Browser låter dig utforska menysystemet och se var specifika plug-ins är installerade.

## Öppna plug-in webbläsaren

Gå till **Hjälp → Programmering → Plug-In Browser**.

## Vad den visar

Medan Procedurbläddraren fokuserar på de råa *funktionerna* i PDB, är Plug-In Browser en delmängdsvy fokuserad på upptäckt av användargränssnittet. Det filtrerar specifikt PDB för att visa "saker som ser ut som menyinstallerade plugin-program."

Internt använder detta en fråga som endast returnerar procedurer som har både en associerad fil på disken och en registrerad menysökväg.

- **Menyträd**: Visar en trädrepresentation av Lumi-menystrukturen.
- **Plug-in-platser**: Hjälper dig att hitta var ett nyligen installerat plug-in har kapslat sig i menyerna.
- **Metadata**: Visar information om plugin-programmets författare, version och datum.

## Användning

Använd plugin-webbläsaren när du vet att en funktion finns men inte kan hitta den i menyerna, eller när du designar din egen plug-in och vill se var liknande verktyg finns.