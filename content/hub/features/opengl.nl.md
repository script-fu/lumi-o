---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi kan de lagenstapel met OpenGL tekenen en penseelstreken op dezelfde manier schilderen. De navigatie over het canvas blijft vloeiend. Grote airbrushes en grote textuurstempels zijn veel sneller. Of een bepaald gereedschap zo schildert, staat opgeslagen bij de gereedschapsvoorinstelling.

## De lagenstapel

Het afbeeldingsvenster toont de samengestelde lagenstapel. Met OpenGL wordt dat beeld als tegels op de grafische processor gehouden, al voorbereid voor het scherm. Verschuiven, zoomen, draaien en spiegelen veranderen de weergave van die tegels. Op een groot canvas blijven de gebaren vloeiend, omdat navigeren een verandering van aanzicht is over een beeld dat er al is.

Bewerkingen werken de tegels bij die ze raken. Hulplijnen, de penseelcursor en de omtrek van de selectie worden over dat beeld getekend.

Zolang de pen neergedrukt blijft, kan een streek die OpenGL gebruikt ter plekke verschijnen tussen de lagen eromheen, ook lagen boven de laag die wordt geschilderd. De markering wordt in de laag geschreven wanneer de pen wordt opgetild.

## Streken per gereedschapsvoorinstelling

Schilderen met OpenGL is een keuze van de gereedschapsvoorinstelling, naast het penseel en de dynamiek. De ene voorinstelling kan op de grafische processor schilderen. Een andere kan het gewone streekpad houden. Terugkeren naar een gereedschap brengt die keuze weer mee.

De winst is het grootst waar de stempels groot zijn. Op het gewone pad raakt een brede airbrush achter op de stylus naarmate de markering groeit: deppen stapelen zich op in een wachtrij, en de streek krijgt gaten, ook terwijl de pen wordt vastgehouden en het penseel blijft uitstoten. Met OpenGL blijft die airbrush markeringen in het tempo van de hand plaatsen.

Een grote textuurstempel laat dezelfde verandering zien. Het gewone pad doet langer over elke dep naarmate de stempel groeit, tot een streek vastloopt. De OpenGL-streek blijft bij het gebaar. Stempels die een oppervlaktekorrel dragen, kunnen eveneens met OpenGL worden getekend.

Markeringen van borstelharen kunnen ook zo worden getekend. Een stempel van borstelharen met ronde punt kan met OpenGL worden geschilderd terwijl de haren langs de streek veranderen. Een kalligrafische streek kan als één doorlopend lint worden getekend, en oppervlaktetextuur kan dat lint maskeren. [Oppervlaktetextuur](/hub/features/paintbrush/#oppervlaktetextuur) en [borstelharen langs de streek](/hub/features/paintbrush/#borstelharen-langs-de-streek) horen bij het penseel zelf.

Sommige streken blijven op het gewone pad. Spectrale menging, een filter op de geschilderde laag en sommige overvloeimodi horen daarbij. Die markeringen worden evengoed geschilderd.

## Wanneer het gewone canvas blijft

OpenGL wordt gebruikt wanneer de grafische hardware het kan uitvoeren en de afbeelding in het geheugen past dat voor het canvas is gereserveerd. Anders tekent Lumi het venster zoals voorheen. De gepubliceerde AppImage neemt eigen grafische bibliotheken mee. Op een stuurprogramma dat die bibliotheken niet kunnen gebruiken, waaronder het propriëtaire stuurprogramma van NVIDIA, blijft het canvas op dat gewone pad.

## Cinnamon op X11, en GNOME op Wayland

Lumi wordt ontwikkeld op Debian met Cinnamon en X11. Dat is nog steeds de ontwikkelomgeving. GNOME op Wayland is getest op het algemene uiterlijk van de toepassing en op deze OpenGL-weergave. De navigatie over het canvas is daar eveneens vloeiend.
