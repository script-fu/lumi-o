---
title: "Publicatielayout"
type: docs
url: "hub/features/publishing-layout"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: dc0367028ed8f6b4e1508c309384967daa43a4148f8d70f00880173a0a1fca7d
---

Illustraties voor druk en publicatie vragen vaak om meer dan alleen een canvasformaat. Pagina's hebben snijranden, spreads hebben middennaden, en belangrijke inhoud moet soms uit de buurt blijven van zones die worden weggesneden of in de rugmarge verdwijnen. De publicatielayout-tools van Lumi houden die aandachtspunten zichtbaar tijdens het schilderen, zonder ze in het kunstwerk plat te drukken.

Layoutgrenzen worden per afbeelding opgeslagen, bij het project bewaard en kunnen worden uitgeschakeld wanneer ze niet nodig zijn. Het doel is boek-, strip- en printf workflows een duidelijk gevoel van paginastructuur te geven, terwijl de gelaagde afbeelding eronder volledig bewerkbaar blijft.

## Afloop en snijrand

Afloop bepaalt hoe ver artwork voorbij de uiteindelijke paginarand doorloopt. Lumi toont het snijgebied als de actieve paginagrens binnen het canvas, met de afloopmarge als gearceerde overlay eromheen. Zo kun je achtergronden en randdetails schilderen die het snijden moeten overleven, zonder te raden waar de afgedrukte pagina eindigt.

Afmetingen kunnen in eenheden worden ingesteld die bij de opdracht passen — inches, millimeters of een andere gangbare druk-eenheid, niet alleen pixels.

## Rugmarge en spreads

Bij dubbele pagina's markeert de rugmarge de beschermde zone rond de middennaad waar belangrijke inhoud vermeden moet worden. Wanneer ingeschakeld, toont Lumi rugmargebanden over de spread, zodat gezichten, tekst en focuspunten buiten het bindgebied blijven terwijl de volledige spread één doorlopend canvas blijft.

Dat is vooral nuttig voor strips, prentenboeken en werk dat als tegenoverliggende pagina's wordt gedrukt in plaats van losse vellen.

## Compositiehulplijnen

Optionele randhulplijnen markeren het bijgesneden paginagebied met subtiele compositieticks. Hulplijnen kunnen per pagina of over de hele spread lopen, en derden, gouden snede of vijfdelen gebruiken, afhankelijk van hoe de layout beoordeeld moet worden.

Ze dienen als stille referentie tijdens layout en afwerking en helpen plaatsing af te lezen tegen de pagina die daadwerkelijk wordt gedrukt, niet alleen tegen het volledige digitale canvas.

## Layout op het canvas bekijken

Layout-overlays worden beheerd vanuit het menu Beeld. Afloop-, rugmarge- en hulplijngebieden kunnen afzonderlijk of samen worden getoond, zodat een kunstenaar zich kan richten op het deel van de publicatiestructuur dat op dat moment telt.

Afbeelding > Layout inschakelen schakelt layoutgrenzen voor de huidige afbeelding in of uit. Wanneer layout uit staat, zijn overlays verborgen en wijken weergaveschakelaars opzij, maar de grensinstellingen blijven met het bestand bewaard voor later gebruik.

## Opgeslagen bij het project

Layoutinstellingen reizen mee met het `.lum`-project. Later openen herstelt afloop, rugmarge, overlay-uiterlijk, hulplijnkeuzes en of layout voor dat bestand is ingeschakeld. Publicatiebewuste setup blijft zo onderdeel van de werkstatus van het kunstwerk, niet een tijdelijke weergavevoorkeur.

Voor kunstenaars die wisselen tussen schetsen, schilderen en printvoorbereiding blijft alles op één plek: dezelfde gelaagde afbeelding, met publicatiestructuur wanneer de pagina die nodig heeft.
