---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi kan rita lagerstacken med OpenGL och måla penseldrag på samma sätt. Navigeringen på duken förblir jämn. Stora airbrushar och stora texturstämplar är mycket snabbare. Om ett visst verktyg målar så här, lagras det valet i verktygets förinställning.

## Lagerstacken

Bildfönstret visar den sammansatta lagerstacken. Med OpenGL hålls den bilden som rutor på grafikprocessorn, redan förberedda för skärmen. Panorering, zoom, rotation och spegling ändrar vyn av de rutorna. På en stor duk förblir gesterna jämna, eftersom navigeringen är ett byte av vy över en bild som redan finns där.

Redigeringar uppdaterar de rutor de rör. Hjälplinjer, penselpekaren och markeringskonturen ritas över den bilden.

Medan pennan är nere kan ett drag som använder OpenGL visas på plats bland lagren runt det, inklusive lager ovanför det som målas. Märket skrivs in i lagret när pennan lyfts.

## Drag, per verktygsförinställning

OpenGL-målning är ett val på verktygsförinställningen, vid sidan av penseln och dynamiken. En förinställning kan måla på grafikprocessorn. En annan kan behålla den vanliga dragvägen. Att återvända till ett verktyg tar med det valet tillbaka.

Vinsten är störst där stämplarna är stora. På den vanliga vägen hamnar en bred airbrush efter pennan när märket växer: avtrycken köar, och draget får luckor, också medan pennan hålls nere och penseln fortsätter att avge färg. Med OpenGL fortsätter den airbrushen att sätta märken i takt med handen.

En stor texturstämpel visar samma förändring. Den vanliga vägen lägger mer tid på varje avtryck när stämpeln växer, tills ett drag stannar av. OpenGL-draget följer gesten. Stämplar som bär en ytgräng kan också ritas med OpenGL.

Borstmärken kan ritas på det här sättet också. En borststämpel med rund spets kan målas med OpenGL medan borsten ändras längs draget. Ett kalligrafiskt drag kan ritas som ett sammanhängande band, och ytans textur kan maskera det bandet. [Ytans textur](/hub/features/paintbrush/#ytans-textur) och [borst längs draget](/hub/features/paintbrush/#borst-längs-draget) hör till själva penseln.

En del drag stannar på den vanliga vägen. Spektral blandning, ett filter på det målade lagret och vissa sammansättningslägen hör dit. De märkena målas ändå.

## När den vanliga duken blir kvar

OpenGL används när grafikhårdvaran kan köra det och bilden ryms i det minne som avsatts för duken. Annars ritar Lumi fönstret som förut. Den publicerade AppImage-filen bär sina egna grafikbibliotek. På en drivrutin som de biblioteken inte kan använda, däribland NVIDIAs proprietära drivrutin, stannar duken på den vanliga vägen.

## Cinnamon på X11 och GNOME på Wayland

Lumi utvecklas på Debian med Cinnamon och X11. Det är fortfarande utvecklingsmiljön. GNOME på Wayland har testats för programmets allmänna utseende och för den här OpenGL-renderingen. Navigeringen på duken är jämn där också.
