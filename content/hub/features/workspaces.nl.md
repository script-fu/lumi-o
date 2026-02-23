---
title: "Werkruimtes"
type: docs
---
Een werkruimte is een opgeslagen momentopname van uw gehele UI-omgeving: welke panelen open zijn en waar, de canvasdecoraties en opvulling voor zowel de normale als de volledige schermweergave, het actieve thema en de pictogrammenset, de lay-out van de gereedschapskist, het actieve palet en uw gereedschapsinstellingen. Met Lumi kunt u zoveel benoemde werkruimten opslaan als u wilt en er direct tussen schakelen: alle geopende afbeeldingen worden ter plekke bijgewerkt, opnieuw opstarten is niet nodig.

## Wat een werkruimte bespaart

In elke benoemde werkruimte wordt het volgende onafhankelijk opgeslagen:

| Onderdeel | Wat het omvat |
| :--- | :--- |
| **Indeling** | Vensterpositie en -grootte, dockindeling (linker- en rechterpaneelkolommen, welke panelen open zijn en in welke volgorde), modus voor één of meerdere vensters, gemaximaliseerde status, zichtbaarheid en positie van de tabbalk |
| **Gereedschapsopties** | De huidige instellingen voor elk gereedschap (penseelgrootte, hardheid, kromtrekkingsgedrag, etc.) |
| **Invoerapparaten** | Configuratie van invoerapparaat: drukcurven, knoptoewijzingen, astoewijzingen voor stylus en andere apparaten |
| **Canvasdecoraties** | Standaardinstellingen per werkruimte voor linialen, schuifbalken, hulplijnen, raster, selectiemarkering, laaggrens en canvasgrens — ingesteld via **Voorkeuren → Afbeeldingsvensters → Standaardweergave** en **Volledige schermweergave**, onafhankelijk voor normale en volledige schermweergave |
| **Canvasvulling** | Opvullingsmodus en kleur per werkruimte voor normale en volledige schermweergave - ingesteld via **Voorkeuren → Afbeeldingsvensters → Standaardweergave** |
| **Thema en pictogrammen** | Actief thema, donkere/lichte kleurvariant, pictogrammenset, overschrijven van pictogramgrootte en lettertypeschaal |
| **Gereedschapskist** | FG/BG-widgetpositie (boven/onder/links/rechts), FG/BG-schaal, zichtbaarheid van de Wilber-mascotte, kopteksten van gereedschapsgroepen |

Het actieve **palet** en **tool preset** worden ook per werkruimte vastgelegd en hersteld wanneer u overschakelt.

> **Canvasdecoraties en opvulling** worden beheerd door
> **Voorkeuren → Afbeeldingsvensters → Geavanceerde vensteropties → Standaardweergave** (normale weergave)
> en **Verschijning op volledig scherm** (volledig scherm). Pas die instellingen naar wens aan,
> sla vervolgens de werkruimte op. De **Bekijk menu**-items (linialen, hulplijnen, etc.) zijn lokaal voor de
> huidige afbeeldingsvenster en worden niet per werkruimte opgeslagen.

### Live updates op schakelaar

Wanneer u van werkruimte wisselt, worden alle geopende afbeeldingsvensters onmiddellijk bijgewerkt: linialen, hulplijnen, schuifbalken, opvulkleur en alle andere weergave-instellingen veranderen op hun plaats zonder dat u afbeeldingen hoeft te sluiten en opnieuw te openen.

## Toegang

**Bewerken → Voorkeuren → Werkruimte**

Het bovenste gedeelte van de pagina Werkruimtevoorkeuren bevat al uw opgeslagen werkruimten en biedt bedieningselementen om deze te beheren.

## Een werkruimte creëren

Stel uw panelen, gereedschappen en palet precies in zoals u dat wilt, en doe dan het volgende:

1. Open **Bewerken → Voorkeuren → Werkruimte**.
2. Klik op **Indeling opslaan als…**.
3. Voer een naam in en klik op **Opslaan**.

De nieuwe werkruimte verschijnt in de vervolgkeuzelijst **Actieve indeling** en in het menu **Windows**.

## Van werkruimte wisselen

Er zijn twee manieren om te wisselen:

- **Venstersmenu**: de lay-outnamen verschijnen onder **Vensters → Lay-out** voor snelle toegang vanaf het canvas.
- **Voorkeuren → Werkruimte**: Selecteer een lay-out in de vervolgkeuzelijst **Actieve lay-out** en klik op **Laad lay-out opnieuw**.

Overschakelen gebeurt onmiddellijk: Lumi herbouwt de paneelindeling, herstelt gereedschapsopties, herlaadt apparaatinstellingen, werkt canvasdecoraties, opvulling, thema en gereedschapskistindeling bij, allemaal zonder opnieuw op te starten.

## Werkruimten beheren

Vanuit **Bewerken → Voorkeuren → Werkruimte**:| Actie | Effect |
| :--- | :--- |
| **Indeling opslaan** | Overschrijft de huidige werkruimte met uw huidige instellingen. |
| **Indeling opslaan als...** | Creëert een nieuwe benoemde werkruimte op basis van uw huidige instellingen. |
| **Lay-out hernoemen…** | Hernoemt de geselecteerde werkruimte. |
| **Indeling opnieuw laden** | Past de geselecteerde werkruimte onmiddellijk toe. |
| **Indeling verwijderen...** | Verwijdert permanent de geselecteerde werkruimte en de bijbehorende bestanden. |

## Persistentie-instellingen

Het onderste deel van de Workspace-voorkeurenpagina bepaalt wat Lumi automatisch opslaat:

- **Sla vensterposities op bij afsluiten**: Wanneer ingeschakeld, worden de dock- en vensterposities elke keer dat u afsluit naar schijf geschreven.
- **Open vensters op dezelfde monitor**: Opent elk venster opnieuw op de monitor waarop het zich tijdens de laatste sessie bevond.
- **Toolopties opslaan bij afsluiten**: slaat de huidige gereedschapsinstellingen op bij het afsluiten.
- **Invoerapparaatinstellingen opslaan bij afsluiten**: slaat de stylus- en apparaatconfiguratie op bij het afsluiten.

Deze instellingen gelden per werkruimte: elke lay-out behoudt zijn eigen opgeslagen status, onafhankelijk van elkaar.

## Voorbeeldworkflows

Een paar manieren waarop kunstenaars meerdere werkruimtes kunnen gebruiken:

- **Schilderij** — grote penseeldocks, warme opvulkleur (ingesteld in Voorkeuren → Afbeeldingsvensters → Standaarduiterlijk), uw favoriete themavariant
- **Inkt** — hulplijnen en canvasgrens ingeschakeld, schuifbalken ingeschakeld (ingesteld in Voorkeuren → Standaardweergave), neutrale opvulkleur
- **Roughs** — dokken verborgen, geen linialen of raster, donkere opvulling, compacte pictogramgrootte om de canvasruimte te maximaliseren
- **Focus op volledig scherm** — verschillende opvulkleur- en decoratie-instellingen in weergave op volledig scherm versus standaardweergave, dus het schakelen tussen volledig scherm geeft een echt andere werkomgeving
- **Scripting** — scriptpaneel geopend, lettergrootte verhoogd voor leesbaarheid, andere pictogrammenset