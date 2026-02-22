---
title: "Palet-editor"
type: docs
---
In de Palette Editor kunt u een Lumi-palet bouwen en beheren. Het bevat uw pigmentset, slaat de mengsels op die u opslaat met de Palette Mixer, registreert de kleuren die u daadwerkelijk hebt gebruikt tijdens het schilderen, en laat u de waardestructuur en verlopen voor het palet configureren.

## Een palet selecteren

Een palet is meer dan een verzameling pigmenten: het is een stilistische verbintenis. Veel kunstenaars werken met een kleine, vaste set pigmenten die ze goed kennen: de manier waarop ze mengen, de neutrale kleuren die ze produceren, de temperatuurverschuivingen daartussen. Die vertrouwdheid wordt onderdeel van hun visuele stem. Een schilder kan een warm palet met weinig chroma gebruiken voor figuurwerk en een apart high-key palet voor landschappen, of hij kan al zijn werk binnen een enkele set van vier pigmenten doen als een opzettelijke beperking die een oeuvre verenigt.

Lumi ondersteunt deze manier van werken. Elk palet heeft zijn eigen pigmenten, mengsels, waardestructuur en kleurverlopen. Als je van palet wisselt, verandert het hele kleursysteem: de kaart, de mixer en de beschikbare mixen worden allemaal bijgewerkt om de nieuwe set weer te geven.

Een vervolgkeuzelijst bovenaan de Palet-editor selecteert het actieve palet. Lumi wordt geleverd met drie paletten in de **Standaard**-groep:

| Palet | Karakter |
| :--- | :--- |
| **Standaard** | Een veelzijdig, warm palet dat het volledige tintwiel bedekt. Goed uitgangspunt voor de meeste onderwerpen. |
| **Meester** | Een groot palet met volledig spectrum voor schilders die maximale kleurdekking en expliciete controle over vergrijsde assen willen. |
| **Zorn** | Een gelimiteerd palet met vier pigmenten, gebaseerd op de aanpak van Anders Zorn. Omvat een verrassend breed scala aan warme huidtinten en neutrale kleuren met een laag chromagehalte met een minimale pigmentset. |

Paletten kunnen ook worden gemaakt, geïmporteerd of gedupliceerd vanaf het tabblad Paletten.

## Paletpigmenten

In het gedeelte **Paletpigmenten** bovenaan de paletweergave staan uw primaire gegevens: de basispigmenten waaruit de rest van het palet is opgebouwd. Dit zijn de inputs voor het spectrale mengsysteem. Secundaire en tertiaire bestanden worden hier automatisch uit gegenereerd en worden gebruikt om de paletkaart te vullen

## Opgeslagen mixen

Het gedeelte **Opgeslagen mengsels** bevat kleuren die u expliciet uit de Paletmixer hebt bewaard met behulp van **Toevoegen aan palet**. Dit zijn uw afgeleide kleuren: de resultaten van spectrale menging, toon- en chroma-aanpassingen, opgeslagen voor hergebruik.

Opgeslagen mixen zijn onderverdeeld in vijf waardegroepen:

| Band | Standaard helderheidsbereik |
| :--- | :--- |
| Hoge sleutel | 80 – 100% |
| Boven midden | 60 – 80% |
| Midden | 40 – 60% |
| Lager midden | 20 – 40% |
| Diep | 0 – 20% |

Lumi plaatst elke opgeslagen mix automatisch in de juiste band op basis van zijn perceptuele lichtheid (CIE L\*). Dit ordent uw mixen op waarde in plaats van te zoeken in een platte lijst, en komt doorgaans overeen met de manier waarop een kunstenaar over kleur denkt.

Opgeslagen mixen kunnen worden hernoemd via de knop **Rename Custom** of het contextmenu.

## Gebruikte mixen

De sectie **Gebruikte mengsels** is een door verf geactiveerde geschiedenis. Elke keer dat een kleur uit het palet op het canvas wordt toegepast, wordt deze hier vastgelegd. Gebruikte mixen worden gerangschikt van meest naar minst recent.

Deze sectie is handig voor het ophalen van een kleur waarmee u hebt geschilderd, maar die u niet expliciet hebt opgeslagen. Om een ​​gebruikte mix permanent te behouden, selecteert u deze en klikt u op **Promoot**. De mix wordt verplaatst naar de opgeslagen mixen in de juiste waardecategorie.

Gebruikte mixen worden per palet opgeslagen en blijven tussen sessies bewaard.

## WaardebandenWaardebanden definiëren waar de grenzen tussen de vijf lichtheidszones liggen. Standaard verdelen ze de helderheid gelijkmatig over het bereik van 0–100%, maar u kunt ze aanpassen aan de toonstructuur van uw onderwerp. Het is nuttig voor schilders om waardegroepen en de gaten daartussen te definiëren en beheren.

### De waardebandschuifregelaar

De **Waardebanden-expander** in de Palette Editor bevat een schuifregelaar met vijf versleepbare scheidingslijnen. Versleep een scheidingslijn om de grens tussen aangrenzende banden te verschuiven. Het label boven de schuifregelaar toont de naam en het exacte percentagebereik van de actieve band.

**Knoppen:**

| Knop | Effect |
| :--- | :--- |
| **Annuleren** | Zet de schuifregelaar terug naar de laatst toegepaste status |
| **Kopiëren** | Kopieert de huidige bandconfiguratie naar het klembord |
| **Plakken** | Plakt een gekopieerde bandconfiguratie uit een ander palet |
| **Standaardwaarden** | Herstelt de fabrieksinstellingen voor gelijke verdeling |
| **Solliciteer** | Voert de wijzigingen door en genereert het palet opnieuw |

**Toepassen** is vereist om de wijzigingen permanent te maken. Het activeert een volledige regeneratie van het palet en verwijdert alle opgeslagen mixen waarvan de lichtheid niet langer binnen een bepaalde band valt. Lumi toont een bevestigingsvenster waarin wordt vermeld hoeveel mixen er worden verwijderd voordat verder wordt gegaan.

### Waardebanden en de paletkaart

De Paletkaart geeft het palet weer als een tintwiel met 36 tintsectoren (elk 10°) en 15 helderheidscellen gerangschikt als concentrische ringen. Elke band komt overeen met drie ringen: de vijf banden × 3 ringen = 15 cellen in totaal.

Door de waardebanden aan te passen, verschuift welke lichtheidswaarden in elke ringlaag terechtkomen. Een band die naar het donkere uiteinde is samengedrukt, zorgt ervoor dat de drie ringen een smaller toonbereik overspannen; een brede band geeft de drie ringen meer tonale spreiding. Dit is hoe dezelfde Palette Map-structuur zich aanpast aan paletten die zijn afgestemd op verschillende toonprioriteiten.

## Paletverlopen

Elk palet kan een of meer **Verlopen** opslaan: vloeiende progressies afgeleid van paletitems die op het canvas kunnen worden toegepast als verloopvullingen of kunnen worden gebruikt als referentiestroken.

Verlopen worden beheerd in de **Verlopen-expander**. De combo bovenaan geeft een overzicht van de verlopen in het huidige palet. **Toevoegen** maakt een nieuw verloop. **Verwijderen** verwijdert de geselecteerde. **Hernoemen** hernoemt het.

### Verloopeditor

De **Gradient Editor-expander** configureert het geselecteerde verloop. Elke kleurovergang heeft drie eindpunten (**A**, **B** en **C**), weergegeven als kleurstalen. Klik op een staal om dit tot het actieve eindpunt voor bewerking te maken.

Elk eindpunt kan worden ingesteld door op **Kies** te klikken en vervolgens op een paletitem in de Paletkaart of de paletweergave te klikken. Het eindpunt is via UID aan dat paletitem gekoppeld; als de invoer verandert, wordt de gradiënt bijgewerkt.

**Besturingselementen per eindpunt:**

| Controle | Effect |
| :--- | :--- |
| **Kracht** | Hoe sterk de eindpuntkleur bijdraagt ​​ten opzichte van zijn buren |
| **Dekking** | Alfa van de eindpuntkleur in het verloop |
| **Curve** | Gamma-aanpassing voor de kleurafwijking vanaf dit eindpunt |

**Verdelingsschuifregelaars** (S1, S2, S3) stellen in waar de drie middelpunten tussen de eindpunten langs de verloopstrook vallen. Als u ze opnieuw instelt, worden de middelpunten weer op gelijke afstand gebracht.

De verloopvoorbeeldstrook bovenaan het Gradient Editor-blok toont het resultaat van de huidige eindpunt- en distributie-instellingen.

## Palet dockbaarHet koppelbare **Palet** (**Panelen > Palet**) is een eenvoudiger, op lezen gericht paneel waarmee u door elk palet kunt bladeren en kleuren kunt selecteren. Het toont dezelfde driedelige weergave (Paletpigmenten, Opgeslagen mengsels, Gebruikte mengsels) zonder de uitbreidingen Waardebanden en Verlopen.

Met een vervolgkeuzelijst voor paletkiezers bovenaan kunt u schakelen tussen alle beschikbare paletten. Klik op een item om het in te stellen als voorgrondkleur. Dubbelklik om de kleurnaameditor te openen. Voor beschrijfbare paletten zijn de acties Kleur bewerken, Nieuwe kleur van FG en Kleur verwijderen beschikbaar in de knoppenbalk.

De Palette Dockable is bedoeld voor snelle kleurtoegang tijdens het schilderen wanneer de volledige Palette Editor te veel ruimte in beslag zou nemen.

## Paletten Tab

Het tabblad **Paletten** (beschikbaar als vastzetbaar tabblad) toont het actieve palet in compacte modus. Het sluit de pigmenten uit om zich te concentreren op opgeslagen mengsels