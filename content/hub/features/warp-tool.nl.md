---
title: "Verdraaiingsgereedschap"
type: docs
---
Het gereedschap Verdraaien duwt, trekt en laat pixels vrij over het canvas lopen. In Lumi gaat het verder dan de meeste implementaties: het kan een hele lagengroep vervormen – ongeacht hoeveel geneste lagen en maskers het bevat – als een enkel verenigd object, zonder dat de structuur plat wordt of verloren gaat.

## Overzicht

Selecteer een laag en sleep eroverheen om pixels in elke richting te verplaatsen. De warp is niet-destructief terwijl u werkt: u kunt individuele streken ongedaan maken en opnieuw uitvoeren, de penseelgrootte of het gedrag tussen streken wijzigen en doorgaan met verfijnen totdat u een commit maakt. Door vast te leggen wordt de geaccumuleerde verplaatsingskaart destructief toegepast op de pixelgegevens van de laag.

Wanneer een **groepslaag** is geselecteerd, werkt het gereedschap op de groep als geheel. U ziet en communiceert met een live preview van de gehele samengestelde groep. Bij het vastleggen wordt dezelfde warp nauwkeurig en onafhankelijk toegepast op elke onderliggende laag en elk masker binnen de groep, waardoor de volledige laagstructuur behouden blijft.

## Groepsvervorming

Het kromtrekken van een groep is de belangrijkste mogelijkheid waarmee Lumi's warptool zich onderscheidt.

### Het probleem dat het oplost

In de meeste verfprogramma's vereist het kromtrekken van een illustratie met meerdere lagen ofwel het eerst plat maken van de groep (waarbij de laagstructuur wordt vernietigd) ofwel het kromtrekken van elke laag afzonderlijk en proberen ze op het oog met elkaar te matchen (vervelend en onnauwkeurig). Geen van beide benaderingen behoudt de oorspronkelijke structuur voor verdere niet-destructieve bewerking.

Lumi vervormt de hele groep als één item en distribueert vervolgens exact dezelfde transformatie naar elke laag erin.

### Hoe het werkt

Wanneer u een groep selecteert en een kromtrekkingsstreek begint, bouwt Lumi een **zwevende voorbeeldlaag** op basis van de samengestelde projectie van de groep. Als de groep een masker heeft, wordt het masker in het voorbeeld ingebakken, zodat het voorbeeld het uiteindelijke uiterlijk nauwkeurig weergeeft. U tekent uw warpstreken rechtstreeks op dit voorbeeld: wat u ziet, is precies wat u krijgt.

Op commit, Lumi:

1. Past de verplaatsing toe op elke basislaag binnen de groep (inclusief diep geneste lagen in subgroepen), waarbij het canvas van elke laag net voldoende wordt uitgebreid om het volledige vervormingsgebied te bestrijken.
2. Past in dezelfde doorgang dezelfde verplaatsing toe op elk masker binnen de groep.
3. Hervat de automatische berekening van de grenzen van de groep, zodat de grootte van de groep wordt aangepast aan de nieuw vervormde onderliggende elementen.
4. Snijdt elke kromgetrokken laag terug naar de daadwerkelijk geschilderde inhoud om de bestandsgrootte compact te houden.
5. Verwijdert de voorbeeldlaag en genereert de groepsprojectie opnieuw van de bijgewerkte onderliggende elementen.

Dit alles gebeurt binnen één enkele ongedaanmakingsstap. Na het vastleggen ziet de groep er precies zo uit als in het voorbeeld, met elke laag en masker intact.

### Maskers

De optie **Warpmaskers** (standaard ingeschakeld) zorgt ervoor dat maskers op elke laag en groep binnen het warpdoel een identieke verplaatsingstransformatie krijgen. Laagmaskers bewegen met hun lagen mee: een masker dat de omtrek van een personage afsneed, blijft diezelfde omtrek afknippen na het kromtrekken.

Wanneer **Warp Masks** is uitgeschakeld, wordt alleen de laaginhoud verplaatst; maskers behouden hun oorspronkelijke posities.

## Gereedschapsopties

### Gedrag

| Modus | Effect |
| :--- | :--- |
| **Verplaatsen** | Duwt pixels in de richting van de lijn. De primaire modus voor het meeste kromtrekwerk. |
| **Groeien** | Vergroot pixels naar buiten vanuit het midden van het penseel. |
| **Krimpen** | Trekt pixels naar binnen, richting het midden van het penseel. |
| ** Met de klok mee draaien ** | Roteert pixels met de klok mee rond het midden van het penseel. |
| ** Draai tegen de klok in ** | Roteert pixels tegen de klok in rond het midden van het penseel. |
| **Wissen** | Verwijdert kromtrekkingsverplaatsing en herstelt pixels naar hun oorspronkelijke posities. |
| **Glad** | Verspreidt verplaatsing en verzacht abrupte overgangen tussen kromgetrokken en niet-kromgetrokken gebieden. |

### Penseelbediening

- **Grootte**: Diameter van de kettingborstel in pixels. Grotere borstels verplaatsen bredere gebieden met een zachtere neerslag; kleinere borstels geven nauwkeurige, plaatselijke controle.
- **Hardheid**: Falloff van midden naar rand. Hoge hardheid zorgt voor een uniforme verplaatsing over het volledige borsteloppervlak; lage hardheid concentreert het effect in het midden.
- **Kracht**: hoe ver pixels per streek worden verplaatst. Lagere sterkte maakt subtiele, geleidelijke vormgeving mogelijk; hogere kracht produceert dramatische, snelle bewegingen.

### Slagtiming

- **Slag tijdens beweging** (alleen bewegingsmodus): past continu warp toe terwijl de muis beweegt, in plaats van alleen op basis van een timerpuls. Gebruik dit voor vloeiende, penseelachtige streken waarbij u wilt dat de verplaatsing de cursor direct volgt.
- **Periodiek beroerte**: past warp toe met een vast tijdsinterval terwijl de muisknop ingedrukt wordt gehouden. Gebruik deze voor de groei-, krimp- en wervelmodi waarbij continue circulaire toepassing de bedoeling is.
- **Tarief**: de frequentie van periodieke beroerte-applicaties.

### Kwaliteit

- **Interpolatie**: de bemonsteringsmethode die wordt gebruikt bij het vastleggen. Lineair is snel en soepel voor het meeste werk; Cubic en Nohalo zorgen voor een hogere natuurgetrouwheid voor fijne details.
- **Preview van hoge kwaliteit**: gebruikt de sampler van commit-kwaliteit tijdens de interactieve preview. Langzamer, maar het voorbeeld komt exact overeen met het vastgelegde resultaat.

### Groepsopties

- **Verdraaiingsgebied uitbreiden** (alleen groepsvervorming): het aantal pixels dat is toegevoegd als transparante marge rond het groepsvoorbeeld aan alle zijden. Dit geeft verplaatste inhoud ruimte om in te bewegen. De standaard 256 px is voldoende voor het meeste werk; verklein het voor grote afbeeldingen waarbij het geheugen belangrijk is, of verhoog het voor zeer grote verplaatsingsstreken.
- **Warpmaskers**: of dezelfde warp moet worden toegepast op laag- en groepsmaskers. Standaard ingeschakeld.

## Ongedaan maken en opnieuw uitvoeren

Elke slag is een discrete ongedaanmakingsstap binnen de warpsessie. **Ctrl+Z** verwijdert de laatste streek en herstelt de verplaatsingskaart naar de vorige staat. **Ctrl+Y** (of **Ctrl+Shift+Z**) past het opnieuw toe. U kunt de hele geschiedenis van een beroerte teruglopen voordat u een beslissing neemt.

Als u op **Escape** drukt of van gereedschap wisselt, worden alle niet-vastgelegde streken verwijderd en worden de lagen in hun oorspronkelijke staat hersteld. Er worden geen wijzigingen geschreven totdat u deze expliciet vastlegt.

## Toewijding

Klik op de knop **Commit** (of druk op **Enter**) om de verzamelde warp destructief toe te passen. Voor groepswarps activeert dit de volledige meerlaagse toepassing die hierboven is beschreven. De geschiedenis van het ongedaan maken van de vastgelegde warp is dan een enkel item in de stapel ongedaan maken van afbeeldingen, omkeerbaar met de standaard **Bewerken → Ongedaan maken**.