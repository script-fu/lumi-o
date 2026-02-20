---
title: "Procedurebrowser"
type: docs
---
De Procedure Browser is het belangrijkste referentiehulpmiddel voor het ontdekken van de honderden functies die beschikbaar zijn in Lumi's Procedurele Database (PDB). Omdat elk hulpmiddel, filter en script in Lumi in de PDB moet worden geregistreerd om opvraagbaar te zijn, is deze browser in feite een complete PDB-verkenner.

## De procedurebrowser openen

Ga naar **Help → Programmeren → Procedurebrowser**.

U kunt het ook openen vanuit de Schemaconsole via **Bladeren**.

## Wat het laat zien

De Procedurebrowser kan alle procedures weergeven die momenteel in het PDB zijn geregistreerd, ongeacht hun oorsprong. Het zoekt standaard naar "intern", om de intern geregistreerde kernprocedures weer te geven.

- **Interne procedures**: kernfuncties voor beeldmanipulatie, laagbeheer en gereedschapscontrole.
- **Externe plug-ins**: procedures die worden geboden door gecompileerde C/C++ plug-ins of permanente extensies.

## Zoeken en filteren

- **Zoekvak**: filtert procedures op naam, beschrijving of auteur. Als u het zoekveld leegmaakt, worden alle beschikbare procedures weergegeven.
- **Zoektype**: met de vervolgkeuzelijst voor zoeken kunt u filteren op specifieke velden. Als u dit instelt op **op type** en zoekt naar "intern", wordt de lijst kleiner en worden alleen de intern geregistreerde kernprocedures weergegeven.
- **Gedetailleerde weergave**: als u op een procedure klikt, worden de parameters, retourwaarden, auteur, datum en een beschrijving van wat de procedure doet weergegeven.

Dit is essentieel voor het vinden van de exacte naam en argumenthandtekening van een functie die u vanuit uw script wilt aanroepen.