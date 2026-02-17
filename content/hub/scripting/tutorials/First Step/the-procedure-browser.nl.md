---
title: "De procedurebrowser"
type: docs
weight: 1
---
Met de **Lumi Procedure Browser** kunt u de beschikbare procedures doorzoeken (ingebouwd en plug-in meegeleverd) en hun parameters inspecteren en waarden retourneren.

### Waar u de Lumi Procedure-browser kunt vinden

U kunt de Procedurebrowser in Lumi openen via het **Help**-menu:

- **Help** -> **Procedurebrowser**

### Wat de procedurebrowser doet

De Procedurebrowser geeft een overzicht van alle interne procedures van Lumi, samen met de procedures die zijn toegevoegd door plug-ins, inclusief degene die u zojuist hebt geïnstalleerd. Elke procedure-invoer biedt nuttige informatie, waaronder:

- De procedurenaam.
- Een beschrijving van wat het doet.
- De parameters die het accepteert (invoerwaarden).
- De retourwaarden (uitvoer).

Zoek op trefwoord of procedurenaam wanneer u een oproephandtekening moet verifiëren of de exacte procedurenaam moet bevestigen.

#### (lumi-bericht) in de procedurebrowser

Zoek naar `lumi-message` om de parameters ervan te bekijken en waarden terug te geven.

### Uw plug-in vinden

Zodra je de "Hello World!" plug-in, u kunt deze vinden in de Procedurebrowser. Zoek eenvoudigweg naar de functienaam die u bij Lumi heeft geregistreerd, in dit geval "scheme-hello-world". De invoer toont de parameters en eventuele retourwaarden die aan de plug-in zijn gekoppeld, samen met een korte beschrijving. U zult ook zien waar enkele van de tekstregels die u tijdens het registratieproces als invoerparameters heeft ingevoerd, worden weergegeven onder de sectie **Aanvullende informatie**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Hierdoor kunt u eenvoudig controleren of uw plug-in correct is geregistreerd en kunt u snel zien hoe deze samenwerkt met andere procedures in Lumi. De Procedure Browser is een krachtig hulpmiddel voor het debuggen en uitbreiden van uw plug-ins door alle beschikbare procedures binnen Lumi te verkennen.