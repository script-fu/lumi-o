---
title: "Procedurbläddraren"
type: docs
weight: 1
---
**Lumi Procedure Browser** låter dig söka efter tillgängliga procedurer (inbyggd och plug-in medföljer) och inspektera deras parametrar och returvärden.

### Var hittar du Lumi Procedure Browser

Du kan komma åt procedurbläddraren i Lumi via menyn **Hjälp**:

- **Hjälp** -> **Procedurwebbläsare**

### Vad procedurwebbläsaren gör

Procedurbläddraren listar alla Lumis interna procedurer, tillsammans med de som lagts till av plugin-program, inklusive den du just har installerat. Varje procedurpost ger användbar information, inklusive:

- Procedurnamnet.
– En beskrivning av vad den gör.
- De parametrar som den accepterar (ingångsvärden).
- Returvärdena (output).

Sök efter nyckelord eller procedurnamn när du behöver verifiera en anropssignatur eller bekräfta det exakta procedurnamnet.

#### (lumi-meddelande) i procedurbläddraren

Sök efter `lumi-message` för att se dess parametrar och returvärden.

### Hitta din plugin

När du har installerat "Hello World!" plug-in, kan du hitta den listad i procedurbläddraren. Sök helt enkelt efter funktionsnamnet som du registrerade hos Lumi, i det här fallet "schema-hello-world". Posten kommer att visa parametrarna och eventuella returvärden som är associerade med plugin-programmet, tillsammans med en kort beskrivning. Du kommer också att se var några av textraderna du angav som indataparametrar under registreringsprocessen visas under avsnittet **Ytterligare information**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Detta gör det enkelt att verifiera att din plugin är korrekt registrerad och ger dig ett snabbt sätt att granska hur det interagerar med andra procedurer i Lumi. Procedurbläddraren är ett kraftfullt verktyg för att felsöka och utöka dina plugin-program genom att utforska alla tillgängliga procedurer inom Lumi.