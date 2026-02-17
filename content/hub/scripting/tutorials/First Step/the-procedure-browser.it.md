---
title: "Il browser delle procedure"
type: docs
weight: 1
---
Il **Lumi Procedure Browser** consente di cercare le procedure disponibili (integrate e plug-in fornite) e di controllarne i parametri e i valori restituiti.

### Dove trovare il browser delle procedure Lumi

È possibile accedere al browser delle procedure in Lumi tramite il menu **Aiuto**:

- **Aiuto** -> **Browser delle procedure**

### Cosa fa il browser delle procedure

Il Browser delle procedure elenca tutte le procedure interne di Lumi, insieme a quelle aggiunte dai plug-in, inclusa quella appena installata. Ogni voce della procedura fornisce informazioni utili, tra cui:

- Il nome della procedura.
- Una descrizione di ciò che fa.
- I parametri accettati (valori di input).
- I valori restituiti (output).

Cerca per parola chiave o nome della procedura quando è necessario verificare la firma di una chiamata o confermare il nome esatto della procedura.

#### (messaggio lumi) nel browser delle procedure

Cerca `lumi-message` per visualizzarne i parametri e i valori restituiti.

### Trovare il plug-in

Dopo aver installato "Hello World!" plug-in, è possibile trovarlo elencato nel browser delle procedure. Cerca semplicemente il nome della funzione che hai registrato con Lumi, in questo caso "scheme-hello-world". La voce visualizzerà i parametri e gli eventuali valori restituiti associati al plug-in, insieme a una breve descrizione. Vedrai anche dove vengono visualizzate alcune delle righe di testo che hai inserito come parametri di input durante il processo di registrazione nella sezione **Informazioni aggiuntive**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Ciò semplifica la verifica che il plug-in sia registrato correttamente e fornisce un modo rapido per verificare come interagisce con le altre procedure in Lumi. Il Procedure Browser è un potente strumento per eseguire il debug ed espandere i plug-in esplorando tutte le procedure disponibili all'interno di Lumi.