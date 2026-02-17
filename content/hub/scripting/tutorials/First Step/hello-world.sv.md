---
title: "Hej världen!"
type: docs
weight: 1
---
Denna handledning går igenom den minimala strukturen för ett Scheme-pluginprogram. Vissa rader är "boilerplate": de krävs för att Lumi ska ladda filen, även om du inte helt förstår dem ännu.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

På hög nivå kommer du att:

1. Definiera en funktion
2. Registrera den så att den visas i procedurdatabasen
3. (Valfritt) Lägg till en menypost
4. Installera filen i en plugin-mapp

### Definiera en funktion

En funktion, även känd som en _procedur_, är en kodbit med ett namn och syfte, den tar en ingång och producerar utdata.

**Input** > **_Function_** > **Utdata**

### Registrera funktionen

Registrering är handlingen att sätta funktionsnamnet på en lista så att Lumi vet om det.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Länk till menyn

Detta talar om för Lumi var du kan hitta din funktion i dess menysystem.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Detta visar menyn "Funky" i huvudmenyraden. Ändra sökvägen för att placera plugin-programmet någon annanstans. Sökvägen `<Image>/Funky` betyder att plugin-programmet kommer att visas under menykategorin **Bild**. Du kan ändra `<Image>` till `<Tools>`, `<Filters>`, etc., beroende på var du vill att plugin-programmet ska visas.

### Kommentarer

I Scheme, Schemes basspråk, görs kommentarer i allmänhet genom att föregå en användbar textrad med `;;`. Din användning av kommentarer beror på ditt flyt som kodare – om du kodar ibland hjälper fler kommentarer. Om du kodar hela tiden är koden lika lätt att läsa som kommentaren skulle vara. Vid funktionell programmering tenderar koden också att bli tillräckligt beskrivande för att kunna läsas som ett skript.

### Syntax

Kod tenderar att ha små regler för hur man placerar objekt på en rad, så att vi enkelt kan läsa raden. Till exempel kan en mening ha ett mellanslag efter ett kommatecken eller punkt. Det hjälper läsbarheten.

Kod kan ordna saker på liknande sätt, vilket kan se konstigt ut till en början:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Exempelkod

Här är det fullständiga exemplet. De flesta Lumi-procedurer har prefixet `lumi-`. Till exempel, `lumi-message` skriver ut en sträng till den konfigurerade meddelandehanteraren.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Installera plugin-programmet

1. Gå till **Lumi -> Redigera -> Inställningar -> Mappar -> Plug-ins**.
2. Lägg till din [repo](/hub/scripting/tools/git) plugin-mapp till listan.
3. Skapa en mapp för plugin-programmet och spara exempelkoden ovan som `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Högerklicka på filen `hello-world.scm`.
5. Gå till **Egenskaper -> Behörigheter -> Tillåt exekvering av fil som program**.
6. Starta om Lumi.

### Prova plugin-programmet

Plugin-programmet bör nu visas under menyn "Funky" i huvudfönstret i Lumi. Klicka på den och den ska visa "Hej världen!" meddelande. Försök att ändra koden, som att ändra meddelandetexten, och spara filen. När du kör plugin-programmet igen kommer dina ändringar att återspeglas utan att starta om Lumi.

Prova att experimentera genom att ändra menysökvägen. Till exempel kommer `"<Image>/File"` att placera den i Arkiv-menyn, och `"<Image>/File/Funky"` kommer att skapa en ny sektion i Arkiv-menyn. Det här är ett utmärkt sätt att anpassa var ditt plugin-program visas och att organisera dina verktyg.