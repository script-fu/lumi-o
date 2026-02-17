---
title: "Filterplugin"
type: docs
weight: 2
---
Vi använde en _procedure_ plug-in för [First Step](../../first-step/) handledning. Dessa typer av plugin-program fungerar utan att behöva en bild eller ritbar som indata. Vanligtvis använder vi en plug-in för att ändra en bild och dess dragbara bilder. Plug-ins som dessa kallas _filter_ plug-ins.

### Vad är en Drawable?

En **ritbar** i Lumi hänvisar till ett bildelement som kan ritas på, till exempel ett lager eller en kanal. Filterplugin-program fungerar vanligtvis på dessa element.

### Ett enkelt filterplugin-exempel

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Kopiera texten och spara den som `simple-filter-plug-in.scm` i en mapp som heter `simple-filter-plug-in` i en av Lumis plugin-mappar. En Lumi plug-ins mapp är _any_ mapp listad under:
 **Lumi > Redigera > Inställningar > Mappar > Plugin-program**

I Linux, högerklicka på filen `simple-filter-plug-in.scm`, gå till **Egenskaper > Behörigheter** och markera **Tillåt exekvering av fil som program**. När filen väl är på rätt plats, körbar och fri från syntaxfel, när Lumi startas om, kommer den att visas i den översta menyraden, inuti en meny som heter **Plug-in**.

### Kör plugin-programmet

1. Öppna en bild (det här filterpluginprogrammet kräver en bild för att fungera).
2. Öppna **Windows > Dockningsbara dialogrutor > Error Console** för att se ett meddelande.
3. Välj **Simple Filter Plug-in Demo** från **Plug-in**-menyn.
4. Ett av de valda lagren kommer att ha sina färger inverterade och ett meddelande kommer att skrivas ut till felkonsolen.

### Redigera plugin-programmet

Du kan anpassa plugin-programmet genom att redigera dess `.scm`-fil. Till exempel, för att ändra meddelandet som visas:

1. Öppna filen och leta reda på raden som definierar `message`.
2. Ersätt `"hello, world"` med din anpassade text.
3. Spara filen.

I Lumi version 3 behöver plugin-program inte uppdateras för att sparade ändringar ska träda i kraft. Kör bara plugin-programmet igen för att se det uppdaterade meddelandet.

### Plug-in undersökning

#### Shebang Line

Den första raden säkerställer att skriptet fungerar som en plug-in i Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Definition av procedur

Proceduren accepterar två argument: den aktiva bilden och de valda ritningarna.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Kärnlogik

En `let`-sats definierar en variabel och utför operationer på den ritbara.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Plugin-registrering

Plug-in är registrerad hos Lumi som en filterplugin:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Menyregistrering
Den här raden anger menyplatsen för plugin-programmet:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Felsökning

Om ett plugin-program inte visas kontrollerar du dess plats, namn och körbara egenskap.

Platsen måste vara i en plugin-sökväg.
Filnamnet måste matcha namnet på mappen som innehåller.
Filen måste ställas in som körbar.


**Felkonsolen** är ett värdefullt verktyg för att felsöka anpassade plugin-program. Om din plug-in inte fungerar som förväntat, kolla här för felmeddelanden eller loggar. Fönstret **Terminal** kan också tillhandahålla felsökningsinformation och rapportera laddningsproblem.