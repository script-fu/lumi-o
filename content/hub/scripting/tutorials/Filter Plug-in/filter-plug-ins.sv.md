---
title: "Filterplugin"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
Vi använde ett plugin-program _procedure_ för handledningen [Första steget](../../first-step/). Dessa typer av plugin-program fungerar utan att behöva en bild eller ritbar som indata. Vanligtvis använder vi en plug-in för att ändra en bild och dess dragbara bilder. Plug-ins som dessa kallas _filter_ plug-ins.

### Vad är en Drawable?

En **ritbar** i Lumi hänvisar till ett bildelement som kan ritas på, till exempel ett lager eller en kanal. Filterplugin-program fungerar vanligtvis på dessa element.

### Ett enkelt filterplugin-exempel

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Använd ett let-uttryck för att definiera en meddelandevariabel och kärnkod
  (let ((message "hello, world"))
    ;; Visa meddelandet i Lumis felkonsol
    (lumi-message message)
    ;; Invertera färgerna på det första valda drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Registrerar insticksprogrammet
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Namn på huvudproceduren
  "Simple Filter Plug-in Demo"             ;; Namnet som det visas i Lumi-menyn
  "Tests a basic Scheme filter plug-in"    ;; Verktygstipsbeskrivning
  "Author Name"                            ;; Ge dig själv lite beröm
  "License"                                ;; Licens
  "Date written"                           ;; Skrivdatum
  "*"                                      ;; Anger att detta plug-in kräver en bild
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Kräver ett eller flera valda drawables

;; Ange menyplatsen för plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Kopiera texten och spara den som `simple-filter-plug-in.scm` i en mapp som heter `simple-filter-plug-in` i en av Lumis plugin-mappar. En Lumi plug-ins mapp är _any_ mapp listad under:
 **Lumi > Redigera > Inställningar > Mappar > Plugin-program**

I Linux, högerklicka på filen `simple-filter-plug-in.scm`, gå till **Egenskaper > Behörigheter** och markera **Tillåt exekvering av fil som program**. När filen väl är på rätt plats, körbar och fri från syntaxfel, när Lumi startas om, kommer den att visas i den översta menyraden, inuti en meny som heter **Plug-in**.

### Kör plugin-programmet

1. Öppna en bild (det här filterpluginprogrammet kräver en bild för att fungera).
2. Öppna **Verktyg > Felsökning > Meddelandekonsol** för att se ett meddelande.
3. Välj **Simple Filter Plug-in Demo** från **Plug-in**-menyn.
4. Ett av de valda lagren kommer att ha sina färger inverterade och ett meddelande kommer att skrivas ut till felkonsolen.

### Redigera plugin-programmet

Du kan anpassa plugin-programmet genom att redigera dess `.scm`-fil. Till exempel, för att ändra meddelandet som visas:

1. Öppna filen och leta reda på raden som definierar `message`.
2. Ersätt `"hello, world"` med din anpassade text.
3. Spara filen.

I Lumi version 3 behöver plugin-program inte uppdateras för att sparade ändringar ska träda i kraft. Kör bara plugin-programmet igen för att se det uppdaterade meddelandet.

### Plug-in undersökning

#### Shebang-rad

Den första raden säkerställer att skriptet fungerar som en plug-in i Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
  (lumi-message message) ;; Visar ett meddelande i Lumis felkonsol
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverterar färgerna på det första valda drawable
```

### Plugin-registrering

Plug-in är registrerad hos Lumi som en filterplugin:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Registrerar huvudproceduren
  "Simple Filter Plug-in Demo"             ;; Namnet som det visas i Lumi-menyn
  "Tests a basic Scheme filter plug-in"    ;; Verktygstipsbeskrivning
  "Author Name"                            ;; Författarens namn
  "License"                                ;; Licenstyp
  "Date written"                           ;; Skrivdatum
  "*"                                      ;; Anger att plug-in kräver en bild
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Kräver ett eller flera valda drawables
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


**Meddelandekonsolen** är ett värdefullt verktyg för att felsöka anpassade plugin-program. Om din plug-in inte fungerar som förväntat, kolla här för felmeddelanden eller loggar. Fönstret **Terminal** kan också tillhandahålla felsökningsinformation och rapportera laddningsproblem.