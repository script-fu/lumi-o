---
title: "De filterplug-in"
type: docs
weight: 2
---
We hebben een _procedure_ plug-in gebruikt voor de [First Step](../../first-step/) tutorial. Dit soort plug-ins werken zonder dat er een afbeelding of tekenbaar bestand als invoer nodig is. Meestal gebruiken we een plug-in om een ​​afbeelding en de tekenbare bestanden te wijzigen. Dergelijke plug-ins worden _filter_plug-ins genoemd.

### Wat is een tekenbaar bestand?

Een **tekenbaar** in Lumi verwijst naar een afbeeldingselement waarop kan worden getekend, zoals een laag of kanaal. Filterplug-ins werken doorgaans op deze elementen.

### Een eenvoudig voorbeeld van een filterplug-in

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

Kopieer de tekst en sla deze op als `simple-filter-plug-in.scm` in een map met de naam `simple-filter-plug-in` in een van de plug-insmappen van Lumi. Een map met Lumi-plug-ins is de map _any_ die wordt vermeld onder:
 **Lumi > Bewerken > Voorkeuren > Mappen > Plug-ins**

Klik in Linux met de rechtermuisknop op het bestand `simple-filter-plug-in.scm`, ga naar **Eigenschappen > Machtigingen** en vink **Uitvoeren van bestand als programma toestaan** aan. Zodra het bestand op de juiste plaats staat, uitvoerbaar is en vrij is van syntaxisfouten, zal het, wanneer Lumi opnieuw wordt opgestart, verschijnen in de bovenste menubalk, in een menu met de naam **Plug-in**.

### De plug-in uitvoeren

1. Open een afbeelding (deze filterplug-in vereist een afbeelding om te werken).
2. Open **Windows > Dockbare dialoogvensters > Foutconsole** om een ​​bericht te zien.
3. Selecteer **Demo van eenvoudige filterplug-in** in het menu **Plug-in**.
4. De kleuren van een van de geselecteerde lagen zijn omgekeerd en er wordt een bericht afgedrukt op de foutconsole.

### De plug-in bewerken

U kunt de plug-in aanpassen door het bestand `.scm` te bewerken. Om bijvoorbeeld het weergegeven bericht te wijzigen:

1. Open het bestand en zoek de regel die `message` definieert.
2. Vervang `"hello, world"` door uw aangepaste tekst.
3. Sla het bestand op.

In Lumi versie 3 hoeven plug-ins niet te worden vernieuwd om opgeslagen wijzigingen door te voeren. Voer de plug-in eenvoudigweg opnieuw uit om het bijgewerkte bericht te zien.

### Plug-inonderzoek

#### Shebang-lijn

De eerste regel zorgt ervoor dat het script als plug-in in Lumi 3 werkt:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Proceduredefinitie

De procedure accepteert twee argumenten: de actieve afbeelding en de geselecteerde tekenbare bestanden.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Kernlogica

Een `let` instructie definieert een variabele en voert bewerkingen uit op het tekenbare bestand.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Plug-inregistratie

De plug-in wordt bij Lumi geregistreerd als filterplug-in:

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

#### Menuregistratie
Deze regel specificeert de menulocatie voor de plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Problemen oplossen

Als een plug-in niet verschijnt, controleer dan de locatie, naam en uitvoerbare eigenschap ervan.

De locatie moet zich in een zoekpad van de plug-in bevinden.
De bestandsnaam moet overeenkomen met de naam van de map die deze bevat.
Het bestand moet zijn ingesteld als uitvoerbaar bestand.


De **Error Console** is een waardevol hulpmiddel voor het oplossen van problemen met aangepaste plug-ins. Als uw plug-in zich niet gedraagt ​​zoals verwacht, kijk dan hier voor foutmeldingen of logbestanden. Het **Terminal**-venster kan ook foutopsporingsinformatie bieden en laadproblemen melden.