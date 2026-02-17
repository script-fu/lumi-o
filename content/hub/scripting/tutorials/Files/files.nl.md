---
title: "Bestanden"
type: docs
weight: 7
---
Het werken met bestanden en mappen is essentieel voor de ontwikkeling van schema's. Of u nu uitvoer opslaat, bronnen laadt of uw projectstructuur organiseert, als u bestandsbewerkingen begrijpt, worden uw scripts robuuster en gebruiksvriendelijker.

Deze pagina behandelt algemene bestands- en maptaken: paden lezen, mappen maken en mapinvoer verzamelen via GUI-parameters.

## Thuismap van gebruiker

Lumi is alleen geschikt voor Linux, dus de thuismap van de gebruiker is afkomstig van de omgevingsvariabele `HOME`.

Om de homedirectory van de gebruiker als een string op te halen:

```scheme
(getenv "HOME")
```

Voorbeelduitvoer:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Er is ook de globale variabele `DIR-SEPARATOR`, die het platformspecifieke padscheidingsteken is. In Lumi (Linux) is dit altijd `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Een directorylocatie verkrijgen

We kunnen de gebruiker om een maplocatie vragen in het Schema-dialoogvenster voor een plug-in.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

De `SF-DIRNAME` biedt een browser voor een directory.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Hier valideren we de twee mapinvoer (bron en bestemming) en vallen we terug op de standaardwaarden als de GUI-paden leeg/ongeldig zijn.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Als u geïnteresseerd bent in de implementatiedetails, zoekt u in de plug-inbron naar `validate-path-and-dir`.

## Een directory maken

Schema biedt de opdracht ```dir-make``` om een map te maken. Deze opdracht gebruikt een "/" gescheiden pad en maakt een enkele map aan met een optionele parameter voor de bevoegdheden. We geven het geen platformspecifieke paden.

Meestal moeten we meerdere mappen maken voor een praktisch pad. We kunnen een verpakking voor ```dir-make``` gebruiken om ons hierbij te helpen.

```scheme
;; Purpose: A wrapper for (dir-make) that creates a given path from a platform
;;          supplied path. Always emits Linux style separators for dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Root dir
    ;; Create the rest of the directories step-by-step
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; build the path
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Opmerking: deze functie gebruikt ook de ingebouwde ```file-exists?``` om onnodige oproepen over te slaan. Het retourneert #t als het aangegeven bestand of de aangegeven map bestaat, en #f als het niet bestaat of niet toegankelijk is voor de aanvragende gebruiker.

## Een pad aanleggen

We moeten ook paden in Scheme afbreken en opnieuw opbouwen.

Gebruik ```strbreakup``` om een pad in delen te splitsen:

### Voorbeelden van Linux-paden

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Opmerking: voor- en achterschuine strepen worden lege tekenreekselementen in de resulterende lijst.

Gebruik ```string-append``` om een pad opnieuw op te bouwen:

### Linux-pad bouwen

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```