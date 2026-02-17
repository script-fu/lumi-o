---
title: "Filer"
type: docs
weight: 7
---
Att arbeta med filer och kataloger är viktigt för utvecklingen av schemat. Oavsett om du sparar utdata, laddar resurser eller organiserar din projektstruktur, kommer förståelse av filoperationer att göra dina skript mer robusta och användarvänliga.

Den här sidan täcker vanliga fil- och kataloguppgifter: läsa sökvägar, skapa kataloger och samla in mappar via GUI-parametrar.

## Användarens hemkatalog

Lumi är endast Linux, så användarens hemkatalog kommer från miljövariabeln `HOME`.

Så här hämtar du användarens hemkatalog som en sträng:

```scheme
(getenv "HOME")
```

Exempel på utdata:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Det finns också den globala variabeln `DIR-SEPARATOR`, som är den plattformsspecifika sökvägsseparatorn. I Lumi (Linux) är det alltid `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Få en katalogplats

Vi kan be användaren om en katalogplats i dialogrutan Schema för ett plugin-program.

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

`SF-DIRNAME` tillhandahåller en webbläsare till en katalog.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Här validerar vi de två katalogingångarna (källa och destination) och faller tillbaka till standardinställningarna om GUI-sökvägarna är tomma/ogiltiga.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Om du är intresserad av implementeringsdetaljerna, sök i plugin-källan efter `validate-path-and-dir`.

## Skapa en katalog

Schema tillhandahåller kommandot ```dir-make``` för att skapa en katalog. Detta kommando tar en "/"-separerad sökväg och skapar en enda katalog med en valfri parameter för privilegierna. Vi ger det inte plattformsspecifika vägar.

Vanligtvis behöver vi skapa flera kataloger för en praktisk väg. Vi kan använda ett omslag för ```dir-make``` för att hjälpa oss här.

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

Obs: Den här funktionen använder också den inbyggda ```file-exists?``` för att hoppa över onödiga samtal. Den returnerar #t om den angivna filen eller katalogen finns, och #f om den inte finns eller om den inte är tillgänglig för den begärande användaren.

## Konstruera en väg

Vi måste också bryta ner och bygga om vägar i Scheme.

För att dela upp en sökväg i delar, använd ```strbreakup```:

### Linux-sökvägsexempel

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Obs! Inledande och efterföljande snedstreck blir tomma strängelement i den resulterande listan.

För att bygga om en sökväg, använd ```string-append```:

### Linux Path Building

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```