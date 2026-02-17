---
title: "Dateien"
type: docs
weight: 7
---
Die Arbeit mit Dateien und Verzeichnissen ist für die Scheme-Entwicklung unerlässlich. Ganz gleich, ob Sie Ausgaben speichern, Ressourcen laden oder Ihre Projektstruktur organisieren: Wenn Sie Dateivorgänge verstehen, werden Ihre Skripts robuster und benutzerfreundlicher.

Auf dieser Seite werden allgemeine Datei- und Verzeichnisaufgaben behandelt: Pfade lesen, Verzeichnisse erstellen und Ordnereingaben über GUI-Parameter sammeln.

## Home-Verzeichnis des Benutzers

Lumi ist nur für Linux verfügbar, daher stammt das Home-Verzeichnis des Benutzers aus der Umgebungsvariablen `HOME`.

So erhalten Sie das Home-Verzeichnis des Benutzers als Zeichenfolge:

```scheme
(getenv "HOME")
```

Beispielausgabe:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Es gibt auch die globale Variable `DIR-SEPARATOR`, die das plattformspezifische Pfadtrennzeichen darstellt. In Lumi (Linux) ist es immer `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Einen Verzeichnisstandort abrufen

Wir können den Benutzer im Scheme-Dialog für ein Plug-in nach einem Verzeichnisspeicherort fragen.

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

Das `SF-DIRNAME` stellt einen Browser für ein Verzeichnis bereit.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Hier validieren wir die beiden Verzeichniseingaben (Quelle und Ziel) und greifen auf die Standardwerte zurück, wenn die GUI-Pfade leer/ungültig sind.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Wenn Sie an den Implementierungsdetails interessiert sind, durchsuchen Sie die Plug-in-Quelle nach `validate-path-and-dir`.

## Erstellen eines Verzeichnisses

Scheme stellt den Befehl ```dir-make``` zum Erstellen eines Verzeichnisses bereit. Dieser Befehl verwendet einen durch „/“ getrennten Pfad und erstellt ein einzelnes Verzeichnis mit einem optionalen Parameter für die Berechtigungen. Wir geben ihm keine plattformspezifischen Pfade.

Normalerweise müssen wir für einen praktischen Pfad mehrere Verzeichnisse erstellen. Hier können wir einen Wrapper für ```dir-make``` verwenden.

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

Hinweis: Diese Funktion verwendet auch das integrierte ```file-exists?```, um unnötige Aufrufe zu überspringen. Es gibt #t zurück, wenn die angegebene Datei oder das angegebene Verzeichnis existiert, und #f, wenn es nicht existiert oder für den anfordernden Benutzer nicht zugänglich ist.

## Einen Pfad konstruieren

Wir müssen auch Pfade in Scheme aufschlüsseln und neu aufbauen.

Um einen Pfad in Teile aufzuteilen, verwenden Sie ```strbreakup```:

### Beispiele für Linux-Pfade

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Hinweis: Führende und abschließende Schrägstriche werden in der resultierenden Liste zu leeren Zeichenfolgeelementen.

Um einen Pfad neu zu erstellen, verwenden Sie ```string-append```:

### Linux-Pfadaufbau

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
„