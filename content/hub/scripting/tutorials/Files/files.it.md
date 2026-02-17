---
title: "File"
type: docs
weight: 7
---
Lavorare con file e directory è essenziale per lo sviluppo di Scheme. Che tu stia salvando l'output, caricando risorse o organizzando la struttura del progetto, comprendere le operazioni sui file renderà i tuoi script più robusti e facili da usare.

Questa pagina copre le attività comuni di file e directory: lettura di percorsi, creazione di directory e raccolta di input di cartelle tramite parametri della GUI.

## Directory home dell'utente

Lumi è solo Linux, quindi la directory home dell'utente proviene dalla variabile di ambiente `HOME`.

Per ottenere la directory home dell'utente come stringa:

```scheme
(getenv "HOME")
```

Esempio di output:

```scheme
"/home/username"
```

## SEPARATORE DIR

Esiste anche la variabile globale `DIR-SEPARATOR`, che è il separatore del percorso specifico della piattaforma. In Lumi (Linux), è sempre `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Ottenere una posizione nella directory

Possiamo chiedere all'utente la posizione della directory nella finestra di dialogo Schema per un plug-in.

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

`SF-DIRNAME` fornisce un browser per una directory.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Qui convalidiamo i due input della directory (sorgente e destinazione) e torniamo ai valori predefiniti se i percorsi della GUI sono vuoti/non validi.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Se sei interessato ai dettagli sull'implementazione, cerca `validate-path-and-dir` nel sorgente del plug-in.

## Creare una directory

Lo schema fornisce il comando ```dir-make``` per creare una directory. Questo comando accetta un percorso separato da "/" e crea una singola directory con un parametro facoltativo per i privilegi. Non forniamo percorsi specifici della piattaforma.

Di solito è necessario creare più directory per un percorso pratico. Possiamo usare un wrapper per ```dir-make``` per aiutarci qui.

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

Nota: questa funzione utilizza anche ```file-exists?``` integrato per saltare le chiamate non necessarie. Restituisce #t se il file o la directory indicata esiste e #f se non esiste o se non è accessibile all'utente richiedente.

## Costruire un percorso

Dobbiamo anche scomporre e ricostruire percorsi in Scheme.

Per dividere un percorso in parti, utilizzare ```strbreakup```:

### Esempi di percorsi Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Nota: le barre iniziali e finali diventano elementi stringa vuoti nell'elenco risultante.

Per ricostruire un percorso, utilizzare ```string-append```:

### Creazione del percorso Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```