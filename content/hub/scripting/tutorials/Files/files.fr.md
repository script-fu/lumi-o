---
title: "Fichiers"
type: docs
weight: 7
---
Travailler avec des fichiers et des répertoires est essentiel pour le développement de Scheme. Que vous enregistriez la sortie, chargeiez des ressources ou organisiez la structure de votre projet, comprendre les opérations sur les fichiers rendra vos scripts plus robustes et plus conviviaux.

Cette page couvre les tâches courantes liées aux fichiers et aux répertoires : lecture des chemins, création de répertoires et collecte des entrées de dossier via les paramètres de l'interface graphique.

## Répertoire personnel de l'utilisateur

Lumi est uniquement Linux, donc le répertoire personnel de l'utilisateur provient de la variable d'environnement `HOME`.

Pour obtenir le répertoire personnel de l'utilisateur sous forme de chaîne :

```scheme
(getenv "HOME")
```

Exemple de sortie :

```scheme
"/home/username"
```

## DIR-SÉPARATEUR

Il existe également la variable globale `DIR-SEPARATOR`, qui est le séparateur de chemin spécifique à la plateforme. Sous Lumi (Linux), il s'agit toujours de `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Obtenir un emplacement de répertoire

Nous pouvons demander à l'utilisateur un emplacement de répertoire dans la boîte de dialogue Scheme pour un plug-in.

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

Le `SF-DIRNAME` fournit un navigateur vers un répertoire.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Ici, nous validons les deux entrées du répertoire (source et destination) et revenons aux valeurs par défaut si les chemins de l'interface graphique sont vides/invalides.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Si vous êtes intéressé par les détails de l'implémentation, recherchez dans la source du plug-in `validate-path-and-dir`.

## Créer un répertoire

Scheme fournit la commande ```dir-make``` pour créer un répertoire. Cette commande prend un chemin séparé par "/" et crée un seul répertoire avec un paramètre facultatif pour les privilèges. Nous ne lui donnons pas de chemins spécifiques à la plateforme.

Habituellement, nous devons créer plusieurs répertoires pour un chemin pratique. Nous pouvons utiliser un wrapper pour ```dir-make``` pour nous aider ici.

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

Remarque : Cette fonction utilise également le ```file-exists?``` intégré pour ignorer les appels inutiles. Il renvoie #t si le fichier ou le répertoire indiqué existe, et #f s'il n'existe pas ou s'il n'est pas accessible à l'utilisateur demandeur.

## Construire un chemin

Nous devons également décomposer et reconstruire les chemins dans Scheme.

Pour diviser un chemin en parties, utilisez ```strbreakup``` :

### Exemples de chemin Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Remarque : Les barres obliques de début et de fin deviennent des éléments de chaîne vides dans la liste résultante.

Pour reconstruire un chemin, utilisez ```string-append``` :

### Création de chemin Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```