---
title: "Le plugin de filtre"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
Nous avons utilisé un plug-in _procedure_ pour le didacticiel [Première étape](../../first-step/). Ces types de plug-ins fonctionnent sans avoir besoin d’une image ou d’un dessin en entrée. Habituellement, nous utilisons un plug-in pour modifier une image et ses dessins. Les plug-ins comme ceux-ci sont appelés plug-ins _filter_.

### Qu'est-ce qu'un Drawable ?

Un **dessinable** dans Lumi fait référence à un élément d'image sur lequel on peut dessiner, comme un calque ou un canal. Les plug-ins de filtre fonctionnent généralement sur ces éléments.

### Un exemple simple de plug-in de filtre

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Utiliser une instruction let pour définir une variable de message et le code principal
  (let ((message "hello, world"))
    ;; Afficher le message dans la console d'erreurs de Lumi
    (lumi-message message)
    ;; Inverser les couleurs du premier drawable sélectionné
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Enregistre le plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Nom de la procédure principale
  "Simple Filter Plug-in Demo"             ;; Le nom tel qu'il apparaît dans le menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Description de l'info-bulle
  "Author Name"                            ;; Accorde-toi un peu de mérite
  "License"                                ;; Licence
  "Date written"                           ;; Date d'écriture
  "*"                                      ;; Indique que ce plug-in nécessite une image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Nécessite un ou plusieurs drawables sélectionnés

;; Spécifier l'emplacement du menu pour le plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Copiez le texte et enregistrez-le sous `simple-filter-plug-in.scm` dans un dossier appelé `simple-filter-plug-in` dans l'un des dossiers de plug-ins Lumi. Un dossier de plug-ins Lumi est _n'importe quel_ dossier répertorié sous :
 **Lumi > Édition > Préférences > Dossiers > Plug-ins**

Sous Linux, cliquez avec le bouton droit sur le fichier `simple-filter-plug-in.scm`, accédez à **Propriétés > Autorisations** et cochez **Autoriser l'exécution du fichier en tant que programme**. Une fois le fichier au bon endroit, exécutable et exempt d'erreurs de syntaxe, au redémarrage de Lumi, il apparaîtra dans la barre d'en-tête du menu supérieur, à l'intérieur d'un menu appelé **Plug-in**.

### Exécution du plug-in

1. Ouvrez une image (ce plug-in de filtre nécessite une image pour fonctionner).
2. Ouvrez **Outils > Débogage > Console de messages** pour afficher un message.
3. Sélectionnez **Démo du plug-in de filtre simple** dans le menu **Plug-in**.
4. L'un des calques sélectionnés verra ses couleurs inversées et un message sera imprimé sur la console d'erreur.

### Modification du plug-in

Vous pouvez personnaliser le plug-in en éditant son fichier `.scm`. Par exemple, pour modifier le message affiché :

1. Ouvrez le fichier et localisez la ligne définissant `message`.
2. Remplacez `"hello, world"` par votre texte personnalisé.
3. Enregistrez le fichier.

Dans Lumi version 3, les plug-ins n'ont pas besoin d'être actualisés pour que les modifications enregistrées prennent effet. Réexécutez simplement le plug-in pour voir le message mis à jour.

### Examen du plug-in

#### Ligne Shebang

La première ligne garantit que le script fonctionne comme un plug-in dans Lumi 3 :

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Définition de la procédure

La procédure accepte deux arguments : l'image active et les drawables sélectionnés.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Logique de base

Une instruction `let` définit une variable et effectue des opérations sur le dessinable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Affiche un message dans la console d'erreurs de Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverse les couleurs du premier drawable sélectionné
```

### Enregistrement du plug-in

Le plug-in est enregistré auprès de Lumi en tant que plug-in de filtre :

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Enregistre la procédure principale
  "Simple Filter Plug-in Demo"             ;; Le nom tel qu'il apparaît dans le menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Description de l'info-bulle
  "Author Name"                            ;; Nom de l'auteur
  "License"                                ;; Type de licence
  "Date written"                           ;; Date d'écriture
  "*"                                      ;; Indique que le plug-in nécessite une image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Nécessite un ou plusieurs drawables sélectionnés
```

#### Menu Inscription

Cette ligne spécifie l'emplacement du menu du plug-in :

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Dépannage

Si un plug-in n'apparaît pas, vérifiez son emplacement, son nom et sa propriété exécutable.

L'emplacement doit se trouver dans un chemin de recherche de plug-in.
Le nom du fichier doit correspondre au nom du dossier contenant.
Le fichier doit être défini comme exécutable.


La **Console de messages** est un outil précieux pour dépanner les plug-ins personnalisés. Si votre plug-in ne se comporte pas comme prévu, recherchez ici les messages d'erreur ou les journaux. La fenêtre **Terminal** peut également fournir des informations de débogage et signaler les problèmes de chargement.