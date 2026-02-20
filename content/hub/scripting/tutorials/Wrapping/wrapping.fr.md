---
title: "Emballage"
type: docs
weight: 4
---
Les commandes Scheme fonctionnent à un niveau bas, ce qui signifie que même des tâches simples peuvent nécessiter plusieurs étapes. Cependant, cette granularité offre de la flexibilité : nous pouvons regrouper les commandes en petites fonctions réutilisables qui font exactement ce dont nous avons besoin. L’emballage n’est pas un concept en noir et blanc ; cela peut aller de simples alias pour les commandes fréquemment utilisées à des fonctions plus complexes qui gèrent des flux de travail entiers. Parfois, un wrapper n’est qu’une fonction pratique destinée à améliorer la lisibilité, tandis que dans d’autres cas, il évolue vers un utilitaire complet qui encapsule plusieurs opérations.

### Pourquoi envelopper des fonctions ?

Les fonctions d'encapsulage présentent plusieurs avantages clés :

- **Simplifie les tâches répétitives** – Au lieu de répéter des commandes de bas niveau, enveloppez-les dans une fonction d'assistance et réutilisez-la.
- **Améliore la lisibilité** – Donner à nos fonctions enveloppées des noms clairs et descriptifs rend notre code plus facile à comprendre en un coup d'œil.
- **Encapsule la complexité** – Plutôt que de traiter de longues listes énigmatiques de commandes, de boucles profondément imbriquées ou d'instructions de message complexes, nous pouvons les décomposer en fonctions d'assistance plus petites et bien structurées.
- **Améliore la maintenabilité** – Si la fonctionnalité principale d'une commande change, nous n'avons besoin de mettre à jour notre fonction enveloppée qu'une seule fois, isolant ainsi nos plug-ins des détails de ces modifications.
- **Encourage la réutilisation du code** – Chaque assistant devient une partie de votre bibliothèque, ce qui accélère l'écriture et le débogage des futurs scripts.

À mesure que vos plug-ins se développent, les wrappers vous aident à maintenir la logique de base lisible et à isoler les détails répétitifs.

Un autre avantage de l'encapsulage des fonctions est de les intégrer dans un surligneur de syntaxe comme Visual Studio Code. Cela améliore la lisibilité et la navigation, rendant les scripts plus clairs. Dans un plug-in utilisant des fonctions personnalisées, toute fonction surlignée en vert confirme qu'elle est correctement référencée depuis notre bibliothèque.

Si vous gérez votre propre bibliothèque d'assistance, envisagez d'ajouter les noms de fonctions de votre projet à la coloration syntaxique de votre éditeur. Cela rend la navigation et la refactorisation plus rapides.

Exemples :

### Graine aléatoire

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Bien que nous puissions utiliser ***msrg-rand*** directement dans notre code, l'envelopper dans une fonction appelée ***random-seed*** améliore la lisibilité. En donnant à la fonction un nom clair et descriptif, il devient plus facile de comprendre son objectif en un coup d'œil.

De plus, définir ***random-seed*** comme fonction autonome nous permet de l'utiliser n'importe où dans nos plug-ins tout en centralisant l'implémentation en un seul endroit. Si jamais nous devons modifier la façon dont la graine est générée, il nous suffit de mettre à jour cette fonction, en laissant le reste de notre code intact.

Par exemple, si nous décidons de passer à ***aléatoire*** à la place :

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

Le nom de la fonction reste le même, garantissant que nos scripts continuent de fonctionner sans modification. Cette approche maintient notre code flexible, maintenable et facile à lire.

### Exportation JPEG

La fonction d'exportation JPEG de Scheme est livrée avec de nombreux paramètres, offrant un contrôle précis sur la façon dont les images sont enregistrées. Cependant, dans la plupart des cas, nous ne nous soucions que de quelques paramètres clés, tels que le nom et la qualité du fichier. Pour simplifier le processus, nous pouvons envelopper la fonction.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

Dans cette fonction wrapper, la plupart des options d'exportation sont codées en dur, exposant uniquement les paramètres que nous sommes susceptibles d'ajuster : le nom et la qualité du fichier. Cette approche améliore la lisibilité et simplifie la sauvegarde des images.De plus, si l'exportateur de Lumi change à l'avenir, nous n'aurons besoin que de mettre à jour cette fonction plutôt que de modifier chaque script qui exporte un JPEG.

### Utilisation du wrapper

Pour exporter un JPEG dans nos plug-ins, nous incluons simplement la bibliothèque et appelons notre fonction personnalisée :

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Cela maintient notre code propre, lisible et adaptable tout en nous permettant d'exporter des fichiers JPEG efficacement avec un minimum d'effort.

### Remplacement de voiture

La fonction ***car*** peut être énigmatique et sujette à des erreurs de script. Il est facile d'appliquer par erreur ***car*** à un vecteur ou à un élément hors liste, ce qui entraîne un comportement inattendu. Pour rendre notre code plus robuste et lisible, nous pouvons envelopper cette fonctionnalité dans une fonction plus sûre.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Cette fonction récupère en toute sécurité le premier élément d'une liste ou d'un vecteur tout en fournissant des avertissements utiles lorsque des entrées invalides ou vides sont rencontrées. En utilisant ***first-item*** au lieu de ***car***, nous réduisons le risque d'erreurs accidentelles et améliorons la clarté de nos scripts.

#### Pourquoi utiliser ce wrapper ?

- **Empêche les plantages de script** – Évite les erreurs causées par l'application de ***car*** à des non-listes.
- **Prend en charge à la fois les listes et les vecteurs** – Étend la convivialité au-delà des simples listes.
- **Fournit des avertissements significatifs** – Aide à déboguer les problèmes d'entrée inattendus.
- **Améliore la lisibilité** – Le nom de la fonction exprime clairement son objectif.

En encapsulant cette logique dans le premier élément, nous rendons nos plug-ins plus robustes et plus faciles à maintenir. Bien sûr, cela dépend de vos préférences personnelles, vous pouvez être tout à fait à l'aise avec l'utilisation directe des fonctions car, caar, cadr et autres fonctions Scheme similaires.

### Encapsulation d'une fonction encapsulée

Encapsuler une fonction déjà encapsulée peut améliorer encore la lisibilité et la maintenabilité. Par exemple, lorsque nous travaillons avec des paires de coordonnées telles que ***coords-pixels (liste 100 200)***, nous pourrions utiliser :

```scheme
(first-item pixel-coords)
```

pour récupérer la coordonnée ***x***. Cependant, bien que fonctionnel, celui-ci n’est pas très expressif. Au lieu de cela, nous pouvons envelopper ***first-item*** dans une définition plus appropriée pour rendre notre intention plus claire.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Pourquoi utiliser cette approche ?

- **Améliore la clarté du code** – Au lieu d'utiliser des fonctions génériques d'accès aux listes, nous définissons explicitement des fonctions qui décrivent leur objectif.
- **Améliore la maintenabilité** – Si notre représentation de coordonnées change (par exemple, en utilisant des vecteurs au lieu de listes), il nous suffit de mettre à jour ces petites fonctions.
- **Encourage la cohérence** – L'utilisation de ***x-coord*** et ***y-coord*** rend le script plus facile à lire et à comprendre en un coup d'œil.

Maintenant, au lieu d'écrire dans un schéma générique :

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Nous pouvons écrire dans _notre_ Scheme :

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

En englobant les fonctions de bas niveau dans des noms significatifs, nous créons une manière plus intuitive de travailler avec les données, réduisant ainsi la confusion et les erreurs potentielles.

### Wrappers expédiés : l'utilitaire Stdlib

Lumi fournit un ensemble de wrappers prêts à l'emploi chargés automatiquement au démarrage, ils sont donc disponibles dans n'importe quel plug-in ou dans la console Scheme sans aucun appel `(load ...)`. Ces bibliothèques — `common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` et `paths.scm` — sont construites exactement sur le même principe que les exemples ci-dessus : elles donnent des noms clairs aux opérations de bas niveau, masquez le passe-partout répétitif et fournissez un endroit unique pour mettre à jour si la commande sous-jacente change.Par exemple, `images.scm` fournit `image-get-open-list` comme wrapper lisible autour de l'appel PDB brut, et `files.scm` expose des assistants de création de chemin qui nécessiteraient autrement des chaînes `string-append` répétées.

Vous pouvez parcourir chaque nom exporté, lire sa docstring et voir de quelle bibliothèque il provient dans **[Utility Browser](@@LUMI_TOKEN_21@@)** (Aide → Programmation → Navigateur d'utilitaires). Il s'agit d'une démonstration pratique de l'encapsulation à grande échelle et d'une source utile de modèles à emprunter lors de la création de votre propre bibliothèque d'assistance.

### Conclusion

Les fonctions d'encapsulation sont un moyen puissant de simplifier le développement de schémas, en rendant les scripts plus lisibles, plus faciles à maintenir et plus robustes. En encapsulant la complexité et en exposant uniquement les détails nécessaires, nous créons une approche plus structurée de l'écriture de plug-ins.

Points clés à retenir de cette approche :

- **Simplifie les tâches répétitives** – Au lieu de répéter manuellement les commandes de bas niveau, nous créons des fonctions réutilisables.
- **Améliore la lisibilité du code** – Des wrappers bien nommés facilitent la compréhension des scripts.
- **Encapsule la complexité** – Les détails de bas niveau sont traités à l'intérieur du wrapper, gardant le script principal propre.
- **Améliore la maintenabilité** – Si la fonctionnalité principale change, nous devons uniquement mettre à jour le wrapper, pas tous les scripts qui en dépendent.
- **Encourage la réutilisation et la cohérence** – Notre bibliothèque personnelle de fonctions s'agrandit au fil du temps, rendant le développement plus rapide et plus efficace.

En utilisant systématiquement l’encapsulation de fonctions, nous pouvons transformer la façon dont nous écrivons les plug-ins Scheme, créant ainsi un environnement de script plus modulaire et plus expressif. En gardant ces principes à l’esprit, nous pouvons continuer à affiner notre approche, en développant une version plus efficace et plus adaptée de Scheme qui répond à nos besoins spécifiques.

Prochaines étapes : identifiez les blocs répétés dans vos scripts et extrayez les petits assistants aux noms clairs.