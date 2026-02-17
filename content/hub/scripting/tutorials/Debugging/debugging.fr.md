---
title: "Débogage"
type: docs
weight: 5
---
En scripting, aucune fonction n’est infaillible. Même les commandes les plus fiables peuvent échouer lorsqu’elles sont confrontées à des entrées ou à des conditions inattendues. Pour nous prémunir contre cela, nous pouvons implémenter un système de débogage personnalisé et adopter des techniques de programmation défensive. En enveloppant les fonctions standard avec des mécanismes de gestion des erreurs et en fournissant des commentaires informatifs, nous pouvons rendre nos scripts plus robustes et plus faciles à dépanner.

Un élément clé de cette stratégie consiste à utiliser un indicateur de débogage global pour contrôler la sortie détaillée, ce qui nous permet d'activer des informations de débogage détaillées en cas de besoin tout en gardant la sortie propre pendant l'exécution normale.

## Indicateur de débogage global

Un indicateur de débogage global est un moyen simple mais efficace de contrôler le niveau d'informations générées lors de l'exécution du script. Lorsqu'il est activé, il fournit des messages de débogage détaillés qui peuvent s'avérer inestimables pour détecter les problèmes. Lorsqu'il est désactivé, il maintient la sortie concise pour une utilisation en production.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

Par défaut, le débogage est désactivé. Pour activer la sortie détaillée pendant le développement, définissez simplement l'indicateur sur `#t` :

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

Nous pouvons également activer ou désactiver temporairement le débogage pour des sections spécifiques de code à l'aide de fonctions d'assistance.

### Contrôle de débogage local

Pour un contrôle plus précis, nous pouvons activer ou désactiver le débogage dans des parties spécifiques du script à l'aide de fonctions d'assistance.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Cela nous permet de contrôler le débogage de manière dynamique :

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Système de messagerie de débogage

Pour gérer efficacement la sortie de débogage dans Scheme, nous utilisons une approche structurée impliquant plusieurs fonctions d'assistance. Ces fonctions garantissent que les messages de débogage et d'avertissement sont clairs, lisibles et maintenables.

### Présentation du système de messagerie de débogage

Notre système de messagerie de débogage se compose des composants suivants :

1. `debug-message` – Affiche les messages de débogage lorsque le débogage est activé.
2. `serialize-item` – Convertit divers types de données Scheme en une représentation sous forme de chaîne.
3. `concat` – Concatène plusieurs éléments en une seule chaîne.
4. `list->string` – Formate une liste en une chaîne lisible.
5. `message` – Affiche la sortie dans la console de message de Lumi.
6. `warning-message` – Affiche les messages d'avertissement lorsque les avertissements sont activés.

Chaque fonction joue un rôle dans le formatage et l'affichage des messages structurés.

---

### Fonction de message de débogage

La fonction `debug-message` est la méthode principale pour afficher la sortie de débogage. Cela garantit que les messages ne sont affichés que lorsque le débogage est activé.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- La condition `when debug` garantit que les messages apparaissent uniquement lorsque le débogage est activé.
- Les messages sont préfixés par `"> "` pour plus de clarté.
- La fonction utilise `concat` pour formater le contenu du message.
- Enfin, il appelle `message` pour envoyer la sortie à la console de message de Lumi.

Exemple d'utilisation :

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Avec le débogage activé, le résultat pourrait être :

```scheme
> item: background-layer has tree position : 3
```

### Sérialisation des données pour les messages de débogage

Les messages peuvent contenir différents types de données tels que des listes, des vecteurs et des nombres. Pour nous assurer qu'ils sont correctement formatés, nous utilisons `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Exemple d'utilisation :

```scheme
(serialize-item '(1 2 3))
```

Sortie :

```scheme
list:
1
2
3
```

### Concaténation des messages

Pour fusionner plusieurs composants de message en une seule chaîne, nous utilisons `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Exemple d'utilisation :

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formatage des listes sous forme de chaînes

La fonction `list->string` convertit une liste en chaîne formatée.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Messages d'avertissementLa fonction `warning-message` fonctionne de manière similaire à `debug-message`, mais elle affiche des avertissements même lorsque le débogage est désactivé.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Garantit que les messages ne sont affichés que lorsque les avertissements sont activés (l'indicateur `warning` est défini dans `common.scm` comme `#t`).
- Appelle `concat` pour formater le contenu du message.
- Utilise `message` pour envoyer la sortie à Lumi.

## Amélioration des fonctions standard

Une fois un système de débogage en place, nous pouvons améliorer notre bibliothèque de fonctions en incorporant des messages détaillés. Cela fournit un aperçu des états des éléments, des valeurs des variables et des appels de fonction.

Un exemple courant est `item-is-valid?`, qui encapsule `lumi-item-id-is-valid` pour renvoyer `#t` ou `#f`. Si `#f` est renvoyé, nous pouvons déclencher un `warning-message` dans le code appelant, si l'entrée n'est pas un nombre, nous pouvons donner un avertissement dans la fonction.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Utilisation pratique

Lors du développement de plug-ins Scheme, l'encapsulation des fonctions de cette manière réduit considérablement le temps de débogage et garantit un code robuste et maintenable. Avec notre système de débogage en place, nous pouvons générer un flux de débogage structuré dans la console d'erreurs en un simple clic.

Dans ce flux de débogage, les appels de fonction sont marqués d'un astérisque (*), ce qui facilite le suivi de l'exécution des scripts et l'identification des échecs, en particulier dans les plug-ins complexes. Cette visibilité nous aide à comprendre le flux des opérations et à diagnostiquer efficacement les comportements inattendus.

Un wrapper pour notre fonction de message pour utiliser un `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Exemple de `call` utilisé en pratique :

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Exemple de flux de débogage lors de l'exécution d'un plug-in :

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Ce journal structuré fournit une chronologie claire des appels de fonctions et des modifications de données, ce qui facilite considérablement le débogage et l'analyse des performances.

## Conclusion

En mettant en œuvre un système de débogage structuré, nous créons des scripts plus sûrs et plus maintenables qui offrent des informations en temps réel sur leur exécution.

### Points clés à retenir

- **Contrôler la verbosité** – Utilisez un indicateur de débogage global pour gérer les niveaux de sortie.
- **Fournir des commentaires clairs** – Enveloppez les fonctions standard avec des messages de débogage informatifs.
- **Améliorer la robustesse** – Gérez les entrées inattendues avec élégance pour éviter les erreurs.
- **Simplifiez le dépannage** – Les messages de débogage structurés facilitent le diagnostic et la résolution des problèmes.

Grâce à cette approche, nos scripts « s'expliquent » efficacement au fur et à mesure qu'ils traitent les données, réduisant ainsi la frustration et améliorant l'efficacité du flux de travail. Le débogage devient un outil proactif plutôt qu'une corvée réactive, rendant notre processus de script à la fois plus fluide et plus gratifiant.