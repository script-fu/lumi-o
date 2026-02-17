---
title: "Valeurs de retour"
type: docs
weight: 8
---
Les valeurs de retour sont importantes car elles vous permettent de contrôler le flux sans état supplémentaire. Dans Scheme, la dernière expression évaluée devient la valeur de retour.

Cette page utilise les assistants de validation de l'exemple de messagerie pour montrer comment les valeurs de retour explicites facilitent la composition du code.

### Qu'est-ce qu'une valeur de retour ?

Dans Scheme, la valeur de retour d'une fonction est déterminée par la dernière expression évaluée par la fonction. Cela signifie que la dernière ligne de code de la fonction évaluée sera renvoyée comme résultat de la fonction. Si aucune valeur n'est explicitement renvoyée, la fonction renvoie `#f` (false) ou `undefined`.

Revoyons la fonction de validation, (is-valid-string ?)

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Dans cette fonction, si le message n'est pas valide, une erreur est générée. Cependant, si le message est valide, aucune valeur de retour explicite n'est donnée et la fonction renvoie `#f` par défaut.

### Rendre les valeurs de retour explicites

Nous pouvons améliorer cela en rendant la valeur de retour plus explicite. Par exemple, nous pourrions renvoyer `#t` (true) si le message est valide :

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

Dans cette version, la fonction retournera `#t` lorsque le message est valide, fournissant un résultat clair. Cela permet à la fonction d'être utilisée de manière plus flexible dans d'autres contextes où un résultat booléen est nécessaire.

### Utiliser efficacement les valeurs de retour

En décidant de ce que renvoient nos fonctions, nous pouvons les rendre plus prévisibles et plus utiles. Renvoyer des valeurs telles que `#t`, `#f` ou un résultat spécifique nous donne plus de contrôle sur la façon dont la fonction interagit avec le reste du code. Par exemple, vous pouvez utiliser la valeur de retour pour prendre d'autres décisions dans la fonction appelante ou la transmettre comme argument à une autre fonction.

Voici un exemple simple d’utilisation d’une valeur de retour pour contrôler le flux logique :

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

Dans ce cas, (send-message) s'appuie sur la valeur de retour de (is-valid-output-display ?) pour décider s'il faut continuer.
L'instruction conditionnelle `cond` sera ignorée si le premier test échoue. Remarquez également comment il se lit de manière assez naturelle, si l'affichage de sortie est valide ?

## Si la logique de l'instruction dans le schéma

Avant l'exemple de bibliothèque refactorisée, voici un rapide aperçu de la logique conditionnelle. Scheme utilise `if` pour choisir entre deux chemins.

Voici une forme simple d'une instruction `if` :

```scheme
(if (conditional test)
  do if true
  do if false)
```

Cette structure vérifie la condition, et si la condition est vraie, elle exécute la première action. Si la condition est fausse, il exécute la deuxième action.

Dans les cas où vous devez effectuer plusieurs actions lorsque la condition est vraie ou fausse, vous pouvez utiliser `begin` pour les regrouper :

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Cela vous permet de gérer des situations plus complexes, dans lesquelles plusieurs expressions ou instructions doivent être exécutées en fonction du résultat du test conditionnel.

D'accord, voici le code de la bibliothèque avec les valeurs de retour intégrées et utilisées pour contrôler le processus d'exécution.

### Refactorisé avec des valeurs de retour

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusion

Les valeurs de retour sont un élément fondamental pour rendre les fonctions flexibles et réutilisables. En décidant soigneusement ce que chaque fonction doit renvoyer, nous pouvons garantir que nos fonctions interagissent bien les unes avec les autres et fournissent des informations utiles au reste du code. Qu'il s'agisse de renvoyer `#t` ou `#f`, ou quelque chose de plus spécifique, les valeurs de retour nous donnent un moyen de contrôler le flux de nos programmes et de gérer divers résultats.