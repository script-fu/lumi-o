---
title: "Refactorisation"
type: docs
weight: 2
---
Une fois qu’une fonction fonctionne, nous pouvons prendre du recul et réfléchir à la meilleure façon de structurer notre code. L'objectif est de rendre notre plug-in aussi clair, compréhensible et maintenable que possible. Ce processus d'amélioration et d'affinement de la structure du code existant sans modifier son comportement est appelé refactoring.

Voici à nouveau la fonction initiale :

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

Le nom de la fonction est le nom de la fonction et le paramètre est ce que la fonction accepte en entrée. Le corps est le bloc de code qui s'exécute lorsque la fonction est appelée.

Forme abstraite :

```scheme
(define (function-name parameter)
  body)
```

### Répétition des codes

Supprimez les répétitions tôt. `(lumi-message "Hello world!\n")` est répété deux fois et la chaîne du message est répétée trois fois. Une variable résout la chaîne répétée.

### Variables

Dans Scheme, une variable a une "portée", là où elle est connue, et cette portée est définie à l'aide d'une instruction `let`. La variable est liée à une valeur dans la partie de liaison et la variable a une portée dans le corps let. La variable n'est connue qu'à l'intérieur du bloc let et n'est pas accessible en dehors de celui-ci.

```scheme
(let ((variable value))
  body)
```

Introduction d'une variable appelée "message":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

Dans notre exemple, nous avons utilisé une variable appelée "message" liée à une chaîne "Hello world!\n". Cela nous permet de modifier le contenu du message une fois au lieu de trois fois, réduisant ainsi le risque d'erreurs et rendant le code plus flexible.

### Fonctions d'extraction

En programmation fonctionnelle, refactoriser le code pour extraire la logique réutilisable en fonctions distinctes est une pratique courante. En faisant cela, la **fonction principale** devient beaucoup plus simple et plus axée sur son objectif de haut niveau, tandis que la **fonction extraite** semble plus complexe car elle gère la logique détaillée. Ceci est intentionnel et correspond aux principes fondamentaux de la programmation fonctionnelle, tels que la modularité, la séparation des préoccupations et la lisibilité. Voici le refactorisé
Bonjour tout le monde ! après extraction.

Extraire la logique :
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Symboles
Dans l'exemple ci-dessus, un type de données appelé symbole est utilisé, tel que 'gui. Les symboles sont transmis en tant que paramètres à la fonction d'envoi de message et peuvent être utilisés pour prendre des décisions conditionnelles simples. Comme les clés symboliques, ce sont des identifiants uniques. Pour plus d'informations sur les symboles, visitez [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Simplifier la fonction principale

Dans la fonction originale (scheme-hello-world), toute la logique d'envoi de messages à différentes sorties (GUI, Error Console, Terminal) était mélangée dans la fonction principale. Après la refactorisation, la fonction principale se concentre simplement sur **ce qui doit être fait**, en envoyant le message à différentes destinations.

La fonction principale refactorisée est plus simple :

- Il indique clairement son objectif : envoyer le même message à plusieurs sorties.
- Cela évite d'encombrer la logique principale avec du code répétitif comme la configuration de gestionnaires de messages pour différentes sorties.
- C'est plus facile à lire et à comprendre d'un seul coup d'œil.

### La complexité de la fonction extraite

En revanche, la fonction **(send-message)** est l'endroit où réside la logique détaillée. Il gère désormais les variations de comportement pour chaque sortie (GUI, Error Console, Terminal). La fonction est un peu plus complexe qu'avant, mais elle est désormais **centralisée** et **isolée**.

## Relier cela à la programmation fonctionnelle

Dans la programmation fonctionnelle, les fonctions sont considérées comme des **citoyens de première classe**, ce qui signifie qu'elles peuvent être réutilisées, transmises et combinées pour former un comportement plus complexe. Le but est de :- **Décomposez les problèmes** en morceaux plus petits et indépendants.
- **Isolez la complexité** dans des fonctions plus petites qui gèrent des tâches spécifiques, comme `send-message`.
- **Gardez la simplicité des fonctions de niveau supérieur** afin qu'elles puissent se concentrer sur l'orchestration du flux de données et d'actions, sans avoir besoin de connaître les détails de la façon dont chaque tâche est accomplie.
- **Séparation des préoccupations** : La fonction s'occupe de la manière dont le message est envoyé en fonction du type de sortie, ce qui isole cette logique de la fonction principale.
- **Modularité** : En gérant toute la logique d'envoi de messages en un seul endroit, nous pouvons facilement apporter des modifications (comme l'ajout de nouvelles options de sortie) sans altérer la fonction principale.
- **Réutilisabilité** : La fonction `send-message` est réutilisable, ce qui signifie que si nous devons envoyer un message à plusieurs sorties ailleurs dans notre code, nous pouvons simplement appeler cette fonction plutôt que de réécrire une logique similaire.

En refactorisant, la fonction principale de cet exemple devient une déclaration **déclarative** de ce qui se passe (« envoyer un message à trois endroits »), tandis que la complexité de la façon d'envoyer ces messages est résumée dans la fonction `send-message`.