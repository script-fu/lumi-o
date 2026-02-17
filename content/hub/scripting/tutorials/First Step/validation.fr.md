---
title: "Validation"
type: docs
weight: 4
---
Lors de la création de plug-ins robustes, il est important de s’assurer que nos fonctions gèrent les erreurs correctement et fonctionnent comme prévu, même en cas d’utilisation abusive ou d’entrées inattendues. La validation permet de protéger l'intégrité de la fonction et d'éviter les plantages ou les comportements involontaires.

Voyons comment nous pouvons améliorer la fonction `send-message` en ajoutant des contrôles de validation pour garantir qu'elle gère correctement les entrées.

### Valider les entrées

Avant d'envoyer un message, nous devons nous assurer que l'argument `output` transmis à la fonction `send-message` est valide. Nous pouvons ajouter une vérification pour confirmer que la destination de sortie est l'une des valeurs attendues (gui, console d'erreur ou terminal).

Exemple :

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Send to the Error Console
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Send to the GUI dialog box
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Send to the terminal window
      ((eq? output 'terminal)
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

Dans cet exemple, nous utilisons `member` pour vérifier si l'argument `output` est valide. Sinon, la fonction génère une erreur avec un message clair, empêchant les valeurs non valides de provoquer des problèmes.

### Gérer les messages vides

Il est également utile de s'assurer que l'argument `message` est valide. Par exemple, si une chaîne vide ou #f (false) est transmise comme message, la fonction doit gérer cela avec élégance.

Exemple de gestion d'un message vide :

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Cette approche garantit que la fonction reçoit toujours des entrées valides, améliorant ainsi sa fiabilité et évitant tout comportement inattendu.

### Exemple de validation combinée

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Send to the Error Console
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Send to the GUI dialog box
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Send to the terminal window
        ((eq? output 'terminal)
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

Dans cette version :
- La fonction vérifie d'abord si le `message` est vide ou invalide. Si le message est valide, il vérifie si `output` est l'une des valeurs acceptées (`gui`, `error-console` ou `terminal`).
- Si les deux vérifications réussissent, le message est envoyé à la sortie appropriée. Sinon, un message d'erreur s'affiche avec une explication claire.
- Une vérification supplémentaire est effectuée pour s'assurer que le message est également une chaîne.

Cette fonction de validation combinée maintient le code plus propre et garantit que les deux entrées sont validées avant qu'une action ne soit entreprise, ce qui rend la fonction plus robuste. Remarquez que nous intégrons également un système de messagerie de débogage. Quand le
le code échoue, nous obtenons une raison, une raison que nous avons écrite nous-mêmes.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```