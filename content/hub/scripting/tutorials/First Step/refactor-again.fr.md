---
title: "Refactoriser à nouveau"
type: docs
weight: 5
---
À mesure que la bibliothèque d’assistance s’agrandit, il devient plus difficile de la suivre d’un seul coup d’œil. Refactorisez à nouveau pour que chaque fonction reste petite et à usage unique.

### Décomposer la complexité

Pour rendre la fonction plus facile à suivre et à maintenir, divisez-la en fonctions plus petites et ciblées. Commencez par séparer la validation du routage des messages.

### Créer une fonction de validation

Nous pouvons prendre la partie de la fonction qui valide les arguments `message` et `output` et la déplacer dans une fonction distincte. De cette façon, la fonction principale `send-message` n'a pas à se soucier de la validation, ce qui la rend plus facile à suivre.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Simplifiez l'envoi des messages

Maintenant que la validation a été déplacée vers une fonction distincte, la fonction `send-message` peut se concentrer uniquement sur l'envoi du message. Ce sera beaucoup plus simple, car il ne gérera que la tâche spécifique consistant à diriger le message vers la bonne destination.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

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
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Décomposer davantage : séparer chaque gestionnaire de sortie

Chaque type de sortie de message (GUI, console d'erreurs, terminal) peut être déplacé dans sa propre fonction. Cela permet de faciliter les tests, les modifications et les extensions potentielles à l'avenir.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Réutilisation de la validation dans chaque fonction d'envoi

Étant donné que la validation est un élément important pour garantir que le message et le résultat sont corrects, il est logique que chaque fonction `send-*` effectue sa propre validation. Cela garantit que quelle que soit la sortie appelée, nous vérifions toujours les entrées en premier.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Voyez que nous avons supprimé la validation de la fonction d'envoi de message et transféré la responsabilité à chaque fonction de sortie individuelle. Ce changement garantit que chaque destination (interface graphique, console d'erreurs, terminal) gère sa propre validation, rationalisant ainsi la fonction d'envoi de message et gardant la logique de validation plus proche de l'endroit où elle est nécessaire.

Cette approche peut simplifier la fonction d'envoi de message, en en faisant un _dispatcher_, tout en garantissant que chaque fonction d'envoi à* valide correctement le message avant son traitement.

En déplaçant la validation dans chaque fonction send-to-*, nous les avons rendues réutilisables en tant que fonctions autonomes. Cela signifie que nous pouvons appeler directement n'importe quelle fonction d'envoi à l'interface graphique, d'envoi à la console d'erreur ou d'envoi au terminal sans compter sur la fonction de répartiteur d'envoi de message. Chacune de ces fonctions gère désormais entièrement sa propre logique et peut être utilisée indépendamment dans d'autres parties du code ou dans d'autres plug-ins, rendant votre code plus modulaire et flexible.

## Avantages de la refactorisation

- **Séparation claire des préoccupations** : chaque fonction ne gère désormais qu'une seule responsabilité, ce qui rend le code plus facile à comprendre.
- **Extensibilité** : L'ajout de nouveaux types de sortie est simple. Vous définissez simplement une nouvelle fonction comme `send-to-file` ou `send-to-logger`, puis ajoutez une casse dans l'instruction `cond`.
- **Réutilisabilité** : Chacune de ces fonctions de gestion de sortie peut être réutilisée ailleurs dans votre projet ou partagée entre plusieurs plug-ins.
- **Cohérence** : En réutilisant la fonction de validation dans chaque fonction `send-to-*`, vous vous assurez que toutes les sorties sont correctement validées, ce qui rend le code plus robuste.

Une version de bibliothèque refactorisée :

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Est-ce tout ce que nous pouvons faire ? Non! il y a encore beaucoup à faire, veuillez continuer à lire.