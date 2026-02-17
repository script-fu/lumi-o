---
title: "Retravailler"
type: docs
weight: 7
---
Cette étape corrige un comportement subtil dans l’exemple de messagerie.

Nous transmettions la chaîne "Hello world!\n" comme message. Le "\n" est un type particulier de caractère, un caractère "d'échappement". Il indique à l'impression de sortie de démarrer une nouvelle ligne. Dans Scheme, cela forcera également un message envoyé à la barre d'état à apparaître sous forme de boîte d'interface graphique.

L'assistant `send-to-gui` envoie des messages à une boîte de dialogue Lumi.

Mettez à jour le contenu et les destinations du message afin que l'exemple se comporte de manière cohérente.

Suppression du caractère d'échappement et extension des fonctions :
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Remplacez les nombres magiques par les constantes fournies par Lumi (par exemple, `MESSAGE-BOX` et `ERROR-CONSOLE`).

Divisez ensuite la validation en deux fonctions afin qu'elle puisse être réutilisée à partir de plusieurs sites d'appel.

- (is-valid-string ?) Pour vérifier qu'une chaîne est une chaîne et non une chaîne vide, dans une fonction send-to*.
- (is-valid-output-display ?) Pour vérifier qu'une destination de sortie donnée est valide, dans la fonction d'envoi de message.

Retravailler la bibliothèque :

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusion

En retravaillant notre bibliothèque de messagerie, nous l'avons rendue plus robuste et plus fiable. Nous avons résolu le problème caché avec le caractère de nouvelle ligne, introduit des constantes pour une meilleure clarté et étendu les fonctionnalités en ajoutant la prise en charge de la barre d'état et des sorties de la boîte de dialogue. De plus, séparer la logique de validation en fonctions plus petites et ciblées garantit que notre code est plus facile à maintenir et à étendre à l'avenir.

Cette refonte démontre comment de petits changements peuvent améliorer la structure globale et les fonctionnalités de notre bibliothèque, ouvrant la voie à plus de flexibilité et de réutilisabilité à mesure que notre projet se développe.