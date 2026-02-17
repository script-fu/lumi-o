---
title: "Pensées finales"
type: docs
weight: 10
---
Vous disposez désormais d'un plug-in de procédure de travail et d'une petite bibliothèque d'assistance. Cette série a présenté les modèles de base que vous utiliserez dans la plupart des scripts Lumi :

- Fonctions : Les éléments constitutifs de nos plug-ins.
- Refactoring : Améliorer la structure du code tout en conservant les fonctionnalités.
- Bibliothèques de codes : centraliser les fonctions réutilisables pour garder notre code propre et modulaire.
- Techniques de validation : s'assurer que les entrées sont valides avant d'exécuter notre logique de base.

Vous avez également vu les bases de l'utilisation de Git pour suivre les modifications et conserver une structure de projet propre. Ce flux de travail facilite les itérations sans perdre les versions de travail.

Voici la version finale de notre code principal de plug-in :

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Code de la bibliothèque :

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

En refactorisant les assistants de messagerie dans une petite bibliothèque, le plug-in reste concentré sur l'intention et la bibliothèque contient les détails d'implémentation. La validation et le routage cohérent des messages garantissent la prévisibilité des échecs.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Prochaines étapes :

- Déplacez les assistants réutilisables dans un fichier de bibliothèque dédié.
- Gardez les plug-ins petits et nommez les procédures pour ce qu'ils font.
- Ajouter une validation aux limites (entrées, chemins de fichiers, options de menu).

Conservez le résultat final sous forme de deux fichiers dans votre référentiel de plug-ins :

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`