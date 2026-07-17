---
title: "Pensées finales"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5233667e27065df0a6bc940209f767b9f9e32876d41fa3d09428737b535906e9
---
Vous disposez désormais d'un plug-in de procédure de travail et d'une petite bibliothèque d'assistance. Cette série a présenté les modèles de base que vous utiliserez dans la plupart des scripts Lumi :

- Fonctions : Les éléments constitutifs de nos plug-ins.
- Refactoring : Améliorer la structure du code tout en conservant les fonctionnalités.
- Bibliothèques de codes : centraliser les fonctions réutilisables pour garder notre code propre et modulaire.
- Techniques de validation : s'assurer que les entrées sont valides avant d'exécuter notre logique de base.

Vous avez également vu les bases de l'utilisation de Git pour suivre les modifications et conserver une structure de projet propre. Ce flux de travail facilite les itérations sans perdre les versions de travail.

Voici la version finale de notre code principal de plug-in :

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
;; Objectif : Envoie un message vers la barre d'état, renvoie #t en cas de succès
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Objectif : Envoie un message vers la boîte de dialogue, renvoie #t en cas de succès
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Objectif : Envoie un message vers la console d'erreurs, renvoie #t en cas de succès
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Objectif : Envoie un message vers le terminal, renvoie #t en cas de succès
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Objectif : Envoie un message vers la sortie appropriée, renvoie #t en cas de succès
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Objectif : Vérifie que le message est une chaîne non vide, renvoie #t s'il est valide
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Objectif : Vérifie que la sortie est une destination valide, renvoie #t si valide
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