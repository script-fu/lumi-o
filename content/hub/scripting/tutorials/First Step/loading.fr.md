---
title: "Chargement"
type: docs
weight: 3
---
Dès qu'une fonction d'assistance se développe, déplacez-la dans un petit fichier de bibliothèque. Cela permet de garder le plug-in concentré et de rendre l'assistant réutilisable sur plusieurs plug-ins.

### Créer une fonction de bibliothèque

Nous pouvons prendre la fonction d'envoi de message et créer un nouveau fichier avec cela comme contenu. Enregistrez le fichier dans votre dossier de dépôt, pas dans la partie plugins, peut-être près du niveau supérieur ;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/** : Il s'agit de votre répertoire principal pour stocker votre code Scheme.
  - **library/** : C'est ici que vivent les fonctions partagées comme `send-message.scm`.
  - **plug-ins/** : C'est ici que vos plug-ins individuels sont stockés.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Exemple de fonction de bibliothèque send-message.scm

```scheme
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
```

### Charger la fonction bibliothèque

Nous pouvons charger cette fonction de bibliothèque avec la commande Scheme `load` ;

Chargement d'un fichier de bibliothèque :

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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

Hé! Nous avons maintenant quelque chose de plus simple et plus court à lire, qui se décrit lui-même sans commentaires. C’est la conclusion satisfaisante du refactoring.