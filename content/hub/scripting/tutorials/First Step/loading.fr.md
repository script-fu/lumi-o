---
title: "Chargement"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
url: "hub/scripting/tutorials/First Step/loading"
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
    - **hello-world/**: Dossier pour le plug-in spécifique « Hello World! ».
      - **hello-world.scm**: Fichier de script du plug-in.

Exemple de fonction de bibliothèque send-message.scm

```scheme
;; Fonction pour gérer la sortie de messages vers diverses destinations
(define (send-message message output)
  (cond
    ;; Envoyer vers la console de messages
    ((eq? output 'error-console)
       ;; Définir le gestionnaire sur console de messages
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Envoyer vers la boîte de dialogue GUI
    ((eq? output 'gui)
       ;; Définir le gestionnaire sur la boîte de dialogue GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Envoyer vers la fenêtre de terminal
    ((eq? output 'terminal)
       ;; La sortie du terminal est gérée avec display
       (display message)))

  ;; Restaurer le gestionnaire de messages par défaut vers la console de messages
  (lumi-message-set-handler 2))
```

### Charger la fonction bibliothèque

Nous pouvons charger cette fonction de bibliothèque avec la commande Scheme `load` ;

Chargement d'un fichier de bibliothèque :

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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