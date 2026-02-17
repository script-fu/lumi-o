---
title: "Bibliothèque de messagerie"
type: docs
weight: 6
---
Au fil du temps, ce qui a commencé comme une fonction unique pour envoyer des messages a évolué vers un ensemble de fonctions liées. Ces fonctions constituent désormais la base d'une **bibliothèque de messagerie**, conçue pour gérer les sorties vers différentes destinations, telles que l'interface graphique, la console d'erreurs et le terminal.

### Pourquoi une bibliothèque de messagerie ?

À mesure que nos besoins augmentent, la gestion des messages sur plusieurs sorties nécessite une approche plus modulaire et extensible. Au lieu d’une seule fonction qui fait tout, nous avons divisé le processus en composants réutilisables, permettant une plus grande flexibilité. Cette bibliothèque peut désormais être utilisée comme un outil de messagerie à usage général auquel d'autres plug-ins ou fonctions peuvent emprunter.

### Que fait la bibliothèque de messagerie ?

La bibliothèque de messagerie comprend actuellement les fonctions suivantes :

- **send-to-gui** : envoie des messages à la boîte de dialogue Lumi GUI.
- **send-to-error-console** : envoie des messages à la console d'erreur Lumi.
- **envoyer au terminal** : envoie des messages à la fenêtre du terminal.
- **send-message** : une fonction de répartiteur qui dirige les messages vers la sortie appropriée.
- **validate-message** : garantit que le message et la sortie sont valides avant l'envoi.

### Extension de la bibliothèque

La **Bibliothèque de messagerie** peut facilement être étendue pour prendre en charge des sorties supplémentaires. Par exemple :

- **envoyer vers un fichier** : enregistre les messages dans un fichier journal.
- **send-to-logger** : intégration à un système de journalisation externe.
- **envoyer à une notification** : afficher les messages sous forme de notifications système.

En suivant le même modèle de conception modulaire et de fonctions réutilisables, cette bibliothèque peut devenir un outil complet permettant de gérer toutes sortes de tâches de messagerie.

## Avantages d'une bibliothèque de messagerie

- **Réutilisabilité** : Les fonctions peuvent être réutilisées dans différents plug-ins ou projets.
- **Modularité** : chaque fonction gère une tâche spécifique, ce qui rend le code plus facile à maintenir et à étendre.
- **Cohérence** : l'utilisation des mêmes fonctions de validation et de gestion des messages garantit un comportement cohérent dans l'ensemble de l'application.

La **Bibliothèque de messagerie** est le début d'un cadre plus large qui pourrait simplifier la façon dont les messages sont gérés dans votre projet. À mesure que la bibliothèque s'agrandit, de nouveaux plug-ins peuvent facilement y accéder pour envoyer des messages partout où ils doivent aller.

Nous pouvons ajuster la structure du fichier :

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Et n'oubliez pas d'ajuster le `load` dans le plug-in principal :

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

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