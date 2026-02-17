---
title: "Bonjour le monde!"
type: docs
weight: 1
---
Ce didacticiel présente la structure minimale d'un plug-in Scheme. Certaines lignes sont « passe-partout » : elles sont nécessaires à Lumi pour charger le fichier, même si vous ne les comprenez pas encore complètement.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

À un niveau élevé, vous :

1. Définir une fonction
2. Enregistrez-le pour qu'il apparaisse dans la base de données des procédures
3. (Facultatif) Ajouter une entrée de menu
4. Installez le fichier dans un dossier de plug-ins

### Définir une fonction

Une fonction, également connue sous le nom de _procédure_, est un morceau de code avec un nom et un objectif, elle prend une entrée et produit une sortie.

**Entrée** > **_Fonction_** > **Sortie**

### Enregistrez la fonction

L'enregistrement consiste à mettre le nom de la fonction sur une liste afin que Lumi le sache.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Lien vers le menu

Cela indique à Lumi où trouver votre fonction dans son système de menus.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Cela affiche le menu "Funky" dans la barre de menu principale. Modifiez le chemin pour placer le plug-in ailleurs. Le chemin `<Image>/Funky` signifie que le plug-in apparaîtra dans la catégorie de menu **Image**. Vous pouvez remplacer `<Image>` par `<Tools>`, `<Filters>`, etc., selon l'endroit où vous souhaitez que le plug-in apparaisse.

### Commentaires

Dans Scheme, le langage de base de Scheme, les commentaires sont généralement effectués en faisant précéder une ligne de texte utile de `;;`. Votre utilisation des commentaires dépendra de votre maîtrise du codeur : si vous codez occasionnellement, davantage de commentaires vous seront utiles. Si vous codez tout le temps, le code est aussi facile à lire que le serait le commentaire. De plus, lors de la programmation fonctionnelle, le code a tendance à devenir suffisamment descriptif pour se lire comme un script.

### Syntaxe

Le code a tendance à avoir de petites règles sur la façon de placer les éléments dans une ligne, afin que nous puissions lire la ligne facilement. Par exemple, une phrase peut comporter un espace après une virgule ou un point. Cela aide à la lisibilité.

Le code peut organiser les choses de la même manière, ce qui peut paraître étrange au premier abord :

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Exemple de code

Voici l'exemple complet. La plupart des procédures Lumi portent le préfixe `lumi-`. Par exemple, `lumi-message` imprime une chaîne dans le gestionnaire de messages configuré.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


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

### Installer le plug-in

1. Accédez à **Lumi -> Modifier -> Préférences -> Dossiers -> Plug-ins**.
2. Ajoutez votre dossier de plug-ins [repo](/hub/scripting/tools/git) à la liste.
3. Créez un dossier pour le plug-in et enregistrez l'exemple de code ci-dessus sous `hello-world.scm` :
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Cliquez avec le bouton droit sur le fichier `hello-world.scm`.
5. Accédez à **Propriétés -> Autorisations -> Autoriser l'exécution du fichier en tant que programme**.
6. Redémarrez Lumi.

### Essayez le plug-in

Le plug-in devrait maintenant apparaître sous le menu "Funky" dans la fenêtre principale de Lumi. Cliquez dessus et il devrait afficher le message "Bonjour tout le monde !" message. Essayez de modifier le code, comme changer le texte du message, et enregistrez le fichier. Lorsque vous exécuterez à nouveau le plug-in, vos modifications seront reflétées sans redémarrer Lumi.

Essayez d'expérimenter en modifiant le chemin du menu. Par exemple, `"<Image>/File"` le placera dans le menu Fichier et `"<Image>/File/Funky"` créera une nouvelle section dans le menu Fichier. C'est un excellent moyen de personnaliser l'endroit où votre plug-in apparaît et d'organiser vos outils.