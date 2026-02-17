---
title: "Le navigateur de procédures"
type: docs
weight: 1
---
Le **Lumi Procedure Browser** vous permet de rechercher les procédures disponibles (intégrées et plug-in fournies) et d'inspecter leurs paramètres et leurs valeurs de retour.

### Où trouver le navigateur de procédures Lumi

Vous pouvez accéder au navigateur de procédures dans Lumi via le menu **Aide** :

- **Aide** -> **Navigateur de procédures**

### Ce que fait le navigateur de procédures

Le navigateur de procédures répertorie toutes les procédures internes de Lumi, ainsi que celles ajoutées par les plug-ins, y compris celui que vous venez d'installer. Chaque entrée de procédure fournit des informations utiles, notamment :

- Le nom de la procédure.
- Une description de ce qu'il fait.
- Les paramètres qu'il accepte (valeurs d'entrée).
- Les valeurs de retour (sortie).

Recherchez par mot-clé ou nom de procédure lorsque vous devez vérifier une signature d'appel ou confirmer le nom exact de la procédure.

#### (message lumi) dans le navigateur de procédures

Recherchez `lumi-message` pour voir ses paramètres et ses valeurs de retour.

### Trouver votre plug-in

Une fois que vous avez installé le programme "Hello World!" plug-in, vous pouvez le trouver répertorié dans le navigateur de procédures. Recherchez simplement le nom de fonction que vous avez enregistré auprès de Lumi, dans ce cas, "scheme-hello-world". L'entrée affichera les paramètres et toutes les valeurs de retour associées au plug-in, ainsi qu'une brève description. Vous verrez également où certaines des lignes de texte que vous avez saisies comme paramètres de saisie lors du processus d'inscription sont affichées dans la section **Informations supplémentaires**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Cela permet de vérifier facilement que votre plug-in est correctement enregistré et vous donne un moyen rapide de vérifier comment il interagit avec d'autres procédures dans Lumi. Le navigateur de procédures est un outil puissant pour déboguer et développer vos plug-ins en explorant toutes les procédures disponibles dans Lumi.