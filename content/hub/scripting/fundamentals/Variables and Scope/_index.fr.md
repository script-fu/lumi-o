---
title: "Variables et portée"
type: docs
weight: 1
---
Dans Scheme, la gestion des variables et de leur portée est un concept central pour écrire des scripts efficaces et maintenables. Les variables stockent les valeurs de données que votre script peut manipuler, tandis que la portée définit l'endroit où ces variables sont accessibles. Comprendre comment définir et utiliser efficacement les variables vous permet de créer un code structuré, réutilisable et sans erreur.

### Saisie dynamique

Scheme est typé dynamiquement : vous ne déclarez pas de types à l'avance, et une variable peut contenir des valeurs de différents types au fil du temps.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### Le rôle des définitions et de la portée des variables dans le schéma

Définir des variables et gérer leur portée répond à plusieurs objectifs :
- **Organisation des données :** Les variables stockent des informations, ce qui rend vos scripts plus lisibles et gérables.
- **Amélioration de la réutilisabilité :** En utilisant des variables étendues, vous pouvez réutiliser des sections de code sans conflits.
- **Encapsulation :** La portée localisée empêche les interactions involontaires entre les variables dans différentes parties de votre script.
- **Simplification de la logique :** Les variables temporaires dans une portée limitée réduisent la complexité des calculs ou des flux de travail plus importants.

### Types de définitions de variables et portée

Scheme fournit plusieurs constructions pour définir et définir la portée des variables :
- **`let`:** Crée des liaisons locales pour les variables dans un bloc de code spécifique.
- **`let*`:** Une version séquentielle de `let` où chaque liaison peut dépendre des précédentes.
- **Nommé `let` :** Une construction puissante pour définir des procédures ou des boucles locales récursives.
- **`define`:** Crée des variables globales ou des fonctions accessibles dans tout votre script.

### Fonctionnement des définitions et de la portée des variables

Les définitions et la portée des variables impliquent généralement :
1. **Déclaration de variables :** attribution d'une valeur à une variable dans un contexte spécifique.
2. **Portée limitée :** Contrôler où la variable est accessible (par exemple, dans un bloc `let` ou globalement).
3. **Utilisation de variables :** Accès et modification des valeurs de variables pour effectuer des calculs, des opérations logiques ou procédurales.

### Exemple : utilisation de `let` pour les variables locales

La construction `let` vous permet de définir des variables temporaires disponibles uniquement dans un bloc spécifique :

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Cet exemple déclare `x` et `y` avec des valeurs locales et calcule leur somme.

### Exemple : utilisation de `define` pour les variables globales

La construction `define` crée des variables ou des fonctions avec une portée globale :

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Ce script définit une constante globale `pi` et une fonction `circle-area` qui l'utilise.

### Comparaison de la portée : Local vs Global

| Fonctionnalité | Portée locale (`let`, `let*`) | Portée mondiale (`define`) |
|------------------|--------------------------------|-------------------------------------------------------------|
| **Accessibilité** | Limité au bloc dans lequel il est défini | Accessible tout au long du script |
| **Encapsulation** | Empêche les interactions involontaires | Peut entrer en conflit avec d'autres variables définies globalement |
| **Cas d'utilisation** | Variables temporaires pour des tâches spécifiques | Variables ou fonctions partagées utilisées partout |

### Résumé- **Les définitions et la portée des variables** sont fondamentales pour organiser et gérer les données dans vos scripts Scheme.
- Utilisez **portée locale** (`let`, `let*`, nommé `let`) pour encapsuler les variables temporaires et éviter les conflits.
- Utilisez la **portée globale** (`define`) pour les fonctions réutilisables ou les constantes partagées dans votre script.
- Une compréhension claire de ces constructions améliorera la lisibilité, la maintenabilité et la fiabilité de votre code.