---
title: "Variables et portée"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a9918c313de4c5b034465400bfcbf1d493996435543a410382e481bde0d19ae4
---
En Scheme, la gestion des variables et de leur portée est un concept central pour écrire des scripts efficaces et maintenables. Les variables stockent les valeurs que votre script peut manipuler, tandis que la portée définit où elles sont accessibles. Savoir définir et utiliser les variables efficacement vous permet d'écrire un code structuré, réutilisable et fiable.

### Typage dynamique

Scheme est typé dynamiquement : vous ne déclarez pas les types à l'avance, et une variable peut contenir des valeurs de types différents au fil du temps.

```scheme
(define x 42)       ; x est un nombre
(set! x "hello")    ; maintenant x est une chaîne
```

### Le rôle des définitions de variables et de la portée dans Scheme

Définir des variables et gérer leur portée sert plusieurs objectifs :
- **Organiser les données :** les variables stockent des informations, ce qui rend vos scripts plus lisibles et plus faciles à gérer.
- **Améliorer la réutilisabilité :** en limitant la portée, vous pouvez réutiliser des sections de code sans conflits.
- **Encapsulation :** une portée localisée évite les interactions involontaires entre variables dans différentes parties du script.
- **Simplifier la logique :** des variables temporaires dans une portée limitée réduisent la complexité des calculs ou des flux de travail plus importants.

### Types de définitions de variables et de portée

Scheme propose plusieurs constructions pour définir et limiter la portée des variables :
- **`let` :** crée des liaisons locales pour des variables dans un bloc de code précis.
- **`let*` :** variante séquentielle de `let`, où chaque liaison peut dépendre des précédentes.
- **Named `let` :** construction puissante pour définir des procédures locales récursives ou des boucles.
- **`define` :** crée des variables globales ou des fonctions accessibles dans tout le script.

### Comment fonctionnent les définitions de variables et la portée

Les définitions de variables et la portée impliquent généralement :
1. **Déclarer des variables :** assigner une valeur à une variable dans un contexte donné.
2. **Limiter la portée :** contrôler où la variable est accessible (par exemple dans un bloc `let` ou globalement).
3. **Utiliser des variables :** accéder aux valeurs et les modifier pour effectuer des calculs, de la logique ou des opérations procédurales.

### Exemple : utiliser `let` pour des variables locales

La construction `let` permet de définir des variables temporaires disponibles uniquement dans un bloc précis :

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Cet exemple déclare `x` et `y` avec des valeurs locales et calcule leur somme.

### Exemple : utiliser `define` pour des variables globales

La construction `define` crée des variables ou des fonctions à portée globale :

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Ce script définit une constante globale `pi` et une fonction `circle-area` qui l'utilise.

### Comparaison de portée : local vs global

| Caractéristique | Portée locale (`let`, `let*`) | Portée globale (`define`) |
|------------------|------------------------------------------|-----------------------------------------------|
| **Accessibilité** | Limitée au bloc où elle est définie | Accessible dans tout le script |
| **Encapsulation** | Évite les interactions involontaires | Peut entrer en conflit avec d'autres variables globales |
| **Cas d'utilisation** | Variables temporaires pour des tâches précises | Variables ou fonctions partagées dans tout le script |

### Résumé

- Les **définitions de variables et la portée** sont fondamentales pour organiser et gérer les données dans vos scripts Scheme.
- Utilisez la **portée locale** (`let`, `let*`, named `let`) pour encapsuler des variables temporaires et éviter les conflits.
- Utilisez la **portée globale** (`define`) pour des fonctions réutilisables ou des constantes partagées dans tout le script.
- Une bonne compréhension de ces constructions améliore la lisibilité, la maintenabilité et la fiabilité de votre code.
