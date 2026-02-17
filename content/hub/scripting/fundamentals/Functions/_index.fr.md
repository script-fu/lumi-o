---
title: "Fonctions"
type: docs
weight: 7
---
Les fonctions sont un concept central de Scheme, fournissant les moyens d'encapsuler la logique, de permettre la réutilisation du code et de structurer efficacement vos scripts. Avec les fonctions, vous pouvez créer des scripts modulaires et maintenables qui gèrent un large éventail de tâches, des opérations de base aux flux de travail avancés dans Lumi.

Cette section sert d'introduction aux fonctions de Scheme et jette les bases pour comprendre leurs types, définitions et utilisations. Les sections suivantes approfondiront les types de fonctions spécifiques et leurs capacités uniques.

## Syntaxe et expressions minimales

Le code du schéma est composé d'**expressions**. Une expression est évaluée à une valeur. La syntaxe est uniforme : les parenthèses forment un appel, avec le nom de l'opérateur ou de la fonction en premier.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Parce que tout est une expression, le flux de contrôle s’inscrit naturellement dans le même style que les appels de fonction.

## Pourquoi les fonctions sont importantes

Les fonctions jouent un rôle central dans Scheme pour plusieurs raisons :

- **Réutilisabilité du code :** Évitez les répétitions en encapsulant la logique dans des composants réutilisables.
- **Modularité :** Décomposez les tâches complexes en éléments plus petits et gérables.
- **Comportement dynamique :** Acceptez les paramètres pour gérer diverses entrées ou vous adapter à différentes situations.
- **Abstraction supérieure :** Simplifiez la logique en vous concentrant sur "ce que" une fonction fait plutôt que sur "comment" elle le fait.

## Aperçu des types de fonctions

Scheme propose une variété de constructions de fonctions, chacune adaptée à des cas d'utilisation spécifiques :

1. **Fonctions nommées**
   Ce sont des fonctions standards définies avec `define`. Ils constituent l’épine dorsale de la plupart des scripts.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Fonctions anonymes**
   Également connues sous le nom de **fonctions lambda**, il s'agit de fonctions sans nom définies en ligne pour une utilisation ponctuelle.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Fonctions d'ordre supérieur**
   Fonctions qui prennent d'autres fonctions comme arguments ou renvoient des fonctions comme résultats, permettant des abstractions puissantes telles que le mappage, le filtrage et la réduction.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Syntaxe générale des fonctions

Les fonctions dans Scheme ont une syntaxe simple et cohérente :

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Le nom de la fonction.
- **`parameter1, parameter2, ...`:** Les arguments pris par la fonction.
- **`body-expression`:** La logique exécutée lorsque la fonction est appelée.

Exemple :

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Effets secondaires et état global

Dans Lumi, de nombreuses procédures utiles ont des **effets secondaires** : elles modifient une image, changent un dessin, écrivent un fichier ou affichent une sortie.

- Isoler les effets secondaires dans de petites procédures clairement nommées.
- Évitez de modifier le contexte global, sauf si cela est nécessaire.
- Lorsque vous changez de contexte (couleurs, pinceaux, etc.), enveloppez le travail avec `lumi-context-push` et `lumi-context-pop` afin que l'état de l'utilisateur soit restauré.