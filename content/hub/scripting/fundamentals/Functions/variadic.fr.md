---
title: "Fonctions variadiques"
type: docs
weight: 2
---
Les **fonctions variadiques** dans Scheme sont des fonctions qui acceptent un nombre variable d'arguments. Ces fonctions sont très polyvalentes et vous permettent de créer du code flexible et réutilisable. En programmation fonctionnelle, les fonctions variadiques simplifient les opérations qui nécessitent de traiter un nombre arbitraire d'entrées, comme la somme d'une liste de nombres ou la concaténation de chaînes.

Les fonctions variadiques sont particulièrement utiles lorsque :

- Le nombre d'arguments ne peut être déterminé à l'avance.
- Vous devez appliquer la même opération à une liste dynamique d'entrées.
- Ecriture d'utilitaires d'agrégation ou de transformation de données.

### Syntaxe des fonctions variadiques

Les fonctions variadiques sont définies à l'aide du symbole `.` avant le nom du dernier paramètre. Ce dernier paramètre rassemble tous les arguments restants dans une liste.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Tous les arguments obligatoires et fixes acceptés par la fonction.
- **`variadic-parameter`:** Un paramètre spécial précédé de `.` qui collecte des arguments supplémentaires sous forme de liste.
- **`body-expression`:** La logique exécutée lorsque la fonction est appelée.

### Exemples de fonctions variadiques

#### Fonction variadique de base

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Explication** :
  - `numbers` rassemble tous les arguments dans une liste.
  - `apply` applique la fonction `+` à tous les éléments de la liste.

**Utilisation** :
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Fonction variadique à paramètres fixes

Vous pouvez combiner des paramètres fixes avec un paramètre variadique pour créer des fonctions plus flexibles.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Explication** :
  - `prefix` est un argument fixe.
  - `names` collecte les arguments restants dans une liste.
  - Chaque nom est préfixé par la chaîne donnée en utilisant `map` et `lambda`.

**Utilisation** :
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Combiner la logique fixe et la logique variadique

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Explication** :
  - `collection-name` est un paramètre fixe.
  - `items` collecte des arguments supplémentaires dans une liste.
  - La fonction concatène le nom de la collection et les éléments en une seule chaîne.

**Utilisation** :
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Cas d'utilisation avancés

#### Traitement des entrées arbitraires

Les fonctions variadiques excellent dans la gestion de données arbitraires. Voici un exemple pour additionner uniquement les nombres positifs :

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtre les nombres non positifs avant de les additionner.

**Utilisation** :
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Fonctions variadiques avec logique récursive

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Explication** :
  - `first` gère le premier argument.
  - `rest` collecte les arguments restants dans une liste.
  - Calcule récursivement la valeur maximale.

**Utilisation** :
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Avantages des fonctions variadiques

- **Flexibilité :** Ils gèrent un large éventail de cas de saisie.
- **Concision :** Réduisez le besoin de plusieurs fonctions surchargées.
- **Opérations dynamiques :** Activez le traitement des données d'exécution sans connaître au préalable le nombre d'arguments.

### Quand utiliser les fonctions variadiques

Utilisez des fonctions variadiques lorsque :

- La fonction doit traiter un nombre inconnu d'arguments.
- Une seule opération s'applique à toutes les entrées (par exemple, sommation, concaténation ou mappage).
- Simplification de la logique d'ordre supérieur avec des arguments dynamiques.

Évitez les fonctions variadiques lorsque :

- La validation des entrées ou la vérification de type est complexe.
- Des arguments fixes suffisent pour la logique requise.
- La lisibilité est compromise en raison d'opérations trop complexes.

### ConclusionLes fonctions variadiques de Scheme fournissent un mécanisme robuste pour gérer les entrées dynamiques. En comprenant leur syntaxe et leur utilisation, vous pouvez créer des scripts flexibles et puissants qui s'adaptent à divers scénarios. Combinées à des fonctions d'ordre supérieur, les fonctions variadiques rendent votre code plus concis et expressif.