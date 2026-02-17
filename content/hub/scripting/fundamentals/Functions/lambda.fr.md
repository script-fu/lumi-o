---
title: "Fonctions Lambda"
type: docs
weight: 1
---
Les **fonctions Lambda** dans Scheme sont des fonctions anonymes, ce qui signifie que ce sont des fonctions sans nom. Ces fonctions sont définies en ligne et sont généralement utilisées pour des opérations courtes et ponctuelles. La construction `lambda` est un outil puissant de programmation fonctionnelle, vous permettant de créer une logique concise et flexible à la volée.

Les fonctions Lambda sont particulièrement utiles lorsque :

- Vous avez besoin d'une petite fonction dans un but précis et temporaire.
- Passer des fonctions comme arguments à des fonctions d'ordre supérieur comme `map`, `filter` ou `fold`.
- Renvoi de fonctions à partir d'autres fonctions.

### Syntaxe des fonctions Lambda

Les fonctions Lambda peuvent être définies seules...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...ou invoqué immédiatement :

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Les paramètres acceptés par la fonction.
- **`body-expression`:** La logique exécutée lorsque la fonction est appelée.
- **Invocation immédiate :** La deuxième forme montre un lambda immédiatement appelé avec des arguments.

### Exemples de fonctions Lambda

#### Utiliser Lambda pour des calculs simples

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Ici :

- Une fonction lambda est créée pour ajouter deux nombres (`x` et `y`).
- La fonction est immédiatement invoquée avec les arguments `3` et `5`.

#### Fonctions Lambda en ligne

L'exemple suivant montre comment utiliser `for-each` avec à la fois une fonction nommée et une fonction lambda :

**Utilisation d'une fonction nommée :**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Explication** :
  - `print-item` est une fonction nommée qui convertit un nombre en chaîne (`number->string`) et l'imprime en utilisant `lumi-message`.
  - `for-each` applique `print-item` à chaque élément de la liste `(1 2 3 4)`.

**Sortie** : 1 2 3 4

**Utilisation d'une fonction Lambda :**

La même logique peut être écrite en ligne avec une fonction lambda, évitant ainsi le besoin d'une fonction nommée distincte :

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Explication** :
  - Le `(lambda (x) (lumi-message (number->string x)))` définit une fonction anonyme.
  - Cette fonction est appliquée à chaque élément de la liste `(1 2 3 4)` par `for-each`.

**Sortie** : 1 2 3 4

#### Fonctions Lambda comme arguments

Les fonctions Lambda sont souvent transmises directement aux fonctions d'ordre supérieur comme `map` ou `filter`.

#### Mettre au carré une liste de nombres

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- La fonction `lambda` met au carré chaque élément de la liste.
- La fonction `map` applique le `lambda` à chaque élément.

#### Fonctions Lambda comme valeurs de retour

Vous pouvez renvoyer une fonction lambda à partir d'une autre fonction pour créer un comportement dynamique.

#### Générer une fonction Adder

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` génère une nouvelle fonction lambda qui ajoute un numéro spécifique (`n`).
- Le lambda renvoyé est stocké dans `add5`, ce qui ajoute `5` à son entrée.

#### Utilisation de Lambda avec `let`

Les lambdas sont souvent utilisés avec `let` pour créer des fonctions temporaires à portée locale.

#### Lambda local pour l'ajout

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- Le `let` lie une fonction lambda au nom `add`.
- Le lambda est ensuite utilisé comme une fonction normale dans la portée `let`.

#### Combiner des Lambdas avec des fonctions d'ordre supérieur

Les Lambda brillent lorsqu'ils sont combinés avec des fonctions d'ordre supérieur pour effectuer des transformations de données complexes.

#### Filtrage des nombres pairs

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- Le `lambda` vérifie si un nombre est pair.
- La fonction `filter` utilise le lambda pour ne conserver que les nombres pairs de la liste.

### Avantages des fonctions Lambda

- **Concision :** Les Lambdas réduisent le code passe-partout en supprimant le besoin de définir des fonctions nommées distinctes.
- **Flexibilité :** Vous pouvez les définir et les utiliser partout où ils sont nécessaires, rendant le code plus modulaire.
- **Lisibilité améliorée :** Pour les tâches courtes et spécifiques, les lambdas indiquent clairement l'intention sans encombrer le code avec des fonctions nommées supplémentaires.

### Quand utiliser les fonctions Lambda

Utilisez les fonctions lambda lorsque :

- La logique est courte et autonome.
- La fonction n'est nécessaire que temporairement ou dans un cadre spécifique.
- Vous travaillez avec des fonctions d'ordre supérieur comme `map`, `filter` ou `reduce`.

Évitez d'utiliser des lambdas pour une logique complexe sur plusieurs lignes, car cela peut réduire la lisibilité. Pour des opérations plus étendues, utilisez plutôt une fonction nommée.

### Conclusion

Les fonctions Lambda dans Scheme offrent un moyen concis et puissant de définir des fonctions anonymes pour des tâches spécifiques. Leur flexibilité et leur facilité d'utilisation en font un outil essentiel pour tout programmeur Scheme. Comprendre comment utiliser efficacement `lambda` vous aidera à écrire des scripts plus propres, plus modulaires et plus efficaces.