---
title: "faire"
type: docs
weight: 5
---
La fonction `do` dans Scheme est un mécanisme de boucle qui permet une itération avec des conditions d'initialisation, de mise à jour et de terminaison. Ceci est particulièrement utile lorsque vous devez effectuer une séquence d’opérations un nombre spécifique de fois ou jusqu’à ce qu’une condition soit remplie.

La forme générale de `do` est :

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable** : La ou les variables de la boucle.
- **Valeur initiale** : La valeur de départ de chaque variable de boucle.
- **Update-expression** : L'expression pour mettre à jour la ou les variables de boucle à la fin de chaque itération.
- **Termination-condition** : La condition pour arrêter la boucle.
- **Result-expression** : La valeur à renvoyer lorsque la boucle se termine.
- **Body** : Le code à exécuter à chaque itération.

---

### Exemple : Additionnez les nombres de 1 à 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- La variable de boucle `i` commence à 1 et s'incrémente de 1 à chaque itération.
- La variable `sum` cumule la somme de `i`.
- La boucle se termine lorsque `i > 5`, renvoyant la valeur finale de `sum`.

**Sortie** : `15`

---

### Comment ça marche

1. **Initialisation** :
   - Chaque variable de boucle se voit attribuer sa valeur initiale.

2. **Contrôle de résiliation** :
   - Au début de chaque itération, la condition de terminaison est vérifiée. Si c'est vrai, la boucle s'arrête et l'expression du résultat est évaluée.

3. **Itération** :
   - Si la condition de terminaison est fausse, le corps est exécuté et les variables de boucle sont mises à jour à l'aide de leurs expressions de mise à jour respectives.

---

### Résumé

- La construction `do` fournit un moyen flexible d'implémenter des boucles avec plusieurs variables et des conditions de terminaison complexes.
- Il est utile pour les tâches qui nécessitent des mises à jour d'état au fil des itérations.
- La condition de terminaison détermine quand la boucle se termine et peut renvoyer un résultat final.

En utilisant `do`, vous pouvez implémenter des algorithmes itératifs dans Scheme avec un contrôle précis sur l'initialisation, les mises à jour et la terminaison. Cela fait de `do` une combinaison d'un **mécanisme de liaison étendu** (comme `let`) et d'une **structure de contrôle itérative**, lui permettant de gérer les boucles et les états temporaires de manière propre et concise.