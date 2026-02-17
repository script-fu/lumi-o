---
title: "Récursion simple"
type: docs
weight: 5
---
La récursion est un concept puissant dans Scheme, où une fonction s'appelle pour résoudre des sous-problèmes plus petits du problème d'origine. Un modèle **simple de récursion** implique un cas de base pour arrêter la récursion et un cas récursif pour réduire le problème.

La structure générale d'une fonction récursive ressemble à ceci :

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condition de base** : arrête la récursivité.
- **Résultat de base** : valeur renvoyée lorsque la condition de base est remplie.
- **Appel récursif** : un appel à la fonction elle-même avec des arguments modifiés qui rapprochent le calcul du cas de base.

---

### Exemple : Somme de nombres (1 à n)

Une fonction récursive simple pour calculer la somme des nombres de 1 à n :

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Comment ça marche : démontage et remontage

La récursivité fonctionne en décomposant le problème d'origine en morceaux plus petits. Chaque appel à la fonction gère une pièce et transmet le reste. Une fois le cas le plus simple atteint, les résultats sont réassemblés au fur et à mesure de la fin du calcul.

#### Trace étape par étape de la somme à n 3

1. **Appel initial** : *somme à n 3*
   → *(+ 3 (somme à n 2))*

2. **Deuxième appel** : *somme à n 2*
   → *(+ 2 (somme à n 1))*

3. **Troisième appel** : *somme à n 1*
   → *(+ 1 (somme à n 0))*

4. **Cas de base** : *somme à n 0*
   → *0*

---

#### Réassembler le résultat final

Une fois le cas le plus simple résolu, chaque couche du calcul complète :

1. *somme à n 0* donne *0*
2. *somme à n 1* devient *(+ 1 0) = 1*
3. *somme-à-n 2* devient *(+ 2 1) = 3*
4. *somme-à-n 3* devient *(+ 3 3) = 6*

---

### Exemple : Impression de chaque élément d'une liste

Voici une fonction récursive simple pour imprimer chaque élément d'une liste :

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Cas de base :** Si la liste est vide (*null ? lst*), arrêtez la récursivité.
- **Cas récursif :** Imprime le premier élément (*car lst*), puis appelle la fonction sur le reste de la liste (*cdr lst*).

#### Exemple d'utilisation

```scheme
(print-elements (list 1 2 3))
```

Sortie :

- *"1"*
- *"2"*
- *"3"*

Résultat : *"fait"*

---

#### Comment ça marche

1. La fonction récupère le premier élément de la liste en utilisant *car* et le traite.
2. Il s'appelle ensuite avec le reste de la liste (*cdr*).
3. Ce processus se répète jusqu'à ce que la liste soit vide (*null ? lst*).

---

### Résumé

- La récursivité simple consiste à :
  1. **Cas de base** : arrête la récursion.
  2. **Cas récursif** : réduit le problème par rapport au cas de base.
- Chaque appel récursif fait progresser le calcul vers son achèvement.
- Une fois le cas de base atteint, les résultats sont combinés au fur et à mesure que la récursion se termine.

La récursion reflète la structure du problème et fournit un flux clair et logique. Assurez-vous toujours d’avoir un cas de base pour éviter une récursion infinie.