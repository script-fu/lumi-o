---
title: "Récursion simple"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
En Scheme, la récursion signifie qu'une fonction s'appelle elle-même pour résoudre des sous-problèmes. Une **récursion simple** a un cas de base pour s'arrêter et un cas récursif qui réduit le problème.

Structure générale :

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Base Condition :** arrête la récursion.
- **Base Result :** valeur au cas de base.
- **Recursive Call :** appel avec arguments réduits.

---

### Exemple : somme de 1 à n

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Cas de base : arrêter lorsque n vaut 0
    0                          ; Résultat de base : la somme est 0
    (+ n (sum-to-n (- n 1))))) ; Appel récursif : additionner le n actuel au résultat du sous-problème plus petit
```

#### Décomposer et recomposer

La récursion décompose le problème ; chaque appel traite une partie. Au cas de base, le résultat se recompose.

#### Pas à pas : sum-to-n 3

1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*
2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*
3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*
4. *sum-to-n 0* → *0*

#### Recomposer le résultat

1. *sum-to-n 0* → *0*
2. *sum-to-n 1* → *1*
3. *sum-to-n 2* → *3*
4. *sum-to-n 3* → *6*

---

### Exemple : afficher chaque élément

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Affiche le premier élément
      (print-elements (cdr lst)))))             ; Traite le reste de la liste
```

- **Cas de base :** liste vide → `"done"`.
- **Récursif :** afficher `car`, traiter le reste avec `cdr`.

#### Utilisation

```scheme
(print-elements (list 1 2 3))
```

Sortie : *"1"*, *"2"*, *"3"* — résultat : *"done"*

---

#### Comment ça marche

1. La fonction récupère le premier élément de la liste avec *car* et le traite.
2. Elle s'appelle ensuite avec le reste de la liste (*cdr*).
3. Le processus se répète jusqu'à ce que la liste soit vide (*null? lst*).

---

### Résumé

- Cas de base pour arrêter ; cas récursif pour réduire.
- Chaque appel progresse vers le cas de base.
- Toujours un cas de base — sinon récursion infinie.