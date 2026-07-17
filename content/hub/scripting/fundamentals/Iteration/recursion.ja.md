---
title: "単純な再帰"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47fd79f37d5542e30722efaf4f87cd10efb77d825101f2045b191e3640137168
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
  (if (= n 0)                  ; 基底ケース: n が 0 のとき終了
    0                          ; 基本結果: 合計は 0
    (+ n (sum-to-n (- n 1))))) ; 再帰呼び出し: 現在の n をより小さな部分問題の結果と合計する
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
      (lumi-message (number->string (car lst))) ; 最初の要素を表示
      (print-elements (cdr lst)))))             ; リストの残りを処理
```

- **Cas de base :** liste vide → `"done"`.
- **Récursif :** afficher `car`, traiter le reste avec `cdr`.

#### Utilisation

```scheme
(print-elements (list 1 2 3))
```

Sortie : *"1"*, *"2"*, *"3"* — résultat : *"done"*

### Résumé

- Cas de base pour arrêter ; cas récursif pour réduire.
- Chaque appel progresse vers le cas de base.
- Toujours un cas de base — sinon récursion infinie.