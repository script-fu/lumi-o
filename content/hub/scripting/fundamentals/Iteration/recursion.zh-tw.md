---
title: "簡單遞迴"
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
  (if (= n 0)                  ; 基準情況：當 n 為 0 時停止
    0                          ; 基準結果：總和為 0
    (+ n (sum-to-n (- n 1))))) ; 遞迴呼叫：將目前 n 與較小子問題的結果相加
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
      (lumi-message (number->string (car lst))) ; 列印第一個元素
      (print-elements (cdr lst)))))             ; 處理串列的其餘部分
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