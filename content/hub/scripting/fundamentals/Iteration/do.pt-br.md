---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db8c12b44717a78fddabba563fc62d081db9644b8a1f2b09d74db91eec84bfd1
---
La fonction `do` en Scheme est une boucle avec initialisation, mise à jour et condition d'arrêt. Utile pour exécuter une séquence un nombre défini de fois ou jusqu'à une condition.

La forme générale de `do` :

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable :** variable(s) de boucle.
- **Initial-value :** valeur initiale.
- **Update-expression :** mise à jour par itération.
- **Termination-condition :** condition d'arrêt.
- **Result-expression :** valeur renvoyée à l'arrêt.
- **Body :** code exécuté à chaque tour.

---

### Exemple : somme de 1 à 5

```scheme
(do ((i 1 (+ i 1))      ; Inicializar i em 1, incrementar de 1
     (sum 0 (+ sum i))) ; Inicializar sum em 0, adicionar i a sum
    ((> i 5) sum)       ; Terminar quando i > 5, retornar sum
  (lumi-message (number->string sum))) ; Imprime a soma a cada passo
```

- `i` commence à 1 et s'incrémente.
- `sum` accumule la somme.
- Arrêt quand `i > 5`, retour de `sum`.

**Sortie** : `15`

---

### Comment ça marche

1. **Initialisation :** valeurs de départ.
2. **Test d'arrêt :** au début de chaque tour.
3. **Itération :** exécuter le corps, mettre à jour les variables.

---

### Résumé

- `do` offre des boucles flexibles à plusieurs variables.
- Utile quand l'état évolue à chaque tour.
- La condition d'arrêt fixe la fin et le résultat.

`do` combine **liaisons** (comme `let`) et **contrôle itératif**.