---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
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
- **Valeur initiale :** valeur initiale.
- **Expression de mise à jour :** mise à jour par itération.
- **Condition d'arrêt :** condition d'arrêt.
- **Expression de résultat :** valeur renvoyée à l'arrêt.
- **Body :** code exécuté à chaque tour.

---

### Exemple : somme de 1 à 5

```scheme
(do ((i 1 (+ i 1))      ; Initialiser i à 1, incrémenter de 1
     (sum 0 (+ sum i))) ; Initialiser la somme à 0, ajouter i à la somme
    ((> i 5) sum)       ; Terminer quand i > 5, renvoyer sum
  (lumi-message (number->string sum))) ; Affiche la somme à chaque étape
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