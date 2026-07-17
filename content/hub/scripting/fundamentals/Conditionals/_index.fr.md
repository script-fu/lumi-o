---
title: "Conditions"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals/_index"
---
Les conditions sont un élément fondamental de la programmation : elles permettent aux scripts de prendre des décisions et de contrôler leur flux en fonction de critères précis. En Scheme, basé sur le langage de programmation Scheme, les conditions vous aident à créer des scripts dynamiques et intelligents qui s'adaptent aux entrées, environnements ou actions utilisateur changeants.

### Le rôle des conditions en Scheme

Les conditions remplissent plusieurs fonctions essentielles dans vos scripts :
- **Diriger la logique :** Elles exécutent des morceaux de code différents selon que certaines conditions sont vraies ou fausses.
- **Plus de flexibilité :** En réagissant dynamiquement aux entrées ou aux états, elles aident votre script à gérer une variété de scénarios.
- **Simplifier la complexité :** Elles décomposent la prise de décision en structures gérables, rendant le code plus facile à lire, déboguer et maintenir.

### Types de conditions disponibles

Scheme propose plusieurs constructions conditionnelles, chacune adaptée à des besoins logiques différents :
- **`if` :** Pour des décisions binaires simples — un bloc si la condition est vraie, un autre si elle est fausse.
- **`cond` :** Une construction puissante à branchements multiples pour gérer plusieurs conditions de façon claire et structurée.
- **`and` / `or` :** Opérateurs logiques qui évaluent des combinaisons de conditions pour une prise de décision plus complexe.
- **`else` :** Un cas de repli qui définit le comportement lorsqu'aucune condition spécifiée n'est remplie.

### Comment fonctionnent les conditions

Les conditions impliquent généralement :
1. **Évaluer une condition :** Une expression de test détermine si une condition est vraie ou fausse.
2. **Exécution par branchement :** Selon l'évaluation, le script choisit le bloc de code à exécuter.
3. **Retourner une valeur (facultatif) :** Dans certains cas, les conditions produisent aussi une valeur utilisable ailleurs dans le script.