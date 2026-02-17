---
title: "Symboles"
type: docs
weight: 6
---
Les symboles sont l'un des principaux types de données de Scheme, représentant des identifiants uniques et immuables. Ils sont principalement utilisés comme clés, marqueurs ou espaces réservés dans les programmes, ce qui les rend essentiels pour écrire du code propre et expressif.

Un symbole dans Scheme est similaire à une chaîne mais diffère en ce que les symboles sont **uniques** et **atomiques**. Cela signifie que deux symboles portant le même nom sont garantis comme étant le même objet, ce qui permet des contrôles d'égalité rapides et une utilisation efficace dans les structures de données.

### Syntaxe

Un symbole s'écrit sous la forme d'une séquence de caractères :

- Commence par une lettre, suivie de lettres, de chiffres ou de caractères spéciaux comme `-`, `+` ou `*`.
- Les symboles sont sensibles à la casse par défaut.

Exemples :

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## Création de symboles

Les symboles sont généralement créés à l'aide de l'opérateur **quote** (`'`), qui indique à Scheme de traiter le nom comme un symbole plutôt que de l'évaluer comme une variable ou une fonction.

### Exemple

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

Vous pouvez également créer des symboles par programme à l'aide de la procédure `string->symbol`, qui convertit une chaîne en symbole.

```scheme
(string->symbol "dynamic-symbol")
```

**Résultat** : `'dynamic-symbol`


## Comparaison des symboles

Les symboles étant uniques, vous pouvez les comparer efficacement à l'aide de `eq?`.

### Exemple

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

Cela rend les symboles idéaux pour être utilisés comme clés dans les structures de données ou comme marqueurs dans votre code.

## Utiliser des symboles

Les symboles sont souvent utilisés dans Scheme pour :

1. **Clés dans les listes d'associations :**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **Identifiants dans le code :**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procédures pour travailler avec des symboles

Scheme fournit des procédures intégrées pour travailler avec des symboles :

| Procédure | Descriptif |
|------------------------|--------------------------------------------------------------------------------------------|
| **`symbol?`** | Vérifie si un objet est un symbole.                                            |
| **`eq?`** | Compare deux symboles pour l'identité (comparaison rapide).                       |
| **`symbol->string`** | Convertit un symbole en chaîne (utile pour l'affichage ou le débogage).          |
| **`string->symbol`** | Convertit une chaîne en symbole (utile pour la création dynamique d'identifiants). |

### Exemples

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## Résumé

Les symboles constituent un moyen léger et efficace de représenter des identifiants, des clés et des marqueurs dans Scheme. Leur immuabilité et leurs contrôles d'identité rapides les rendent idéaux pour de nombreuses tâches de programmation. Comprendre comment utiliser efficacement les symboles améliorera votre capacité à écrire du code Scheme propre et expressif.