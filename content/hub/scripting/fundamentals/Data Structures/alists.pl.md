---
title: "Listy stowarzyszeń (alistów)"
type: docs
weight: 6
---
**Lista powiązań** (lub **alista**) to podstawowa struktura danych w schemacie używana do reprezentowania kolekcji par klucz-wartość. Jest zaimplementowany jako lista par, gdzie każda para kojarzy klucz (zwykle symbol) z wartością. Alisty są proste, elastyczne i dobrze nadają się do małych i średnich zbiorów danych.

### Struktura listy stowarzyszeń

Alista to lista, na której każdy element jest **parą** (skonstruowaną za pomocą `cons`). Każda para składa się z:

- **Klucz**: Pierwszy element (zazwyczaj symbol).
- **Wartość**: Drugi element, który może mieć dowolny typ danych.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Klucz**: `'name`, `'age`, `'city`
- **Wartość**: `"Alice"`, `30`, `"Paris"`
- **Struktura**: Lista par:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Tworzenie listy

Możesz utworzyć listę, ręcznie konstruując pary lub budując ją programowo za pomocą `cons`.

#### Używanie pojedynczego cudzysłowu (`'`)

Pojedynczy cudzysłów (`'`) jest skrótem od **cytowania**, co uniemożliwia Scheme ocenę wyrażenia. Dzięki temu idealnie nadaje się do tworzenia statycznych list, w których wszystkie klucze i wartości są zakodowane na stałe.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Wynik**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Korzystanie z cudzysłowu (`` ` ``) and Comma (`,`)

Operator cudzysłowu (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Jest to przydatne do tworzenia list, w których klucze lub wartości są obliczane w czasie wykonywania.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Wynik**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Przykładowe porównanie

Statyczna lista używająca `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Dynamiczny alist używający `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Dostęp do danych na liście

Aby pobrać wartość z listy, możesz użyć funkcji `assoc`, która wyszukuje parę według jej klucza.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Wyodrębnianie wartości

Po odzyskaniu pary za pomocą `assoc` użyj `cdr`, aby wyodrębnić wartość:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Podsumowanie kluczowych funkcji

- **Pojedynczy cudzysłów (`'`)**: Tworzy statyczną listę, w której wszystkie elementy są danymi dosłownymi.
- **Cytat wsteczny (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Zapis kropkowy (`.`)**: Używany do konstruowania par, wiążących klucz z wartością na liście.