---
title: "Listy asocjacyjne (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
---
**Lista powiązań** (lub **alista**) to podstawowa struktura danych w Scheme używana do reprezentowania kolekcji par klucz-wartość. Jest zaimplementowana jako lista par, gdzie każda para kojarzy klucz (zwykle symbol) z wartością. Alisty są proste, elastyczne i dobrze nadają się do małych i średnich zbiorów danych.

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
;; Ręczne zdefiniowanie alisty
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programowe dodanie nowej pary
(define updated-alist (cons '(country . "France") alist))
```

**Wynik**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Korzystanie z cudzysłowu (`` ` ``) i przecinek (`,`)

Operator cudzysłowu (`` ` ``) jest podobny do pojedynczego cudzysłowu, ale pozwala dynamicznie wstawiać obliczone wyrażenia za pomocą przecinka (`,`). Jest to przydatne do tworzenia list, w których klucze lub wartości są obliczane w czasie wykonywania.

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

Dynamiczny alist używający `` ` `` i `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Dostęp do danych na liście

Aby pobrać wartość z listy, możesz użyć funkcji `assoc`, która wyszukuje parę według jej klucza.

```scheme
(assoc 'name alist)   ; Zwraca (name . "Alice")
(assoc 'country alist) ; Zwraca #f (klucz nie znaleziony)
```

### Wyodrębnianie wartości

Po odzyskaniu pary za pomocą `assoc` użyj `cdr`, aby wyodrębnić wartość:

```scheme
(cdr (assoc 'name alist))   ; Zwraca "Alice"
```

### Podsumowanie kluczowych funkcji

- **Pojedynczy cudzysłów (`'`)**: Tworzy statyczną listę, w której wszystkie elementy są danymi dosłownymi.
- **Cytat wsteczny (`` ` ``)**: Umożliwia dynamiczne tworzenie alist poprzez łączenie elementów statycznych z ewaluowanymi wyrażeniami (przy użyciu `,`).
- **Zapis kropkowy (`.`)**: Używany do konstruowania par, wiążących klucz z wartością na liście.