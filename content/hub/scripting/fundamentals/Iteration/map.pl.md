---
title: "mapa"
type: docs
weight: 3
---
Funkcja `map` w Scheme służy do zastosowania procedury do każdego elementu listy (lub wielu list) i **zwrócenia nowej listy** zawierającej wyniki. Dzięki temu idealnie nadaje się do przekształcania danych.

Najprostsza forma `map` wygląda następująco:

```scheme
(map procedure list)
```

- **Procedura**: Funkcja do zastosowania do każdego elementu listy.
- **Lista**: Lista, której elementy zostaną przekształcone.

---

### Przykład: podwój każdy element

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Tutaj funkcja `double` jest stosowana do każdego elementu listy `(1 2 3 4)`.
- Rezultatem jest nowa lista z podwojonym każdym elementem.

**Wyjście**: `(2 4 6 8)`

---

### Jak to działa

1. **Tworzy nową listę**:
   - `map` stosuje podaną procedurę do każdego elementu listy i zbiera wyniki w nową listę.

2. **Przekształca dane**:
   - Służy głównie do transformacji danych, a nie do wywoływania efektów ubocznych.

---

#### Przykład: użycie z wieloma listami

Jeśli dostępnych jest wiele list, `map` przetwarza odpowiednie elementy z każdej listy.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Funkcja `sum` dodaje odpowiednie elementy z obu list i zwraca wyniki w postaci nowej listy.

**Wyjście**: `(5 7 9)`

---

### Podsumowanie

- Funkcja `map` to potężne narzędzie do przekształcania list poprzez zastosowanie procedury do każdego elementu.
- W przeciwieństwie do `for-each`, `map` **tworzy nową listę** zawierającą wyniki zastosowania procedury.
- Obsługuje wiele list, umożliwiając operacje na nich na elementach.

Używając `map`, możesz efektywnie tworzyć przekształcone wersje swoich danych, zachowując jednocześnie oryginalne listy.