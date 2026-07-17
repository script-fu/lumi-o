---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
url: "hub/scripting/fundamentals/Iteration/map"
---
Funkcja `map` w Scheme służy do stosowania procedury do każdego elementu listy (lub wielu list) i **zwracania nowej listy** zawierającej wyniki. Idealna do transformacji danych.

Najprostsza postać `map` wygląda tak:

```scheme
(map procedure list)
```

- **Procedura:** Funkcja stosowana do każdego elementu listy.
- **Lista:** Lista, której elementy zostaną przekształcone.

---

### Przykład: podwojenie każdego elementu

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Tutaj funkcja `double` jest stosowana do każdego elementu listy `(1 2 3 4)`.
- Wynikiem jest nowa lista z podwojonymi elementami.

**Wynik**: `(2 4 6 8)`

---

### Jak to działa

1. **Tworzy nową listę:**
   - `map` stosuje podaną procedurę do każdego elementu listy i zbiera wyniki w nowej liście.

2. **Transformuje dane:**
   - Służy głównie do transformacji danych, a nie do wykonywania efektów ubocznych.

---

#### Przykład: z wieloma listami

Gdy podane są wiele list, `map` przetwarza odpowiadające sobie elementy z każdej listy.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Funkcja `sum` dodaje odpowiadające sobie elementy z dwóch list i zwraca wyniki jako nową listę.

**Wynik**: `(5 7 9)`

---

### Podsumowanie

- Funkcja `map` to potężne narzędzie do transformacji list poprzez stosowanie procedury do każdego elementu.
- W przeciwieństwie do `for-each`, `map` **tworzy nową listę** z wynikami zastosowanej procedury.
- Obsługuje wiele list, umożliwiając operacje element po elemencie między nimi.

Dzięki `map` można efektywnie tworzyć przekształcone wersje danych, pozostawiając oryginalne listy niezmienione.
