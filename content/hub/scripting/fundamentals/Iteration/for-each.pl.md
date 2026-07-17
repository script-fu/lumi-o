---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
url: "hub/scripting/fundamentals/Iteration/for-each"
---
Funkcja `for-each` w Scheme służy do stosowania procedury do każdego elementu listy (lub wielu list). W przeciwieństwie do `map`, która zwraca nową listę z wynikami, `for-each` służy do **efektów ubocznych**, takich jak wypisywanie lub aktualizacja zmiennych.

Najprostsza postać `for-each` wygląda tak:

```scheme
(for-each procedure list)
```

- **Procedura:** Funkcja stosowana do każdego elementu listy.
- **Lista:** Lista, której elementy zostaną przetworzone.

---

### Przykład: wypisanie listy

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Tutaj funkcja `print-item` jest stosowana do każdego elementu listy `(1 2 3 4)`.
- Każda liczba jest wypisywana po kolei.

**Wynik**: `1 2 3 4`

---

### Jak to działa

1. **Iteruje po każdym elemencie:**
   - Podana procedura jest wykonywana dla każdego elementu listy, w kolejności.

2. **Wykonuje efekty uboczne:**
   - Typowe efekty uboczne to wypisywanie, logowanie lub modyfikacja zmiennych zewnętrznych. W przeciwieństwie do `map`, `for-each` nie zwraca nowej listy.

---

#### Przykład: z wieloma listami

Gdy podane są wiele list, `for-each` przetwarza odpowiadające sobie elementy z każdej listy.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Funkcja `sum-and-print` sumuje odpowiadające sobie elementy z dwóch list i wypisuje wyniki.

**Wynik**: `5 7 9`

---

### Podsumowanie

- Funkcja `for-each` jest przydatna do wykonywania efektów ubocznych na każdym elemencie listy.
- W przeciwieństwie do `map`, `for-each` nie tworzy nowej listy — koncentruje się wyłącznie na efektach ubocznych procedury.
- Może obsługiwać wiele list jednocześnie, stosując procedurę do odpowiadających sobie elementów.

Dzięki `for-each` można skutecznie przetwarzać listy, gdy celem jest wykonywanie działań, a nie transformacja danych.
