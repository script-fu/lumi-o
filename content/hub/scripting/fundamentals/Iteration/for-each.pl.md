---
title: "dla każdego"
type: docs
weight: 5
---
Funkcja `for-each` w schemacie służy do zastosowania procedury do każdego elementu listy (lub wielu list). W przeciwieństwie do `map`, które zwraca nową listę z wynikami, `for-each` jest używane ze względu na **efekty uboczne**, takie jak drukowanie lub aktualizacja zmiennych.

Najprostsza forma `for-each` wygląda następująco:

```scheme
(for-each procedure list)
```

- **Procedura**: Funkcja do zastosowania do każdego elementu listy.
- **Lista**: Lista, której elementy będą przetwarzane.

---

### Przykład: Drukuj listę

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Tutaj funkcja `print-item` jest stosowana do każdego elementu listy `(1 2 3 4)`.
- Powoduje to, że każdy numer będzie drukowany sekwencyjnie.

**Wyjście**: `1 2 3 4`

---

### Jak to działa

1. **Iteruje po każdym elemencie**:
   - Podana procedura jest wykonywana dla każdego elementu listy w kolejności.

2. **Występuje skutki uboczne**:
   - Częste skutki uboczne obejmują drukowanie, rejestrowanie lub modyfikowanie zmiennych zewnętrznych. W przeciwieństwie do `map`, `for-each` nie zwraca nowej listy.

---

#### Przykład: użycie z wieloma listami

Jeśli dostępnych jest wiele list, `for-each` przetwarza odpowiednie elementy z każdej listy.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Funkcja `sum-and-print` sumuje odpowiednie elementy z dwóch list i drukuje wyniki.

**Wyjście**: `5 7 9`

---

### Podsumowanie

- Funkcja `for-each` jest przydatna do wykonywania efektów ubocznych na każdym elemencie listy.
- W przeciwieństwie do `map`, `for-each` nie tworzy nowej listy — skupia się wyłącznie na skutkach ubocznych procedury.
- Może obsługiwać wiele list jednocześnie, stosując procedurę do odpowiednich elementów.

Używając `for-each`, możesz skutecznie przetwarzać listy, gdy celem jest wykonanie akcji, a nie przekształcanie danych.