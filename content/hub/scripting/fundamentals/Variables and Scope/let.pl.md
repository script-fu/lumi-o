---
title: "pozwalać"
type: docs
weight: 4
---
Nazwa `let` jest używana, ponieważ odzwierciedla matematyczne pochodzenie wprowadzenia tymczasowych powiązań, jak w _"Niech \( x = 2 \) i \( y = 3 \)"_.

Instrukcja `let` w schemacie jest **konstrukcją wiążącą** używaną do definiowania zmiennych w zlokalizowanym zakresie. Umożliwia utworzenie tymczasowych powiązań dla zmiennych, a następnie wykonanie bloku kodu przy użyciu tych powiązań. Jest to szczególnie przydatne do utrzymywania modułowości kodu i unikania zanieczyszczenia zmiennymi globalnymi.

Istnieją trzy główne formy `let` w schemacie:

- **`let`**: Standardowa możliwość tworzenia prostych powiązań lokalnych.
- **`let*`**: Let sekwencyjny, w którym powiązania mogą zależeć od wyników poprzednich powiązań.
- **Nazwany `let`**: Specjalna forma `let`, która tworzy pętle rekurencyjne lub nazwane procedury.

W najprostszej formie `let` tworzy powiązania zmiennych lokalnych i ocenia wyrażenie z tymi powiązaniami.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Wiązania**: Lista par, gdzie każda para przypisuje `value` do `variable`.
- **Wyrażenie**: Treść `let`, która może wykorzystywać zmienne zdefiniowane lokalnie.

### Przykład

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Definiuje dwie zmienne lokalne, `x` (10) i `y` (20).
- Następnie oblicza `(+ x y)` przy użyciu tych zmiennych.

**Wynik**: `30`

---

## Konstrukcja `let*`

Konstrukt `let*` jest podobny do `let`, ale powiązania są oceniane **sekwencyjnie**. Oznacza to, że późniejsze powiązania mogą zależeć od wcześniejszych.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Przykład

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- Pierwsze powiązanie przypisuje `10` do `x`.
- Drugie powiązanie oblicza `y` jako `(+ x 5)`, używając wartości `x`.
- Ciało oblicza `(* x y)`.

**Wynik**: `150`

---

## Nazwany `let`

Nazwany `let` jest specjalną formą `let`, która dostarcza nazwę samego bloku `let`, zamieniając go w procedurę rekurencyjną. Jest to przydatne do tworzenia pętli lub obliczeń rekurencyjnych.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nazwa**: Blok `let` otrzymuje nazwę, która skutecznie definiuje funkcję.
- **Wiązania**: Początkowe wartości zmiennych, podobne do standardowego `let`.
- **Treść**: Wyrażenie może wywoływać rekurencyjnie nazwane `let`.

### Przykład: pętla z Named `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- Funkcja `loop` zaczyna się od `n = 5` i `result = 1`.
- Jeśli `n` to `0`, zwraca `result`.
- W przeciwnym razie wywołuje się rekurencyjnie z `n - 1` i `result * n`.

**Wynik**: `120` (silnia 5)

---

## Tabela podsumowująca| Konstruuj | Opis | Przypadek użycia |
|------------|------------------------------------------------------|------------------------------------------------------------------------------------------------|
| **`let`** | Definiuje lokalne powiązania dla zmiennych.    | Użyj, gdy wszystkie powiązania są niezależne i nie zależą od siebie.     |
| **`let*`** | Definiuje sekwencyjne powiązania lokalne.       | Użyj, gdy późniejsze wiązania zależą od wyników wcześniejszych.           |
| **Nazwany `let`** | Definiuje rekurencyjne procedury lokalne. | Używaj pętli, obliczeń iteracyjnych lub rekurencji w kontekście lokalnym. |

---

## Przykłady

### Używanie `let` do obliczeń lokalnych

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Wynik**: `13` (oblicza `x² + y²`)

---

### Używanie `let*` dla zależności sekwencyjnych

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Wynik**: `8` (oblicza `x³`)

---

### Używanie Named `let` do obliczeń rekurencyjnych

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Wynik**: `120` (silnia 5)

---

Używając `let`, `let*` i nazwany `let`, Scheme umożliwia programowanie modułowe, rekurencyjne i sekwencyjne z jasnymi zasadami określania zakresu.