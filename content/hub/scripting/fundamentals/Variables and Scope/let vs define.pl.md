---
title: "Nazwany niech lub lokalny definiuje"
type: docs
weight: 5
---
Zarówno **o nazwie `let`**, jak i **lokalnie `define`** są potężnymi narzędziami w Scheme do konstruowania kodu, ale służą różnym celom. Zrozumienie, kiedy używać każdego z nich, pomaga w tworzeniu przejrzystych, modułowych i wydajnych skryptów.

### Przegląd

- **Nazwany `let`**: Konstrukcja łącząca wiązanie zmiennych i rekursję w zlokalizowanym zakresie, zwykle używana do obliczeń iteracyjnych lub rekurencyjnych.
- **Lokalny `define`**: Sposób definiowania funkcji pomocniczych lub zmiennych w zakresie funkcji otaczającej, dzięki czemu można je ponownie wykorzystać w różnych częściach tej funkcji.

---

### Nazwany `let`

#### Charakterystyka:
1. Łączy powiązania zmiennych i rekurencję w jedną konstrukcję.
2. Zakres obejmuje treść bloku `let`.
3. Idealny do **lokalnej rekurencji** lub procesów iteracyjnych specyficznych dla pojedynczego zadania.

#### Składnia
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Przykład: Sumowanie elementów listy
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Wynik**: `10`

- **Jak to działa**: Funkcja `loop` jest zdefiniowana w `let`, umożliwiając wywołania rekurencyjne ze zaktualizowanymi powiązaniami.

---

### Lokalny `define`

#### Charakterystyka:
1. Umożliwia tworzenie funkcji pomocniczych lub zmiennych, które można ponownie wykorzystać w funkcji otaczającej.
2. Zasięg obejmujący funkcję otaczającą, ale widoczny w całej treści.
3. Idealny do modularyzacji kodu z wieloma krokami lub logiką wielokrotnego użytku.

#### Składnia
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Przykład: przetwarzanie wielu wartości
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Wynik**: `41` (oblicza \(2^2 + 3^3 + 4^2\))

- **Jak to działa**: Funkcje pomocnicze `square` i `cube` można ponownie wykorzystać w ramach funkcji `process-values`, umożliwiając logikę modułową.

---

### Kluczowe różnice

| **Aspekt** | **Nazwany `let`** | **Lokalny `define`** |
|-----------------------------------|----------------------------------------------------------------|------------------------------------------------|
| **Cel** | Łączy rekurencję i iterację w sposób zlokalizowany. | Definiuje funkcje pomocnicze lub zmienne wielokrotnego użytku. |
| **Zakres** | Ograniczone do treści bloku `let`.           | Widoczne w całej funkcji otaczającej.      |
| **Ponowne użycie** | Nie nadaje się do ponownego użycia poza blokiem `let`.             | Możliwość wielokrotnego użycia w ramach tej funkcji.    |
| **Najlepszy przypadek użycia** | Zlokalizowana rekurencja lub iteracja powiązana z pojedynczym zadaniem. | Modularyzacja kodu z wieloma krokami wielokrotnego użytku. |
| **Składnia** | Łączy wiązanie i rekurencję w jedną konstrukcję.  | Jawnie definiuje funkcje lub zmienne.      |

---

### Kiedy używać nazwy `let`

1. **Logika jednorazowego użytku**: Gdy rekurencja lub iteracja są specyficzne dla pojedynczego obliczenia.
2. **Enkapsulacja**: Aby uniknąć dodawania dodatkowych nazw funkcji do przestrzeni nazw funkcji otaczającej.
3. **Iteracja**: Podczas zarządzania zmiennymi pośrednimi w konstrukcji zapętlonej.

**Przykład: obliczenia silni**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Wynik**: `120`

---

### Kiedy używać lokalnego `define`

1. **Pomocnicy wielokrotnego użytku**: Gdy logika wymaga ponownego użycia w wielu częściach funkcji.
2. **Projekt modułowy**: Aby podzielić złożone obliczenia na mniejsze, nazwane podzadania.
3. **Wiele kroków**: Gdy w różnych częściach obliczeń potrzebnych jest wiele funkcji pomocniczych.**Przykład: przetwarzanie danych wejściowych**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Wynik**: `(13 36)` (oblicza \(2^2 + 3^2\) i \(2^2 \cdot 3^2\))

---

### Łączenie deklaracji i danych wejściowych w nazwie `let`

Jedną z najpotężniejszych cech nazwanego `let` jest jego zdolność do łączenia **deklaracji zmiennych lokalnych** i **parametrów wejściowych** dla rekurencji w jedną konstrukcję. To sprawia, że ​​nazwa `let` jest zarówno zwięzła, jak i wyrazista w przypadku zadań iteracyjnych lub rekurencyjnych.

#### Deklaracja zmiennej lokalnej
W nazwanym `let` powiązania w nawiasach działają jak **zmienne lokalne**, które są inicjowane określonymi wartościami. Zakres tych zmiennych obejmuje treść `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` i `y`** to zmienne lokalne zdefiniowane i zainicjowane jako część `let`.

---

#### Parametry wejściowe dla rekurencji
Te same zmienne działają również jako **parametry wejściowe** dla rekurencyjnych wywołań nazwanego `let`. Kiedy nazwany `let` wywołuje sam siebie, aktualizuje te zmienne o nowe wartości.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Pierwsza iteracja**: `x = 1`, `y = 2`
- **Druga iteracja**: `x = 2`, `y = 4`
- **Trzecia iteracja**: `x = 3`, `y = 8` i tak dalej...

---

#### Odpowiednik przy użyciu lokalnego `define`

Nazwany `let` zawiera inicjalizację zmiennej jako część swojej składni. Eliminuje to potrzebę oddzielnego kroku w celu ustawienia wartości początkowych. Poniższe dwa przykłady są równoważne:

##### Używanie nazwy `let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Używanie lokalnego `define`
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Obydwa wykonują te same obliczenia, ale o nazwie `let` łączy deklarację zmiennej i konfigurację rekurencji w jedną zwięzłą konstrukcję.

---

#### Zalety łączenia deklaracji i danych wejściowych

1. **Zwięzłość**: Nazwany `let` redukuje szablon poprzez połączenie inicjalizacji zmiennych i rekurencji w jedną konstrukcję.
2. **Przejrzystość**: Wyjaśnia, że ​​rekurencja jest lokalna dla `let` i powiązana z konkretnym zadaniem.
3. **Enkapsulacja**: Logika rekurencyjna pozostaje samodzielna i nie zanieczyszcza przestrzeni nazw funkcji otaczającej.

Ta dwufunkcyjna natura nazwanego `let` — zarówno jako deklaracji zmiennej, jak i rekurencyjnego mechanizmu wejściowego — sprawia, że ​​jest to potężna i unikalna funkcja w programowaniu schematów.

### Podsumowanie

- Użyj **o nazwie `let`** dla **lokalnej rekurencji** lub **iteracji**, zwłaszcza gdy logika jest ściśle powiązana z pojedynczym zadaniem.
- Użyj **lokalnego `define`** do **modularyzacji kodu** z funkcjami pomocniczymi lub zmiennymi wielokrotnego użytku.

Rozumiejąc różnice między nimi, można pisać bardziej zwięzłe, zorganizowane i łatwiejsze w utrzymaniu programy w ramach schematu.