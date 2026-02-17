---
title: "Funkcje wariadyczne"
type: docs
weight: 2
---
**Funkcje wariadyczne** w schemacie to funkcje, które akceptują zmienną liczbę argumentów. Funkcje te są bardzo wszechstronne i pozwalają na tworzenie elastycznego kodu wielokrotnego użytku. W programowaniu funkcjonalnym funkcje wariadyczne upraszczają operacje wymagające przetworzenia dowolnej liczby danych wejściowych, takie jak sumowanie listy liczb lub łączenie ciągów.

Funkcje wariadyczne są szczególnie przydatne, gdy:

- Nie można z góry określić liczby argumentów.
- Musisz zastosować tę samą operację do dynamicznej listy wejść.
- Pisanie narzędzi do agregacji lub transformacji danych.

### Składnia funkcji wariadycznych

Funkcje wariadyczne definiuje się za pomocą symbolu `.` przed nazwą ostatniego parametru. Ten ostatni parametr zbiera wszystkie pozostałe argumenty w listę.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Wszelkie wymagane, stałe argumenty akceptowane przez funkcję.
- **`variadic-parameter`:** Specjalny parametr poprzedzony `.`, który zbiera dodatkowe argumenty w postaci listy.
- **`body-expression`:** Logika wykonywana w momencie wywołania funkcji.

### Przykłady funkcji wariadycznych

#### Podstawowa funkcja wariadyczna

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Wyjaśnienie**:
  - `numbers` zbiera wszystkie argumenty w listę.
  - `apply` stosuje funkcję `+` do wszystkich elementów listy.

**Wykorzystanie**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Funkcja variadyczna ze stałymi parametrami

Możesz łączyć stałe parametry z parametrem variadic, aby utworzyć bardziej elastyczne funkcje.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Wyjaśnienie**:
  - `prefix` jest stałym argumentem.
  - `names` zbiera pozostałe argumenty w listę.
  - Każda nazwa jest poprzedzona podanym ciągiem znaków `map` i `lambda`.

**Wykorzystanie**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Łączenie logiki stałej i wariadycznej

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Wyjaśnienie**:
  - `collection-name` jest parametrem stałym.
  - `items` zbiera dodatkowe argumenty w listę.
  - Funkcja łączy nazwę kolekcji i elementy w jeden ciąg.

**Wykorzystanie**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Zaawansowane przypadki użycia

#### Przetwarzanie dowolnych danych wejściowych

Funkcje wariadyczne doskonale radzą sobie z obsługą dowolnych danych. Oto przykład sumowania tylko liczb dodatnich:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtruje liczby niedodatnie przed sumowaniem.

**Wykorzystanie**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Funkcje wariadyczne z logiką rekurencyjną

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Wyjaśnienie**:
  - `first` obsługuje pierwszy argument.
  - `rest` zbiera pozostałe argumenty na listę.
  - Rekurencyjnie oblicza wartość maksymalną.

**Wykorzystanie**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Korzyści z funkcji wariadycznych

- **Elastyczność:** obsługują szeroką gamę przypadków wejściowych.
- **Zwięzłość:** Zmniejsza potrzebę korzystania z wielu przeciążonych funkcji.
- **Operacje dynamiczne:** Włącz przetwarzanie danych w czasie wykonywania bez wcześniejszej znajomości liczby argumentów.

### Kiedy używać funkcji wariadycznych

Używaj funkcji variadic, gdy:

- Funkcja musi przetworzyć nieznaną liczbę argumentów.
- Pojedyncza operacja dotyczy wszystkich danych wejściowych (np. sumowanie, łączenie lub mapowanie).
- Uproszczenie logiki wyższego rzędu za pomocą dynamicznych argumentów.

Unikaj funkcji variadic, gdy:

- Walidacja danych wejściowych lub sprawdzanie typu jest złożone.
- Stałe argumenty wystarczą dla wymaganej logiki.
- Czytelność jest ograniczona z powodu zbyt skomplikowanych operacji.

### WniosekFunkcje wariadyczne w schemacie zapewniają solidny mechanizm obsługi dynamicznych danych wejściowych. Rozumiejąc ich składnię i zastosowanie, możesz tworzyć elastyczne i wydajne skrypty, które dostosowują się do różnych scenariuszy. W połączeniu z funkcjami wyższego rzędu funkcje variadic sprawiają, że Twój kod jest bardziej zwięzły i wyrazisty.