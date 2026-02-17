---
title: "Wektory"
type: docs
weight: 5
---
W schemacie wektor jest kolejną podstawową strukturą danych używaną do grupowania wartości. W przeciwieństwie do list, wektory to indeksowane zbiory elementów o stałym rozmiarze, zapewniające szybszy losowy dostęp i aktualizacje. Każdy element wektora może być dowolnego typu, łącznie z innym wektorem. Wektory są reprezentowane za pomocą #, po którym następują nawiasy. `#(1 2 3)`

Chociaż wektory i listy mogą wyglądać podobnie, w programowaniu schematów służą różnym celom:

- Listy są częściej używane w operacjach rekurencyjnych i strukturach dynamicznych, ponieważ ich implementacja w postaci połączonych węzłów umożliwia efektywną manipulację ich początkiem i przejściem poprzez dekompozycję rekurencyjną.

- Z drugiej strony wektory są zoptymalizowane pod kątem scenariuszy, w których wymagany jest losowy dostęp do elementów lub aktualizacji w określonych indeksach, dzięki czemu są bardziej odpowiednie do przypadków użycia, takich jak tabele przeglądowe, konfiguracje o stałym rozmiarze lub operacje indeksowane o krytycznym znaczeniu dla wydajności.

Zasadniczo listy są naturalnym wyborem w przypadku algorytmów rekurencyjnych i danych o dynamicznie zmienianym rozmiarze, natomiast wektory wyróżniają się, gdy najważniejsze są wzorce dostępu o stałym rozmiarze lub indeksowane.

### Proste wektory

```scheme
(vector 1 2 3)
```

- Tworzy wektor składający się z trzech elementów: `1`, `2` i `3`.

Wynik: **`#(1 2 3)`**

#### Dostęp do elementów wektorowych

Dostęp do elementów wektora uzyskuje się za pomocą procedury `vector-ref`, która pobiera element o określonym indeksie (zaczynając od `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iteracja: przetwarzanie każdego elementu w wektorze

Możesz iterować po wektorze za pomocą pętli lub rekurencji. Schemat zapewnia `vector-length` w celu określenia rozmiaru wektora. Oto prosta pętla do wydrukowania każdego elementu wektora:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Przypadek podstawowy:** Jeśli indeks `i` osiągnie długość wektora, zatrzymaj pętlę.
- **Przypadek rekurencyjny:** Wydrukuj element o indeksie `i`, a następnie zwiększ `i`.

#### Przykładowe użycie

```scheme
(print-elements (vector 1 2 3))
```

Wynik:

- `"1"`
- `"2"`
- `"3"`

Wynik: „gotowe”

### Mieszane wektory

Wektory mogą zawierać elementy różnych typów, w tym ciągi znaków, wartości logiczne, liczby, inne wektory, a nawet wyniki wyrażeń:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Tworzy to wektor z:
  - Liczba (`42`)
  - Ciąg (`"hello"`)
  - Wartość logiczna (`#t`)
  - Kolejny wektor (`#(1 2)`)
  - Wynik wyrażenia (`(+ 3 4)`, którego wynikiem jest `7`)

Wynik: **`#(42 "hello" #t #(1 2) 7)`**

### Konstruowanie wektorów

Wektory są tworzone przy użyciu `vector` lub przy użyciu `make-vector` w celu utworzenia wektora o stałym rozmiarze i wartości początkowej.

```scheme
(make-vector 5 0)
```

Tworzy wektor o rozmiarze `5` ze wszystkimi elementami inicjalizowanymi na `0`.

Wynik: `#(0 0 0 0 0)`

### Aktualizacja wektorów

Procedura `vector-set!` aktualizuje element wektora o określonym indeksie.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Wynik: `#(1 42 3)`

### Sprawdzanie wektorów

Procedura `vector?` sprawdza, czy dana wartość jest wektorem.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Wynik:

- `(vector? (vector 1 2 3))` zwraca `#t` (true)
- `(vector? 42)` zwraca `#f` (fałsz)

### Wektory i zachowanie podczas przekazywania przez referencjeNa schemacie wektory można modyfikować i przekazywać przez odniesienie. Oznacza to, że gdy przekazujesz wektor do funkcji, funkcja może bezpośrednio modyfikować oryginalny wektor. Wszelkie zmiany dokonane w wektorze wewnątrz funkcji zostaną odzwierciedlone również poza funkcją. To zachowanie jest przydatne do wydajnego udostępniania i aktualizowania danych w wielu funkcjach, ale wymaga również ostrożności, aby uniknąć niezamierzonych skutków ubocznych.

#### Przykład: modyfikowanie wektora w funkcji

Oto przykład pokazujący, jak wektory są przekazywane przez referencję i modyfikowane:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Wynik: `#(10 99 30)`

#### Wyjaśnienie krok po kroku

1. **Utwórz wektor:** `my-vector` jest inicjowany wartościami `10`, `20` i `30`.
2. **Przejście do funkcji:** `my-vector` jest przekazywane do `modify-vector` wraz z indeksem i nową wartością do aktualizacji.
3. **Modyfikuj w funkcji:** Procedura `vector-set!` aktualizuje wartość pod określonym indeksem bezpośrednio w oryginalnym wektorze.
4. **Odzwierciedlaj zmiany:** Ponieważ wektory są przekazywane przez odniesienie, zmiany dokonane w funkcji są odzwierciedlane w oryginalnym wektorze.

#### Konsekwencje przekazywania przez odwołanie

- **Wydajność:** Przekazywanie wektorów przez referencje jest wydajne, ponieważ pozwala uniknąć kopiowania dużych struktur.
- **Skutki uboczne:** Zachowaj ostrożność podczas udostępniania wektorów między funkcjami, aby uniknąć niezamierzonych modyfikacji udostępnianych danych.

### Operacje na wektorach

Scheme udostępnia kilka wbudowanych procedur do pracy z wektorami, w tym:

- `vector-length`: Zwraca liczbę elementów wektora.
- `vector->list`: Konwertuje wektor na listę.
- `list->vector`: Konwertuje listę na wektor.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Wynik:

- `(vector-length (vector 1 2 3))` zwraca `3`
- `(vector->list (vector 1 2 3))` zwraca `(1 2 3)`
- `(list->vector (list 1 2 3))` zwraca `#(1 2 3)`

### Zagnieżdżone wektory

Wektory na schemacie mogą zawierać inne wektory jako elementy, tworząc strukturę zagnieżdżoną.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Tworzy wektor składający się z trzech elementów, z których każdy sam jest wektorem.

Wynik: **`#(#(1 2) #(3 4) #(5))`**

#### Dostęp do danych zagnieżdżonych

Aby uzyskać dostęp do elementów w zagnieżdżonym wektorze, użyj `vector-ref` wielokrotnie, aby poruszać się po strukturze.

#### Przykład: uzyskiwanie dostępu do elementów

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Podsumowanie

- **Wektory** na schemacie to indeksowane struktury danych o stałym rozmiarze.
- Użyj `vector`, aby utworzyć wektor, `vector-ref`, aby uzyskać dostęp do elementów i `vector-set!`, aby zaktualizować elementy.
- Wbudowane procedury, takie jak `vector-length`, `vector->list` i `list->vector` umożliwiają elastyczne operacje.
- Zagnieżdżone wektory pozwalają na tworzenie złożonych, hierarchicznych struktur danych.