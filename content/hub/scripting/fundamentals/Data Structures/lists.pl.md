---
title: "Listy"
type: docs
weight: 4
---
W schemacie **lista** jest podstawową strukturą danych używaną do grupowania wartości. Listy to uporządkowane zbiory elementów, z których każdy może być dowolnego typu, łącznie z inną listą. Listy są szeroko stosowane w schemacie zarówno do przechowywania danych, jak i struktury programu.

### Przykład 1: Prosta lista

```scheme
(list 1 2 3)
```

- Tworzy listę trzech elementów: `1`, `2` i `3`.

Wynik: **`(1 2 3)`**

---

#### Dostęp do elementów listy

Dostęp do elementów listy uzyskuje się za pomocą procedur `car` i `cdr`:

- `car` pobiera pierwszy element listy.
- `cdr` pobiera resztę listy (wszystko oprócz pierwszego elementu).

#### Przykłady

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Wynik:

- `(car my-list)` zwraca `1`
- `(cdr my-list)` zwraca `(2 3)`

---

#### Prosta rekurencja: iteracja po liście

Wywołując rekurencyjnie `car` na `cdr` listy, możesz przetwarzać każdy element jeden po drugim, aż do przejścia przez listę. Stanowi to podstawę wielu algorytmów przetwarzania list.

#### Przykład: drukowanie każdego elementu listy

Oto prosta funkcja rekurencyjna umożliwiająca wydrukowanie każdego elementu listy:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Przypadek podstawowy:** Jeśli lista jest pusta (`null? lst`), zatrzymaj rekursję.
- **Przypadek rekurencyjny:** Wydrukuj pierwszy element (`car lst`), a następnie wywołaj funkcję na pozostałej części listy (`cdr lst`).

#### Przykładowe użycie

```scheme
(print-elements (list 1 2 3))
```

Dane wyjściowe:

- `"1"`
- `"2"`
- `"3"`

Wynik: „gotowe”

---

#### Jak to działa

1. Funkcja pobiera pierwszy element listy za pomocą `car` i przetwarza go.
2. Następnie wywołuje siebie wraz z resztą listy (`cdr`).
3. Ten proces powtarza się, aż lista będzie pusta (`null? lst`).

---

### Przykład 2: Typy mieszane

Listy mogą zawierać elementy różnych typów, w tym ciągi znaków, wartości logiczne, liczby, inne listy, a nawet wyniki wyrażeń:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Spowoduje to utworzenie listy zawierającej:
  - Liczba (`42`)
  - Ciąg (`"hello"`)
  - Wartość logiczna (`#t`)
  - Kolejna lista (`(1 2)`)
  - Wynik wyrażenia (`(+ 3 4)`, którego wynikiem jest `7`)

Wynik: **`(42 "hello" #t (1 2) 7)`**

---

Te przykłady pokazują wszechstronność list w Scheme, co czyni je potężnym narzędziem do organizowania danych i manipulowania nimi.

### Konstruowanie list

Procedura `cons` służy do konstruowania nowej listy poprzez połączenie elementu z istniejącą listą.

```scheme
(cons new-element existing-list)
```

#### Przykład

```scheme
(cons 0 (list 1 2 3))
```

- Dodaje `0` na początek listy `(1 2 3)`.

Wynik: **`(0 1 2 3)`**

---

### Sprawdzanie list

Procedura `list?` sprawdza, czy dana wartość jest listą.

```scheme
(list? value)
```

#### Przykład: lista?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Wynik:

- `(list? (list 1 2 3))` zwraca `#t` (true)
- `(list? 42)` zwraca `#f` (fałsz)

---

### Operacje na listach

Scheme udostępnia kilka wbudowanych procedur pracy z listami, w tym:

- `length`: Zwraca liczbę elementów na liście.
- `append`: Łączy dwie lub więcej list w jedną.
- `reverse`: Zwraca nową listę z elementami w odwrotnej kolejności.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Wynik:

- `(length (list 1 2 3))` zwraca `3`
- `(append (list 1 2) (list 3 4))` zwraca `(1 2 3 4)`
- `(reverse (list 1 2 3))` zwraca `(3 2 1)`#### Korzystanie z `list-ref`

Procedura `list-ref` pobiera element o określonym indeksie listy (indeks liczony od zera).

```scheme
(list-ref lst index)
```

- **`lst`**: Lista, z której chcesz pobrać element.
- **`index`**: Indeks od zera wskazujący, który element ma zostać zwrócony.

##### Przykład: lista-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Wynik: `30`

---

### Listy zagnieżdżone

Listy w schemacie mogą zawierać inne listy jako elementy, tworząc zagnieżdżoną strukturę.

#### Przykład: tworzenie listy zagnieżdżonej

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Tworzy listę trzech elementów, z których każdy sam jest listą.

Wynik: **`((1 2) (3 4) (5))`**

---

#### Dostęp do danych zagnieżdżonych

Aby uzyskać dostęp do elementów na zagnieżdżonej liście, możesz użyć kombinacji `car` i `cdr`, aby poruszać się po strukturze.

#### Przykład: uzyskiwanie dostępu do elementów

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Wyjaśnienie

1. **`car nested-list`**:
   - Pobiera pierwszy element `nested-list`, którym jest `(1 2)`.

2. **`car (car nested-list)`**:
   - Pobiera pierwszy element `(1 2)`, którym jest `1`.

3. **`cdr (car nested-list)`**:
   - Pobiera resztę `(1 2)`, czyli `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Pobiera pierwszy element `(2)`, którym jest `2`.

---

#### Przykład: uzyskiwanie dostępu do elementów z innych podlist

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Takie podejście umożliwia systematyczne nawigowanie i uzyskiwanie dostępu do określonych elementów na zagnieżdżonej liście, zapewniając dużą elastyczność pracy z danymi hierarchicznymi.

### Podsumowanie

- **Listy** w schemacie to wszechstronne i niezbędne struktury danych.
- Użyj `list`, aby utworzyć listę, `car` i `cdr`, aby uzyskać dostęp do elementów, oraz `cons`, aby skonstruować listy.
- Wbudowane procedury, takie jak `length`, `append`, `reverse` i `list-ref` sprawiają, że operacje na listach są łatwe i wydajne.
- Listy można zagnieżdżać, co umożliwia tworzenie złożonych struktur danych w zaawansowanych przypadkach.