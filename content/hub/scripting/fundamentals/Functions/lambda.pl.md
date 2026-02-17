---
title: "Funkcje lambdy"
type: docs
weight: 1
---
**Funkcje Lambda** w Scheme są funkcjami anonimowymi, co oznacza, że ​​są to funkcje bez nazwy. Funkcje te są definiowane inline i są zwykle używane do krótkich, jednorazowych operacji. Konstrukt `lambda` to potężne narzędzie w programowaniu funkcjonalnym, pozwalające na tworzenie zwięzłej i elastycznej logiki na bieżąco.

Funkcje lambda są szczególnie przydatne, gdy:

- Potrzebujesz małej funkcji do konkretnego, tymczasowego celu.
- Przekazywanie funkcji jako argumentów do funkcji wyższego rzędu, takich jak `map`, `filter` lub `fold`.
- Zwracanie funkcji z innych funkcji.

### Składnia funkcji lambda

Funkcje lambda można definiować samodzielnie...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...lub wywołane natychmiast:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Parametry akceptowane przez funkcję.
- **`body-expression`:** Logika wykonywana w momencie wywołania funkcji.
- **Natychmiastowe wywołanie:** Druga forma pokazuje natychmiastowe wywołanie lambdy z argumentami.

### Przykłady funkcji lambda

#### Używanie lambdy do prostych obliczeń

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Tutaj:

- Utworzono funkcję lambda w celu dodania dwóch liczb (`x` i `y`).
- Funkcja zostanie natychmiast wywołana z argumentami `3` i `5`.

#### Wbudowane funkcje lambda

Poniższy przykład ilustruje sposób użycia `for-each` zarówno z funkcją nazwaną, jak i funkcją lambda:

**Korzystanie z nazwanej funkcji:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Wyjaśnienie**:
  - `print-item` to nazwana funkcja, która konwertuje liczbę na ciąg znaków (`number->string`) i drukuje ją za pomocą `lumi-message`.
  - `for-each` dotyczy `print-item` dla każdego elementu na liście `(1 2 3 4)`.

**Wyjście**: 1 2 3 4

**Korzystanie z funkcji Lambda:**

Tę samą logikę można zapisać w linii z funkcją lambda, unikając potrzeby stosowania osobnej funkcji nazwanej:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Wyjaśnienie**:
  - `(lambda (x) (lumi-message (number->string x)))` definiuje funkcję anonimową.
  - Ta funkcja jest stosowana do każdego elementu listy `(1 2 3 4)` autorstwa `for-each`.

**Wyjście**: 1 2 3 4

#### Lambda pełni funkcję argumentów

Funkcje lambda są często przekazywane bezpośrednio do funkcji wyższego rzędu, takich jak `map` lub `filter`.

#### Podnoszenie listy liczb do kwadratu

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- Funkcja `lambda` podnosi do kwadratu każdy element listy.
- Funkcja `map` stosuje `lambda` do każdego elementu.

#### Funkcje lambda jako wartości zwracane

Możesz zwrócić funkcję lambda z innej funkcji, aby stworzyć dynamiczne zachowanie.

#### Generowanie funkcji sumującej

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` generuje nową funkcję lambda, która dodaje określoną liczbę (`n`).
- Zwrócona lambda jest przechowywana w `add5`, co powoduje dodanie `5` do jej wejścia.

#### Używanie Lambdy z `let`

Lambdy są często używane z `let` do tworzenia funkcji tymczasowych o zasięgu lokalnym.

#### Lokalna lambda do dodania

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- `let` wiąże funkcję lambda z nazwą `add`.
- Lambda jest następnie używana jako normalna funkcja w zakresie `let`.

#### Łączenie lambd z funkcjami wyższego rzędu

Lambdy wyróżniają się w połączeniu z funkcjami wyższego rzędu w celu wykonywania złożonych transformacji danych.

#### Filtrowanie liczb parzystych

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- `lambda` sprawdza, czy liczba jest parzysta.
- Funkcja `filter` używa lambdy, aby zachować z listy tylko liczby parzyste.

### Korzyści z funkcji lambda

- **Zwięzłość:** Lambdy redukują standardowy kod, eliminując potrzebę definiowania oddzielnych nazwanych funkcji.
- **Elastyczność:** możesz je definiować i używać tam, gdzie są potrzebne, dzięki czemu kod jest bardziej modułowy.
- ** Poprawiona czytelność:** W przypadku krótkich, konkretnych zadań wyrażenia lambda wyjaśniają intencję bez zaśmiecania kodu dodatkowymi nazwanymi funkcjami.

### Kiedy używać funkcji lambda

Użyj funkcji lambda, gdy:

- Logika jest krótka i zamknięta.
- Funkcja jest potrzebna tylko tymczasowo lub w określonym zakresie.
- Pracujesz z funkcjami wyższego rzędu, takimi jak `map`, `filter` lub `reduce`.

Unikaj używania lambd w przypadku złożonej logiki wielowierszowej, ponieważ może to zmniejszyć czytelność. W przypadku bardziej rozbudowanych operacji użyj zamiast tego funkcji nazwanej.

### Wniosek

Funkcje Lambda w Scheme zapewniają zwięzły i skuteczny sposób definiowania anonimowych funkcji dla określonych zadań. Ich elastyczność i łatwość użycia czynią je niezbędnym narzędziem dla każdego programisty Scheme. Zrozumienie, jak efektywnie używać `lambda` pomoże Ci pisać czystsze, bardziej modułowe i wydajne skrypty.