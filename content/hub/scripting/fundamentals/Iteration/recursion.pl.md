---
title: "Prosta rekurencja"
type: docs
weight: 5
---
Rekurencja to potężna koncepcja w schemacie, w której funkcja wywołuje samą siebie, aby rozwiązać mniejsze podproblemy pierwotnego problemu. Wzorzec **prostej rekurencji** obejmuje przypadek podstawowy zatrzymujący rekurencję i przypadek rekurencyjny mający na celu zmniejszenie problemu.

Ogólna struktura funkcji rekurencyjnej wygląda następująco:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Warunek podstawowy**: Zatrzymuje rekurencję.
- **Wynik bazowy**: Wartość zwracana po spełnieniu warunku podstawowego.
- **Wywołanie rekurencyjne**: Wywołanie samej funkcji ze zmodyfikowanymi argumentami, które przybliżają obliczenia do przypadku podstawowego.

---

### Przykład: suma liczb (1 do n)

Prosta funkcja rekurencyjna do obliczania sumy liczb od 1 do n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Jak to działa: rozkładanie i składanie

Rekurencja działa poprzez rozbicie pierwotnego problemu na mniejsze części. Każde wywołanie funkcji obsługuje jeden element i przekazuje resztę dalej. Po osiągnięciu najprostszego przypadku wyniki są ponownie składane po zakończeniu obliczeń.

#### Krok po kroku śledzenie sumy do n 3

1. **Początkowe wywołanie**: *suma do n 3*
   → *(+ 3 (suma do n 2))*

2. **Drugie połączenie**: *suma do n 2*
   → *(+ 2 (suma do n 1))*

3. **Trzecie połączenie**: *suma do n 1*
   → *(+ 1 (suma do n 0))*

4. **Przypadek podstawowy**: *suma do n 0*
   → *0*

---

#### Ponowne składanie wyniku końcowego

Po rozwiązaniu najprostszego przypadku każda warstwa obliczeń kończy się:

1. *suma do n 0* daje *0*
2. *suma do n 1* staje się *(+ 1 0) = 1*
3. *suma do n 2* staje się *(+ 2 1) = 3*
4. *suma do n 3* staje się *(+ 3 3) = 6*

---

### Przykład: drukowanie każdego elementu listy

Oto prosta funkcja rekurencyjna umożliwiająca wydrukowanie każdego elementu listy:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Przypadek podstawowy:** Jeśli lista jest pusta (*null? lst*), zatrzymaj rekurencję.
- **Przypadek rekurencyjny:** Wydrukuj pierwszy element (*car lst*), a następnie wywołaj funkcję na pozostałej części listy (*cdr lst*).

#### Przykładowe użycie

```scheme
(print-elements (list 1 2 3))
```

Dane wyjściowe:

- *"1"*
- *"2"*
- *"3"*

Wynik: *„gotowe”*

---

#### Jak to działa

1. Funkcja pobiera pierwszy element listy za pomocą *car* i przetwarza go.
2. Następnie wywołuje siebie wraz z resztą listy (*cdr*).
3. Ten proces jest powtarzany, aż lista będzie pusta (*null? lst*).

---

### Podsumowanie

- Prosta rekurencja składa się z:
  1. **Przypadek podstawowy**: Zatrzymuje rekurencję.
  2. **Przypadek rekurencyjny**: Zmniejsza problem w kierunku przypadku podstawowego.
- Każde wywołanie rekurencyjne prowadzi obliczenia do końca.
- Po osiągnięciu przypadku podstawowego wyniki są łączone po zakończeniu rekurencji.

Rekurencja odzwierciedla strukturę problemu i zapewnia przejrzysty, logiczny przepływ. Zawsze upewnij się, że jest to przypadek podstawowy, aby uniknąć nieskończonej rekurencji.