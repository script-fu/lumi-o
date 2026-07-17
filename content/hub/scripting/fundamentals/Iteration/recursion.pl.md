---
title: "Prosta rekursja"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
Rekursja to potężna koncepcja w Scheme, w której funkcja wywołuje samą siebie, rozwiązując mniejsze podproblemy oryginalnego problemu. Wzorzec **prostej rekursji** obejmuje przypadek bazowy zatrzymujący rekursję oraz przypadek rekurencyjny zmniejszający problem.

Ogólna struktura funkcji rekurencyjnej wygląda tak:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Warunek bazowy:** Zatrzymuje rekursję.
- **Wynik bazowy:** Wartość zwracana, gdy spełniony jest warunek bazowy.
- **Wywołanie rekurencyjne:** Wywołanie samej funkcji ze zmodyfikowanymi argumentami, które przybliżają obliczenia do przypadku bazowego.

---

### Przykład: suma liczb (od 1 do n)

Prosta funkcja rekurencyjna obliczająca sumę liczb od 1 do n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Przypadek bazowy: zatrzymaj, gdy n wynosi 0
    0                          ; Wynik bazowy: suma wynosi 0
    (+ n (sum-to-n (- n 1))))) ; Wywołanie rekurencyjne: dodaj bieżące n do wyniku mniejszego podproblemu
```

---

#### Jak to działa: rozkładanie i składanie

Rekursja działa poprzez rozbijanie oryginalnego problemu na mniejsze części. Każde wywołanie funkcji obsługuje jedną część i przekazuje resztę dalej. Gdy osiągnięty zostaje najprostszy przypadek, wyniki są składane w miarę ukończenia obliczeń.

#### Śledzenie krok po kroku: sum-to-n 3

1. **Pierwsze wywołanie:** *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Drugie wywołanie:** *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Trzecie wywołanie:** *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Przypadek bazowy:** *sum-to-n 0*
   → *0*

---

#### Składanie końcowego wyniku

Gdy najprostszy przypadek zostaje rozwiązany, każda warstwa obliczeń się kończy:

1. *sum-to-n 0* daje *0*
2. *sum-to-n 1* staje się *(+ 1 0) = 1*
3. *sum-to-n 2* staje się *(+ 2 1) = 3*
4. *sum-to-n 3* staje się *(+ 3 3) = 6*

---

### Przykład: wypisywanie każdego elementu listy

Oto prosta funkcja rekurencyjna wypisująca każdy element listy:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "gotowe")
    (begin
      (lumi-message (number->string (car lst))) ; Wypisuje pierwszy element
      (print-elements (cdr lst)))))             ; Przetwarza resztę listy
```

- **Przypadek bazowy:** Jeśli lista jest pusta (*null? lst*), rekursja się zatrzymuje.
- **Przypadek rekurencyjny:** Wypisz pierwszy element (*car lst*), następnie wywołaj funkcję na reszcie listy (*cdr lst*).

#### Przykład użycia

```scheme
(print-elements (list 1 2 3))
```

Wynik:

- *"1"*
- *"2"*
- *"3"*

Rezultat: *„gotowe”*

---

#### Jak to działa

1. Funkcja pobiera pierwszy element listy za pomocą *car* i go przetwarza.
2. Następnie wywołuje samą siebie z resztą listy (*cdr*).
3. Proces powtarza się, aż lista będzie pusta (*null? lst*).

---

### Podsumowanie

- Prosta rekursja składa się z:
  1. **Przypadku bazowego:** Zatrzymuje rekursję.
  2. **Przypadku rekurencyjnego:** Zmniejsza problem w kierunku przypadku bazowego.
- Każde wywołanie rekurencyjne przybliża obliczenia do zakończenia.
- Gdy osiągnięty zostaje przypadek bazowy, wyniki są łączone po zakończeniu rekursji.

Rekursja odzwierciedla strukturę problemu i zapewnia jasny, logiczny przepływ. Zawsze zapewnij przypadek bazowy, aby uniknąć nieskończonej rekursji.
