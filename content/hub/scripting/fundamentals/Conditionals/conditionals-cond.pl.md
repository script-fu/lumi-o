---
title: "kon"
type: docs
weight: 5
---
W schemacie warunek `cond` służy do wybrania jednego z kilku możliwych bloków kodu do wykonania na podstawie wielu testów. To jest jak wielogałęziowy `if`, w którym każda gałąź jest sprawdzana w kolejności, aż do znalezienia pasującej.

### Składnia

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Każdy test jest oceniany w kolejności, w jakiej został napisany.
- Kiedy wynik testu ma wartość true (`#t`), wykonywany jest odpowiadający mu **następnik**, a wyrażenie `cond` przestaje oceniać dalsze testy.
- Klauzula `else` jest opcjonalna i służy jako rezerwa, jeśli żaden z testów nie uzyska wyniku true.

### Jak to działa

1. **Przetestuj każdy warunek**:
   - `cond` ocenia testy w kolejności, w jakiej są wymienione.

2. **Wykonaj pasujący następnik**:
   - Po znalezieniu pierwszego testu, który ma wartość true (`#t`), wykonywany jest jego **następnik**.
   - Jeśli żaden test nie zwróci wartości true i występuje klauzula `else`, wykonywana jest **konsekwencja zastępcza**.

### Przykłady

#### Przykład 1: Następniki pojedynczego wyrażenia

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Pierwszy test `(< 3 2)` daje wynik fałszywy (`#f`).
- Drugi test `(= 3 3)` ma wartość true (`#t`), więc zwracany jest `"This will run"`.
- Klauzula `else` nie jest wykonywana, ponieważ znaleziono już dopasowanie.

Wynik: **„To zadziała”**

#### Przykład 2: Wiele akcji przy użyciu `begin`

Gdy konsekwencja obejmuje wiele działań, użyj `begin`, aby je pogrupować:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- Pierwszy test `(< 5 3)` daje wynik fałszywy (`#f`).
- Drugi test `(> 5 3)` daje wynik prawdziwy (`#t`):
  - Drukuje `"Condition met"`.
  - Następnie oblicza `(* 5 5)` i zwraca `25`.

Wynik: **Wypisuje „Warunek spełniony” i zwraca 25,**

#### Przykład 3: Użycie bloku `let` w następniku

Jeśli chcesz wprowadzić zmienne lokalne, użyj bloku `let`:

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- Pierwszy test `(< 0 -1)` jest fałszywy.
- Drugi test `(> 0 -1)` jest prawdziwy, więc:
  - Wykonywany jest blok `let`, wiążący `y` z `20`.
  - Drukuje `"Positive condition met"`.
  - Następnie oblicza `(+ y y)` i zwraca `40`.

Wynik: **Wypisuje komunikat „Spełniono warunek pozytywny” i zwraca 40,**

#### Przykład 4: Powrót z `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Żaden z dwóch pierwszych testów nie daje wartości true.
- Klauzula `else` jest wykonywana i zwraca `"Fallback value"`.

Wynik: **„Wartość zastępcza”**

### Podsumowanie

- Użyj `cond` do obsługi wielu warunków w jasny i zwięzły sposób.
- Następnikami mogą być pojedyncze wyrażenia lub akcje zgrupowane przy użyciu `begin`.
- Użyj `let` w następnikach, aby zadeklarować zmienne lokalne do obliczeń.
- Zawsze dołączaj klauzulę `else` jako rozwiązanie awaryjne w przypadku nieoczekiwanych przypadków.

Ta elastyczność sprawia, że ​​`cond` jest potężnym i czytelnym narzędziem do obsługi złożonej logiki rozgałęzień.