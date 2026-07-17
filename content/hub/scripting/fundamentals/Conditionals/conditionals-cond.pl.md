---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
W Scheme warunek `cond` służy do wyboru jednego z wielu możliwych bloków kodu do wykonania na podstawie wielu testów. Jest to wielościeżkowy odpowiednik `if`, w którym każda gałąź jest sprawdzana po kolei, aż zostanie znaleziona pasująca.

### Składnia

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Każdy test jest ewaluowany w kolejności, w jakiej został zapisany.
- Gdy test ewaluuje się do prawdy (`#t`), wykonywany jest odpowiadający mu **consequent**, a wyrażenie `cond` przestaje ewaluować dalsze testy.
- Klauzula `else` jest opcjonalna i służy jako rezerwa, gdy żaden test nie ewaluuje się do prawdy.

### Jak to działa

1. **Testowanie każdego warunku:**
   - `cond` ewaluuje testy w podanej kolejności.

2. **Wykonanie pasującego consequent:**
   - Gdy znaleziony zostanie pierwszy test ewaluujący się do prawdy (`#t`), wykonywany jest jego **consequent**.
   - Jeśli żaden test nie ewaluuje się do prawdy i istnieje klauzula `else`, wykonywany jest **fallback-consequent**.

### Przykłady

#### Przykład 1: consequent jako pojedyncze wyrażenie

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Pierwszy test `(< 3 2)` ewaluuje się do fałszu (`#f`).
- Drugi test `(= 3 3)` ewaluuje się do prawdy (`#t`), więc zwracane jest `"This will run"`.
- Klauzula `else` nie jest wykonywana, ponieważ znaleziono już pasujący warunek.

Wynik: **"This will run"**

#### Przykład 2: wiele działań z `begin`

Gdy consequent obejmuje wiele działań, użyj `begin`, aby je pogrupować:

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

- Pierwszy test `(< 5 3)` ewaluuje się do fałszu (`#f`).
- Drugi test `(> 5 3)` ewaluuje się do prawdy (`#t`):
  - Wypisuje `"Condition met"`.
  - Następnie oblicza `(* 5 5)` i zwraca `25`.

Wynik: **Wypisuje "Condition met" i zwraca 25.**

#### Przykład 3: blok `let` w consequent

Gdy potrzebne są zmienne lokalne, użyj bloku `let`:

```scheme
(cond
  ;; Przypadek 1: Jeśli 0 jest mniejsze od -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Przypadek 2: gdy 0 jest większe niż -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Przypadek domyślny: gdy żaden z powyższych warunków nie jest spełniony
  (else
    (let ((z 0))
      z)))
```

- Pierwszy test `(< 0 -1)` jest fałszywy.
- Drugi test `(> 0 -1)` jest prawdziwy, więc:
  - Wykonywany jest blok `let` wiążący `y` z wartością `20`.
  - Wypisuje `"Positive condition met"`.
  - Następnie oblicza `(+ y y)` i zwraca `40`.

Wynik: **Wypisuje "Positive condition met" i zwraca 40.**

#### Przykład 4: rezerwa z `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Żaden z dwóch pierwszych testów nie ewaluuje się do prawdy.
- Wykonywana jest klauzula `else`, która zwraca `"Fallback value"`.

Wynik: **"Fallback value"**

### Podsumowanie

- Używaj `cond` do obsługi wielu warunków w przejrzysty i zwięzły sposób.
- Consequent mogą być pojedynczymi wyrażeniami lub zgrupowanymi działaniami za pomocą `begin`.
- Używaj `let` w consequent, aby deklarować zmienne lokalne do obliczeń.
- Zawsze dołącz klauzulę `else` jako rezerwę na nieoczekiwane przypadki.

Ta elastyczność czyni `cond` potężnym i czytelnym narzędziem do obsługi złożonej logiki rozgałęzień.
