---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
W najprostszej postaci warunek `if` w Scheme ewaluuje test i na podstawie wyniku wykonuje jeden z dwóch możliwych bloków kodu. Najprostsza postać wygląda tak:

```scheme
(if test-is-true
  do-this)
```

- Jeśli `test` ewaluuje się do prawdy (`#t`), wykonywany jest **blok consequent**. Blok może zwracać wartość lub wykonywać inne działania, takie jak przypisanie zmiennej lub wypisanie wyniku.

### Przykład

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- W tym przypadku `test` to `(< 0 1)` (sprawdzenie, czy 0 jest mniejsze od 1).
- Ponieważ test ewaluuje się do prawdy (`#t`), wykonywany jest blok kodu `(lumi-message "True!")`, który wypisuje `"True!"`.

### Dodawanie warunku else: `if-else`

Gdy warunek `if` ma alternatywny blok kodu (przypadek `else`), struktura wygląda tak:

```scheme
(if test
  do-this
  else-do-this)
```

- Jeśli `test` ewaluuje się do prawdy (`#t`), wykonywany jest blok **consequent**.
- Jeśli `test` ewaluuje się do fałszu (`#f`), wykonywany jest blok **alternative**.

```scheme
(if test
  consequent
  alternative)
```

### Jak to działa

1. **Wyrażenie testowe:**
   - Najpierw ewaluowany jest `test`.

2. **Wynik w zależności od testu:**
   - Jeśli `test` ewaluuje się do prawdy (`#t`), wykonywany jest **blok consequent**.
   - Jeśli `test` ewaluuje się do fałszu (`#f`), wykonywany jest **blok alternative**.

Zarówno bloki `consequent`, jak i `alternative` mogą wykonywać dowolne poprawne operacje Scheme, w tym zwracać wartości, modyfikować zmienne lub uruchamiać procedury.

### Przykłady

#### Przykład 1: zwracanie wartości

```scheme
(if (< 0 1)
  1
  0)
```

- Tutaj `test` to `(< 0 1)` (sprawdzenie, czy 0 jest mniejsze od 1).
- Ponieważ test ewaluuje się do prawdy (`#t`), wykonywany jest blok **consequent** (`1`) i zwracana jest jego wartość.

Wynik: **1**

#### Przykład 2: ewaluacja bloku `begin`

Gdy trzeba wykonać wiele działań, gdy warunek jest prawdziwy lub fałszywy, można użyć `begin` lub `let`, aby je pogrupować.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- W tym przykładzie `test` to `(= 0 1)` (sprawdzenie, czy 0 równa się 1).
- Ponieważ test ewaluuje się do fałszu (`#f`), wykonywany jest blok **alternative**:
  - Najpierw wypisuje `"False condition met, calculating..."`.
  - Następnie oblicza `(* 3 4)` i zwraca `12`.

Wynik: **Wypisuje "False condition met, calculating..." i zwraca 12.**

#### Przykład 3: ewaluacja wyrażenia `let`

Użycie `let` pozwala zadeklarować zmienne lokalne w bloku kodu.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- W tym przykładzie `test` to `(= 1 1)` (sprawdzenie, czy 1 równa się 1).
- Ponieważ test ewaluuje się do prawdy (`#t`), wykonywany jest blok **consequent**:
  - Najpierw wypisuje `"True condition met, calculating..."`.
  - Następnie oblicza `(* -1 10)` i zwraca `-10`.

Wynik: **Wypisuje "True condition met, calculating..." i zwraca -10.**

### Podsumowanie

- Warunek `if` to potężne narzędzie w Scheme do ewaluacji testów i wykonywania odpowiednich bloków kodu.
- Obsługuje zarówno proste wyrażenia, jak i złożone bloki kodu zwracające wartości, modyfikujące zmienne lub wykonujące efekty uboczne.
- Pamiętaj: jeśli nie ma jawnego bloku `else`, `if` ewaluuje i wykonuje **consequent** tylko wtedy, gdy test jest prawdziwy; w przeciwnym razie **alternative**.
