---
title: "Jeśli"
type: docs
weight: 4
---
W najprostszej formie warunek `if` w schemacie ocenia test i na podstawie wyniku wykonuje jeden z dwóch możliwych bloków kodu. Najprostsza forma wygląda następująco:

```scheme
(if test-is-true
  do-this)
```

- Jeśli `test` ma wartość true (`#t`), wykonywany jest **blok kodu w następstwie**. Blok może zwrócić wartość lub wykonać inne akcje, takie jak przypisanie zmiennej lub wydruk.

### Przykład

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- W tym przypadku `test` to `(< 0 1)` (sprawdzanie, czy 0 jest mniejsze niż 1).
- Ponieważ test ma wartość true (`#t`), wykonywany jest blok kodu `(lumi-message "True!")`, który wypisuje `"True!"`.

### Dodawanie warunku Else: `if-else`

Kiedy używasz warunku `if` z alternatywnym blokiem kodu (przypadek `else`), struktura wygląda następująco:

```scheme
(if test
  do-this
  else-do-this)
```

- Jeśli `test` ma wartość true (`#t`), wykonywany jest **następujący** blok kodu.
- Jeśli `test` ma wartość false (`#f`), wykonywany jest **alternatywny** blok kodu.

```scheme
(if test
  consequent
  alternative)
```

### Jak to działa

1. **Wyrażenie testowe**:
   - Wyrażenie `test` jest oceniane jako pierwsze.

2. **Wynik na podstawie testu**:
   - Jeśli `test` ma wartość true (`#t`), wykonywany jest **kolejny blok kodu**.
   - Jeśli `test` ma wartość false (`#f`), wykonywany jest **alternatywny blok kodu**.

Zarówno bloki kodu `consequent`, jak i `alternative` mogą wykonywać dowolną prawidłową operację na schemacie, w tym zwracać wartości, modyfikować zmienne lub uruchamiać procedury.

### Przykłady

#### Przykład 1: Zwracanie wartości

```scheme
(if (< 0 1)
  1
  0)
```

- Tutaj `test` to `(< 0 1)` (sprawdzanie, czy 0 jest mniejsze niż 1).
- Ponieważ wynik testu ma wartość true (`#t`), wykonywany jest **następujący** blok (`1`) i zwracana jest jego wartość.

Wynik: **1**

#### Przykład 2: Obliczanie bloku początkowego

W przypadkach, gdy musisz wykonać wiele akcji, gdy warunek jest prawdziwy lub fałszywy, możesz użyć `begin` lub `let`, aby zgrupować je razem.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- W tym przykładzie `test` to `(= 0 1)` (sprawdzanie, czy 0 równa się 1).
- Ponieważ wynik testu jest fałszywy (`#f`), wykonywany jest blok **alternatywny**:
  - Najpierw drukuje `"False condition met, calculating..."`.
  - Następnie oblicza `(* 3 4)` i zwraca `12`.

Wynik: **Wypisuje „Spełniono fałszywy warunek, obliczam…” i zwraca 12,**

#### Przykład 3: Ocena instrukcji let

Użycie `let` pozwala nam zadeklarować zmienne o zasięgu lokalnym za pomocą bloku kodu.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- W tym przykładzie `test` to `(= 1 1)` (sprawdzanie, czy 1 równa się 1).
- Ponieważ wynik testu jest prawdziwy (`#t`), wykonywany jest **następujący** blok:
  - Najpierw drukuje `"True condition met, calculating..."`.
  - Następnie oblicza `(* -1 10)` i zwraca `-10`.

Wynik: **Wypisuje „Prawdziwy warunek spełniony, obliczanie…” i zwraca -10,**

### Podsumowanie- Warunek `if` jest potężnym narzędziem w schemacie do oceny testów i wykonywania odpowiednich bloków kodu.
- Może obsługiwać zarówno proste wyrażenia, jak i złożone bloki kodu, które zwracają wartości, modyfikują zmienne lub wykonują efekty uboczne.
- Pamiętaj: Jeśli nie ma wyraźnego bloku `else`, `if` ocenia i wykonuje **następnik** tylko wtedy, gdy test jest prawdziwy. W przeciwnym razie ocenia i wykonuje **alternatywę**.