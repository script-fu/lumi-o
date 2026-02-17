---
title: "Kiedy"
type: docs
weight: 5
---
W schemacie, chociaż `if` jest elegancki i wszechstronny, może stać się mylący, jeśli zostanie użyty bez wyraźnego `else`. Jest to szczególnie prawdziwe, gdy intencją jest wykonanie pojedynczej gałęzi kodu tylko wtedy, gdy warunek jest prawdziwy, bez alternatywnej akcji dla przypadku `false`. W takich scenariuszach konstrukcja `when` zapewnia jaśniejszą i bardziej zwięzłą alternatywę.

Podstawowa forma `when` wygląda następująco:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Jeśli `test` ma wartość true (`#t`), wszystkie wyrażenia w treści konstrukcji `when` są wykonywane sekwencyjnie.
- Jeśli `test` ma wartość false (`#f`), nic się nie dzieje i nie są zwracane żadne wartości.

### Przykład

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Kontrastowe `if` i `when`

Aby lepiej zrozumieć różnicę między `if` i `when`, rozważ następujący przykład, w którym oba są używane razem:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Wyjaśnienie:

1. **Warunek `if`**:
   - Test `(= 0 1)` sprawdza, czy 0 jest równe 1.
   - Ponieważ to jest fałszywe (`#f`), wykonywana jest gałąź `else` `if`.

2. **Konstrukcja `when` w gałęzi `else`**:
   - Test `when` `(< 0 1)` sprawdza, czy 0 jest mniejsze niż 1.
   - Ponieważ jest to prawdą (`#t`), wszystkie wyrażenia w treści `when` są wykonywane sekwencyjnie:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Dlaczego warto używać tutaj `when`?

- Użycie `when` zamiast innego `if` upraszcza logikę, gdy nie ma potrzeby jawnej gałęzi `else` dla warunku.
- `when` wyjaśnia, że ​​istotna jest tylko prawdziwa gałąź, co zmniejsza potencjalne zamieszanie.

### Podsumowanie

- Użyj `if`, gdy potrzebujesz zarówno prawdziwej, jak i fałszywej gałęzi.
- Użyj `when`, gdy istnieje tylko jedna gałąź dla prawdziwego przypadku, zwłaszcza gdy należy wykonać wiele akcji.
- Połączenie `if` i `when` może pomóc w jasnym i zwięzłym ustrukturyzowaniu bardziej złożonych warunków.