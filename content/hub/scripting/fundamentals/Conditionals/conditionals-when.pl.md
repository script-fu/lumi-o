---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
W Scheme `if` jest elegancki i wszechstronny, ale bez jawnej gałęzi `else` może szybko wprowadzać w błąd — zwłaszcza gdy chcemy wykonać kod tylko wtedy, gdy warunek jest prawdziwy, bez alternatywy dla przypadku fałszywego. W takich sytuacjach konstrukcja `when` daje jaśniejszą i bardziej zwięzłą alternatywę.

Podstawowa postać `when` wygląda tak:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Jeśli `test` ewaluuje się do prawdy (`#t`), wszystkie wyrażenia w ciele `when` są wykonywane po kolei.
- Jeśli `test` ewaluuje się do fałszu (`#f`), nic się nie dzieje i nie zwracana jest żadna wartość.

### Przykład

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Porównanie `if` i `when`

Aby lepiej zrozumieć różnicę, rozważ przykład, w którym obie konstrukcje występują razem:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Wyjaśnienie

1. **Warunek `if`:**
   - Test `(= 0 1)` sprawdza, czy 0 równa się 1.
   - Ponieważ wynik to fałsz (`#f`), wykonywana jest gałąź `else`.

2. **Konstrukcja `when` w gałęzi `else`:**
   - Test `when` `(< 0 1)` sprawdza, czy 0 jest mniejsze od 1.
   - Ponieważ wynik to prawda (`#t`), wszystkie wyrażenia w ciele `when` są wykonywane po kolei:
     - najpierw wypisuje `"The 'when' condition is true!"`,
     - następnie wypisuje `"Executing multiple actions within 'when'."`.

#### Dlaczego tu `when`?

- Zamiast kolejnego `if` bez sensownej gałęzi `else` logika jest prostsza.
- `when` jasno pokazuje, że liczy się tylko przypadek prawdziwy, co redukuje zamieszanie.

### Podsumowanie

- Używaj `if`, gdy potrzebujesz zarówno gałęzi prawdziwej, jak i fałszywej.
- Używaj `when`, gdy istnieje tylko gałąź dla przypadku prawdziwego — zwłaszcza gdy trzeba wykonać wiele działań.
- Łączenie `if` i `when` pomaga uporządkować bardziej złożone warunki w czytelny i zwięzły sposób.
