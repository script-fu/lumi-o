---
title: "Do"
type: docs
weight: 5
---
Funkcja `do` w schemacie jest mechanizmem zapętlającym, który umożliwia iterację z warunkami inicjalizacji, aktualizacji i zakończenia. Jest to szczególnie przydatne, gdy trzeba wykonać sekwencję operacji określoną liczbę razy lub do momentu spełnienia warunku.

Ogólna forma `do` to:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Zmienna**: Zmienne pętli.
- **Wartość-początkowa**: Wartość początkowa każdej zmiennej pętli.
- **Wyrażenie-aktualizacyjne**: Wyrażenie aktualizujące zmienne pętli na końcu każdej iteracji.
- **Warunek zakończenia**: Warunek zatrzymania pętli.
- **Wyrażenie-wyniku**: Wartość zwracana po zakończeniu pętli.
- **Treść**: Kod do wykonania w każdej iteracji.

---

### Przykład: zsumuj liczby od 1 do 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- Zmienna pętli `i` zaczyna się od 1 i zwiększa się o 1 w każdej iteracji.
- Zmienna `sum` gromadzi sumę `i`.
- Pętla kończy się, gdy `i > 5`, zwracając końcową wartość `sum`.

**Wyjście**: `15`

---

### Jak to działa

1. **Inicjalizacja**:
   - Każdej zmiennej pętli przypisana jest jej wartość początkowa.

2. **Kontrola zakończenia**:
   - Na początku każdej iteracji sprawdzany jest warunek zakończenia. Jeśli ma wartość true, pętla zatrzymuje się i oceniane jest wyrażenie wynikowe.

3. **Iteracja**:
   - If the termination condition is false, the body is executed, and the loop variables are updated using their respective update expressions.

---

### Podsumowanie

- Konstrukcja `do` zapewnia elastyczny sposób implementacji pętli z wieloma zmiennymi i złożonymi warunkami zakończenia.
- Jest to przydatne w przypadku zadań wymagających aktualizacji stanu w iteracjach.
- Warunek zakończenia określa, kiedy pętla się kończy i może zwrócić ostateczny wynik.

Używając `do`, możesz zaimplementować algorytmy iteracyjne w schemacie z precyzyjną kontrolą inicjalizacji, aktualizacji i zakończenia. To sprawia, że ​​`do` jest kombinacją **mechanizmu wiązania o określonym zakresie** (takiego jak `let`) i **iteracyjnej struktury kontrolnej**, umożliwiającej obsługę pętli i stanu tymczasowego w czysty i zwięzły sposób.