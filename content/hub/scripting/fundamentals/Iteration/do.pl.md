---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
---
Funkcja `do` w Scheme to mechanizm pętli umożliwiający iterację z inicjalizacją, aktualizacją i warunkami zakończenia. Jest szczególnie przydatna, gdy trzeba wykonać sekwencję operacji określoną liczbę razy lub do momentu spełnienia warunku.

Ogólna postać `do` wygląda tak:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Zmienne:** Zmienna(e) pętli.
- **Wartość początkowa:** Wartość początkowa każdej zmiennej pętli.
- **Wyrażenie aktualizacji:** Wyrażenie aktualizujące zmienną(e) pętli na końcu każdej iteracji.
- **Warunek zatrzymania:** Warunek zatrzymania pętli.
- **Wyrażenie wyniku:** Wartość zwracana po zakończeniu pętli.
- **Ciało:** Kod wykonywany w każdej iteracji.

---

### Przykład: suma liczb od 1 do 5

```scheme
(do ((i 1 (+ i 1))      ; Inicjalizuj i do 1, zwiększaj o 1
     (sum 0 (+ sum i))) ; Inicjalizuj sum do 0, dodaj i do sum
    ((> i 5) sum)       ; Zakończ, gdy i > 5, zwróć sum
  (lumi-message (number->string sum))) ; Wypisuje sumę na każdym kroku
```

- Zmienna pętli `i` zaczyna od 1 i zwiększa się o 1 w każdej iteracji.
- Zmienna `sum` akumuluje sumę wartości `i`.
- Pętla kończy się, gdy `i > 5`, zwracając końcową wartość `sum`.

**Wynik**: `15`

---

### Jak to działa

1. **Inicjalizacja:**
   - Każdej zmiennej pętli przypisywana jest wartość początkowa.

2. **Sprawdzenie warunku zakończenia:**
   - Na początku każdej iteracji sprawdzany jest warunek zakończenia. Jeśli jest prawdziwy, pętla się zatrzymuje i ewaluowane jest wyrażenie wyniku.

3. **Iteracja:**
   - Jeśli warunek zakończenia jest fałszywy, wykonywane jest ciało pętli, a zmienne pętli są aktualizowane za pomocą odpowiednich wyrażeń aktualizacji.

---

### Podsumowanie

- Konstrukcja `do` zapewnia elastyczny sposób implementacji pętli z wieloma zmiennymi i złożonymi warunkami zakończenia.
- Jest przydatna w zadaniach wymagających aktualizacji stanu między iteracjami.
- Warunek zakończenia określa, kiedy pętla się kończy i może zwrócić końcowy wynik.

Dzięki `do` można implementować algorytmy iteracyjne w Scheme z precyzyjną kontrolą inicjalizacji, aktualizacji i zakończenia. `do` łączy **mechanizm wiązania w zakresie** (jak `let`) z **strukturą kontroli iteracyjnej**, umożliwiając obsługę pętli i stanu tymczasowego w czysty i zwięzły sposób.
