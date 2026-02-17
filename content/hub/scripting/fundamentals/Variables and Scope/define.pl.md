---
title: "określić"
type: docs
weight: 3
---
Instrukcja `define` w Scheme jest wszechstronną konstrukcją używaną do tworzenia globalnych lub lokalnych powiązań. Najczęściej używany jest do definiowania zmiennych i funkcji, dzięki czemu można je ponownie wykorzystać i udostępnić w całym skrypcie lub w określonym zakresie. Zrozumienie `define` jest kluczowe dla pisania modułowych, wielokrotnego użytku i czytelnych programów Scheme.

### Cel `define`

Konstrukcja `define` służy wielu celom:
- **Definiowanie zmiennych**: Przypisuje wartości do nazw zmiennych, udostępniając je do późniejszego wykorzystania.
- **Definiowanie funkcji**: Tworzy procedury wielokrotnego użytku, które zawierają określoną logikę.
- **Definicje lokalne**: Używane w funkcji `define` tworzy lokalne powiązania, które nie wpływają na globalną przestrzeń nazw.

---

### Definiowanie zmiennych za pomocą `define`

Podstawowym zastosowaniem `define` jest tworzenie zmiennych przechowujących wartości stałe lub obliczone.

#### Składnia
```scheme
(define variable-name value)
```

#### Przykład: Definiowanie stałej
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Wynik**: `6.28318`

---

### Definiowanie funkcji za pomocą `define`

Możesz użyć `define` do tworzenia procedur wielokrotnego użytku.

#### Składnia
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Przykład: Definiowanie prostej funkcji
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Wynik**: `16`

---

### Lokalne definicje za pomocą `define`

Używane wewnątrz funkcji `define` tworzy lokalne powiązania, które są dostępne tylko w obrębie funkcji otaczającej. Pozwala to uniknąć zanieczyszczania globalnej przestrzeni nazw i pomaga uporządkować kod.

#### Przykład: Lokalne funkcje pomocnicze
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Wynik**: `41` (oblicza \(2^2 + 3^3 + 4^2\))

---

### Kluczowe cechy `define`

1. **Zakres globalny lub lokalny**:
   - Kiedy jest używany na najwyższym poziomie, `define` tworzy zmienne globalne lub funkcje.
   - Kiedy jest używany wewnątrz innej funkcji, `define` tworzy lokalne powiązania.

2. **Ponowne użycie**:
   - Funkcje zdefiniowane za pomocą `define` mogą być ponownie użyte wielokrotnie w różnych kontekstach.

3. **Poprawiona czytelność**:
   - Podział logiki na mniejsze, dobrze nazwane funkcje poprawia przejrzystość i łatwość konserwacji kodu.

---

### Różnice między `define` i `let`

| **Aspekt** | **`define`** | **`let`** |
|----------------------------------|----------------------------------------------------------------|-------------------------------------------------|
| **Cel** | Tworzy globalne lub lokalne powiązania dla zmiennych lub funkcji. | Tworzy tymczasowe powiązania w zlokalizowanym zakresie. |
| **Zakres** | Globalny na najwyższym poziomie; local, gdy znajduje się w innej funkcji. | Zawsze lokalnie w bloku `let`.       |
| **Ponowne użycie** | Funkcje i zmienne mogą być ponownie wykorzystywane w wielu miejscach. | Zmienne są tymczasowo powiązane z pojedynczym blokiem. |
| **Składnia** | Jawnie definiuje zmienne lub funkcje.       | Łączy wiązanie zmiennych z oceną wyrażenia. |