---
title: "Zmienne i zakres"
type: docs
weight: 1
---
W Scheme zarządzanie zmiennymi i ich zakresem jest podstawową koncepcją pisania wydajnych i łatwych w utrzymaniu skryptów. Zmienne przechowują wartości danych, którymi może manipulować skrypt, natomiast zakres określa, gdzie te zmienne są dostępne. Zrozumienie, jak skutecznie definiować i wykorzystywać zmienne, pozwala na tworzenie ustrukturyzowanego, wielokrotnego użytku i wolnego od błędów kodu.

### Dynamiczne pisanie

Schemat jest wpisywany dynamicznie: nie deklarujesz typów z góry, a zmienna może z czasem przechowywać wartości różnego rodzaju.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### Rola definicji i zakresu zmiennych w schemacie

Definiowanie zmiennych i zarządzanie ich zakresem służy kilku celom:
- **Organizowanie danych:** Zmienne przechowują informacje, dzięki czemu Twoje skrypty są bardziej czytelne i łatwiejsze w zarządzaniu.
- **Poprawa możliwości ponownego użycia:** Używając zmiennych o określonym zasięgu, możesz ponownie wykorzystywać sekcje kodu bez konfliktów.
- **Enkapsulacja:** Zlokalizowany zakres zapobiega niezamierzonym interakcjom między zmiennymi w różnych częściach skryptu.
- **Uproszczenie logiki:** Zmienne tymczasowe w ograniczonym zakresie zmniejszają złożoność większych obliczeń lub przepływów pracy.

### Rodzaje definicji i zakres zmiennych

Scheme udostępnia kilka konstrukcji do definiowania i określania zakresu zmiennych:
- **`let`:** Tworzy lokalne powiązania dla zmiennych w obrębie określonego bloku kodu.
- **`let*`:** Sekwencyjna wersja `let`, w której każde powiązanie może zależeć od poprzedniego.
- **Nazwany `let`:** Potężna konstrukcja do definiowania rekurencyjnych procedur lokalnych lub pętli.
- **`define`:** Tworzy zmienne globalne lub funkcje dostępne w całym skrypcie.

### Jak działają definicje i zakres zmiennych

Zmienne definicje i zakres zazwyczaj obejmują:
1. **Deklarowanie zmiennych:** Przypisywanie wartości zmiennej w określonym kontekście.
2. **Ograniczenie zakresu:** Kontrolowanie, gdzie zmienna jest dostępna (np. w bloku `let` lub globalnie).
3. **Używanie zmiennych:** Dostęp i modyfikowanie wartości zmiennych w celu wykonywania obliczeń, operacji logicznych lub proceduralnych.

### Przykład: użycie `let` dla zmiennych lokalnych

Konstrukcja `let` umożliwia zdefiniowanie zmiennych tymczasowych, które są dostępne tylko w obrębie określonego bloku:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- W tym przykładzie deklarowane są `x` i `y` z wartościami lokalnymi i obliczana jest ich suma.

### Przykład: użycie `define` dla zmiennych globalnych

Konstrukcja `define` tworzy zmienne lub funkcje o zasięgu globalnym:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Ten skrypt definiuje stałą globalną `pi` i funkcję `circle-area`, która jej używa.

### Porównanie zakresu: lokalny i globalny

| Funkcja | Zasięg lokalny (`let`, `let*`) | Globalny zasięg (`define`) |
|--------------------------------|------------------------------------------|-------------------------------------------------------|
| **Dostępność** | Ograniczone do bloku, w którym jest zdefiniowane | Dostępne w całym skrypcie |
| **Enkapsulacja** | Zapobiega niezamierzonym interakcjom | Może kolidować z innymi globalnie zdefiniowanymi zmiennymi |
| **Przypadek użycia** | Zmienne tymczasowe dla konkretnych zadań | Wspólne zmienne lub funkcje używane w całym programie |

### Streszczenie- **Definicje i zakres zmiennych** są podstawą organizowania danych i zarządzania nimi w skryptach Scheme.
- Użyj **zakresu lokalnego** (`let`, `let*`, o nazwie `let`), aby hermetzować zmienne tymczasowe i uniknąć konfliktów.
- Użyj **zakresu globalnego** (`define`) dla funkcji lub stałych wielokrotnego użytku współdzielonych w całym skrypcie.
- Jasne zrozumienie tych konstrukcji poprawi czytelność, łatwość konserwacji i niezawodność kodu.