---
title: "Symbole"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4153c94fca6fa6c5e1e98ac9449a9e7bd9cdc5b9e5dc4b96da5d8d1e8de3df43
url: "hub/scripting/fundamentals/Variables and Scope/symbols"
---
Symbole to jeden z podstawowych typów danych w Scheme, reprezentujący unikalne, niezmienne identyfikatory. Używa się ich głównie jako kluczy, znaczników lub obiektów zastępczych w programach, co czyni je niezbędnymi do pisania czystego i wyrazistego kodu.

Symbol w Scheme jest podobny do ciągu znaków, ale różni się tym, że symbole są **unikalne** i **atomowe**. Oznacza to, że dwa symbole o tej samej nazwie są tym samym obiektem, co pozwala na szybkie sprawdzanie równości i efektywne wykorzystanie w strukturach danych.

### Składnia

Symbol zapisywany jest jako ciąg znaków:

- Zaczyna się od litery, po której następują litery, cyfry lub znaki specjalne, takie jak `-`, `+` lub `*`.
- W symbolach domyślnie rozróżniana jest wielkość liter.

Przykłady:

```scheme
'hello       ; Symbol o nazwie `hello`
'foo-bar     ; Symbol o nazwie `foo-bar`
'*special*   ; Symbol o nazwie `*special*`
```

## Tworzenie symboli

Symbole są zwykle tworzone przy użyciu operatora **cytatu** (`'`), który informuje Scheme, aby traktował nazwę jako symbol, a nie oceniał ją jako zmienną lub funkcję.

### Przykład

```scheme
'my-symbol   ; Tworzy symbol `my-symbol`
```

Symbole można także tworzyć programowo, korzystając z procedury `string->symbol`, która konwertuje ciąg znaków na symbol.

```scheme
(string->symbol "dynamic-symbol")
```

**Wynik**: `'dynamic-symbol`


## Porównywanie symboli

Ponieważ symbole są unikalne, możesz je efektywnie porównywać za pomocą `eq?`.

### Przykład

```scheme
(eq? 'apple 'apple)   ; #t (ten sam symbol)
(eq? 'apple 'orange)  ; #f (różne symbole)
```

Dzięki temu symbole idealnie nadają się do wykorzystania jako klucze w strukturach danych lub znaczniki w kodzie.

## Używanie symboli

Symbole są często używane w Scheme do:

1. **Klucze na listach stowarzyszeń:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Zwraca (name . "Alice")
```

2. **Identyfikatory w kodzie:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedury pracy z symbolami

Scheme udostępnia wbudowane procedury do pracy z symbolami:

| Procedura | Opis |
|--------------------------------|--------------------------------------------------------------------------------------|
| **`symbol?`** | Sprawdza, czy obiekt jest symbolem.                                            |
| **`eq?`** | Porównuje dwa symbole tożsamości (szybkie porównanie).                       |
| **`symbol->string`** | Konwertuje symbol na ciąg (przydatne do wyświetlania lub debugowania).          |
| **`string->symbol`** | Konwertuje ciąg znaków na symbol (przydatne przy dynamicznym tworzeniu identyfikatorów). |

### Przykłady

```scheme
(symbol? 'example)            ; #t (prawda: to symbol)
(symbol->string 'example)     ; Wynik: "example"
(string->symbol "new-symbol") ; Wynik: 'new-symbol
```

## Podsumowanie

Symbole to lekki i wydajny sposób przedstawiania identyfikatorów, kluczy i znaczników w Scheme. Ich niezmienność i szybkie sprawdzanie tożsamości czynią je idealnymi do wielu zadań programistycznych. Zrozumienie, jak efektywnie używać symboli, zwiększy Twoją zdolność pisania czystego i wyrazistego kodu Scheme.