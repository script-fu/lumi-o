---
title: "Funkcje"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a1808e88698d7f38626bf136806af5388132ed2799927b899141c749dac679a3
url: "hub/scripting/fundamentals/Functions/_index"
---
Funkcje są podstawową koncepcją w Scheme, zapewniającą środki do enkapsulacji logiki, umożliwienia ponownego wykorzystania kodu i efektywnej struktury skryptów. Dzięki funkcjom możesz tworzyć modułowe, łatwe w utrzymaniu skrypty, które obsługują szeroki zakres zadań, od podstawowych operacji po zaawansowane przepływy pracy w Lumi.

Ta sekcja służy jako wprowadzenie do funkcji w Scheme i stanowi podstawę do zrozumienia ich typów, definicji i zastosowań. W kolejnych sekcjach zagłębimy się w konkretne typy funkcji i ich unikalne możliwości.

## Minimalna składnia i wyrażenia

Kod Scheme składa się z **wyrażeń**. Wyrażenie zwraca wartość. Składnia jest jednolita: nawiasy tworzą wywołanie, a nazwa operatora lub funkcji znajduje się na początku.

```scheme
(+ 1 2)         ; Dodaje 1 i 2, wynik 3
(if #t 1 0)     ; Zwraca 1, ponieważ warunek jest prawdziwy
(list 1 2 3)    ; Tworzy listę: (1 2 3)
```

Ponieważ wszystko jest wyrażeniem, przepływ sterowania w naturalny sposób wpasowuje się w ten sam styl, co wywołania funkcji.

## Dlaczego funkcje mają znaczenie

Funkcje odgrywają kluczową rolę w Scheme z kilku powodów:

- **Ponowne wykorzystanie kodu:** Unikaj powtórzeń, hermetyzując logikę w komponentach wielokrotnego użytku.
- **Modułowość:** Podziel złożone zadania na mniejsze, łatwiejsze do wykonania części.
- **Zachowanie dynamiczne:** Akceptuj parametry, aby obsługiwać różne wejścia lub dostosowywać się do różnych sytuacji.
- **Wyższa abstrakcja:** Uprość logikę, skupiając się na tym, „co” funkcja robi, a nie na „jak” to robi.

## Przegląd typów funkcji

Scheme oferuje różnorodne konstrukcje funkcji, każda dostosowana do konkretnych przypadków użycia:

1. **Nazwane funkcje**
   Są to standardowe funkcje zdefiniowane za pomocą `define`. Stanowią one podstawę większości skryptów.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Funkcje anonimowe**
   Są to nienazwane funkcje definiowane bezpośrednio w kodzie, zwane także **funkcjami lambda**, przeznaczone do jednorazowego użytku.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Funkcje wyższego rzędu**
   Funkcje, które przyjmują inne funkcje jako argumenty lub zwracają funkcje jako wyniki, umożliwiając zaawansowane abstrakcje, takie jak mapowanie, filtrowanie i redukcja.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Zwraca (1 4 9 16)
   ```

## Ogólna składnia funkcji

Funkcje w Scheme mają prostą i spójną składnię:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Nazwa funkcji.
- **`parameter1, parameter2, ...`:** Argumenty przyjmowane przez funkcję.
- **`body-expression`:** Logika wykonywana w momencie wywołania funkcji.

Przykład:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Zwraca 8
```

## Skutki uboczne i stan globalny

W Lumi wiele przydatnych procedur ma **skutki uboczne**: modyfikują obraz, zmieniają rysunek, zapisują plik lub wyświetlają wynik.

- Wyodrębnij skutki uboczne w małych, wyraźnie nazwanych procedurach.
- Unikaj zmiany kontekstu globalnego, jeśli nie jest to konieczne.
- Kiedy zmieniasz kontekst (kolory, pędzle itp.), zawiń pracę `lumi-context-push` i `lumi-context-pop`, aby przywrócić stan użytkownika.