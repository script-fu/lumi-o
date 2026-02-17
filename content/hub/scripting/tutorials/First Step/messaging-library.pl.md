---
title: "Biblioteka wiadomości"
type: docs
weight: 6
---
Z biegiem czasu to, co początkowo było pojedynczą funkcją wysyłania wiadomości, przekształciło się w zbiór powiązanych funkcji. Funkcje te stanowią obecnie podstawę **Biblioteki wiadomości**, zaprojektowanej do obsługi danych wyjściowych do różnych miejsc docelowych, takich jak GUI, konsola błędów i terminal.

### Dlaczego biblioteka wiadomości?

W miarę wzrostu naszych potrzeb obsługa komunikatów przesyłanych na wiele wyjść wymaga bardziej modułowego i rozszerzalnego podejścia. Zamiast jednej funkcji, która robi wszystko, podzieliliśmy proces na komponenty, które można ponownie wykorzystać, co zapewnia większą elastyczność. Biblioteki tej można teraz używać jako narzędzia do przesyłania wiadomości ogólnego przeznaczenia, z którego mogą korzystać inne wtyczki lub funkcje.

### Do czego służy biblioteka wiadomości?

Biblioteka wiadomości zawiera obecnie następujące funkcje:

- **send-to-gui**: Wysyła wiadomości do okna dialogowego GUI Lumi.
- **send-to-error-console**: Wysyła wiadomości do konsoli błędów Lumi.
- **wyślij do terminala**: Wysyła wiadomości do okna terminala.
- **send-message**: Funkcja dyspozytora, która kieruje wiadomości do odpowiedniego wyjścia.
- **validate-message**: Zapewnia, że ​​wiadomość i dane wyjściowe są prawidłowe przed wysłaniem.

### Rozszerzanie biblioteki

**Bibliotekę wiadomości** można łatwo rozszerzyć, aby obsługiwała dodatkowe wyjścia. Na przykład:

- **wyślij do pliku**: zapisywanie wiadomości w pliku dziennika.
- **wyślij do rejestratora**: Integracja z zewnętrznym systemem rejestrowania.
- **wyślij do powiadomienia**: wyświetlaj wiadomości jako powiadomienia systemowe.

Kierując się tym samym wzorcem modułowej konstrukcji i funkcjami wielokrotnego użytku, biblioteka ta może stać się wszechstronnym narzędziem do obsługi wszelkiego rodzaju zadań związanych z przesyłaniem wiadomości.

## Korzyści z biblioteki wiadomości

- **Ponowne użycie**: Funkcje można ponownie wykorzystać w różnych wtyczkach lub projektach.
- **Modułowość**: Każda funkcja obsługuje jedno konkretne zadanie, dzięki czemu kod jest łatwiejszy w utrzymaniu i rozszerzaniu.
- **Spójność**: Korzystanie z tych samych funkcji sprawdzania poprawności i obsługi komunikatów zapewnia spójne zachowanie w całej aplikacji.

**Biblioteka wiadomości** to początek szerszej struktury, która może uprościć sposób zarządzania wiadomościami w Twoim projekcie. W miarę powiększania się biblioteki nowe wtyczki mogą z łatwością korzystać z niej, aby wysyłać wiadomości, gdziekolwiek chcą.

Możemy dostosować strukturę pliku:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

I pamiętaj o dostosowaniu `load` w głównej wtyczce:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```