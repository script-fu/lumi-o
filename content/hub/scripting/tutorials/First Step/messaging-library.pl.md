---
title: "Biblioteka wiadomości"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
url: "hub/scripting/tutorials/First Step/messaging-library"
---
Z biegiem czasu to, co początkowo było pojedynczą funkcją wysyłania wiadomości, przekształciło się w zbiór powiązanych funkcji. Te funkcje stanowią teraz fundament **biblioteki wiadomości**, zaprojektowanej do obsługi wyjścia na różne cele, takie jak GUI, konsola komunikatów i terminal systemu operacyjnego.

### Dlaczego biblioteka wiadomości?

W miarę wzrostu naszych potrzeb obsługa komunikatów przesyłanych na wiele wyjść wymaga bardziej modułowego i rozszerzalnego podejścia. Zamiast jednej funkcji, która robi wszystko, podzieliliśmy proces na komponenty, które można ponownie wykorzystać, co zapewnia większą elastyczność. Bibliotekę tę można teraz wykorzystywać jako uniwersalne narzędzie do wiadomości, z którego mogą korzystać inne wtyczki lub funkcje.

### Do czego służy biblioteka wiadomości?

Biblioteka wiadomości zawiera obecnie następujące funkcje:

- **send-to-gui**: Wysyła wiadomości do okna dialogowego GUI Lumi.
- **send-to-error-console**: Wysyła wiadomości do konsoli komunikatów Lumi.
- **send-to-terminal**: Wysyła wiadomości do okna terminala.
- **send-message**: Funkcja dyspozytora, która kieruje wiadomości do odpowiedniego wyjścia.
- **validate-message**: Zapewnia, że wiadomość i dane wyjściowe są prawidłowe przed wysłaniem.

### Rozszerzanie biblioteki

**Bibliotekę wiadomości** można łatwo rozszerzyć, aby obsługiwała dodatkowe wyjścia. Na przykład:

- **send-to-file**: zapisywanie wiadomości w pliku dziennika.
- **send-to-logger**: integracja z zewnętrznym systemem rejestrowania.
- **send-to-notification**: wyświetlanie wiadomości jako powiadomień systemowych.

Stosując ten sam wzorzec modułowego projektu i funkcji wielokrotnego użytku, biblioteka ta może rozrosnąć się w kompletne narzędzie do obsługi wszelkich zadań związanych z wiadomościami.

## Korzyści z biblioteki wiadomości

- **Ponowne użycie**: Funkcje można ponownie wykorzystać w różnych wtyczkach lub projektach.
- **Modularyzacja kodu**: Każda funkcja obsługuje jedno konkretne zadanie, dzięki czemu kod jest łatwiejszy w utrzymaniu i rozszerzaniu.
- **Spójność**: Korzystanie z tych samych funkcji sprawdzania poprawności i obsługi komunikatów zapewnia spójne zachowanie w całej aplikacji.

**Biblioteka wiadomości** to początek szerszego frameworku, który może uprościć zarządzanie wiadomościami w projekcie. W miarę rozwoju biblioteki nowe wtyczki mogą z łatwością z niej korzystać, wysyłając wiadomości tam, gdzie tego potrzebują.

Możemy dostosować strukturę pliku:

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

I pamiętaj o dostosowaniu `load` w głównej wtyczce:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
