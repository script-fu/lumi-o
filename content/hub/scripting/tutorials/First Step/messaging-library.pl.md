---
title: "Biblioteka wiadomości"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bfe459b9f717201d646bde29196fd66e6c3b19b3e9dbdb3338e0c853153e1c05
---
Z biegiem czasu to, co początkowo było pojedynczą funkcją wysyłania wiadomości, przekształciło się w zbiór powiązanych funkcji. These functions now form the foundation of a **Messaging Library**, designed to handle output to different destinations, such as the GUI, Message console, and OS terminal.

### Dlaczego biblioteka wiadomości?

W miarę wzrostu naszych potrzeb obsługa komunikatów przesyłanych na wiele wyjść wymaga bardziej modułowego i rozszerzalnego podejścia. Zamiast jednej funkcji, która robi wszystko, podzieliliśmy proces na komponenty, które można ponownie wykorzystać, co zapewnia większą elastyczność. This library can now be used as a general-purpose messaging tool that other plug-ins or functions can borrow from.

### Do czego służy biblioteka wiadomości?

Biblioteka wiadomości zawiera obecnie następujące funkcje:

- **send-to-gui**: Wysyła wiadomości do okna dialogowego GUI Lumi.
- **send-to-error-console**: Wysyła wiadomości do konsoli Lumi Message.
- **wyślij do terminala**: Wysyła wiadomości do okna terminala.
- **send-message**: Funkcja dyspozytora, która kieruje wiadomości do odpowiedniego wyjścia.
- **validate-message**: Zapewnia, że ​​wiadomość i dane wyjściowe są prawidłowe przed wysłaniem.

### Rozszerzanie biblioteki

**Bibliotekę wiadomości** można łatwo rozszerzyć, aby obsługiwała dodatkowe wyjścia. Na przykład:

- **wyślij do pliku**: zapisywanie wiadomości w pliku dziennika.
- **send-to-logger**: Integrate with an external logging system.
- **wyślij do powiadomienia**: wyświetlaj wiadomości jako powiadomienia systemowe.

By following the same pattern of modular design and reusable functions, this library can grow into a comprehensive tool for handling all kinds of messaging tasks.

## Korzyści z biblioteki wiadomości

- **Ponowne użycie**: Funkcje można ponownie wykorzystać w różnych wtyczkach lub projektach.
- **Modułowość**: Każda funkcja obsługuje jedno konkretne zadanie, dzięki czemu kod jest łatwiejszy w utrzymaniu i rozszerzaniu.
- **Spójność**: Korzystanie z tych samych funkcji sprawdzania poprawności i obsługi komunikatów zapewnia spójne zachowanie w całej aplikacji.

The **Messaging Library** is the beginning of a broader framework that could simplify how messages are managed in your project. W miarę powiększania się biblioteki nowe wtyczki mogą z łatwością korzystać z niej, aby wysyłać wiadomości, gdziekolwiek chcą.

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