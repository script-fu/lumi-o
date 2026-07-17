---
title: "Refaktoryzacja ponownie"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4563817b27aa107aa948c9bb7fb53f358c663dfbc6f070c4a4b725b0d1d600f0
---
W miarę rozrastania się biblioteki pomocniczej, śledzenie jej na pierwszy rzut oka staje się trudniejsze. Ponownie dokonaj refaktoryzacji, aby każda funkcja była mała i przeznaczona do jednego celu.

### Przełamywanie złożoności

Aby ułatwić śledzenie i utrzymywanie funkcji, podziel ją na mniejsze, skupione funkcje. Zacznij od oddzielenia sprawdzania poprawności od kierowania komunikatów.

### Utwórz funkcję sprawdzającą

Możemy wykorzystać część funkcji sprawdzającą argumenty `message` i `output` i przenieść ją do osobnej funkcji. W ten sposób podstawowa funkcja `send-message` nie musi się martwić o walidację, co ułatwia jej przestrzeganie.

```scheme
(define (validate-message message output)
  ;; Sprawdź, czy wiadomość jest niepustym ciągiem znaków
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Sprawdź, czy wyjście jest jednym z oczekiwanych miejsc docelowych
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Uprość wysyłanie wiadomości

Teraz, gdy sprawdzanie poprawności zostało przeniesione do osobnej funkcji, funkcja `send-message` może skupić się na samym wysyłaniu wiadomości. Będzie to znacznie prostsze, ponieważ zajmie się jedynie konkretnym zadaniem skierowania wiadomości do właściwego miejsca docelowego.

```scheme
(define (send-message message output)
  ;; Wywołaj funkcję walidacji przed kontynuowaniem
  (validate-message message output)

  (cond
    ;; Wyślij do Message console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Wyślij do okna dialogowego GUI
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Wyślij do okna terminala
    ((eq? output 'terminal)
       (display message)))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))
```

### Dalsze rozbicie: oddziel każdą procedurę obsługi wyjścia

Każdy typ komunikatu wyjściowego (GUI, konsola komunikatów, terminal) można przenieść do własnej funkcji. Pozwala to na łatwiejsze testowanie, modyfikację i potencjalną rozbudowę w przyszłości.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Wyślij do właściwego wyjścia
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))
```

### Ponowne użycie walidacji w każdej funkcji wysyłania

Ponieważ walidacja jest ważną częścią zapewnienia, że zarówno komunikat, jak i dane wyjściowe są poprawne, sensowne jest, aby każda funkcja `send-*` przeprowadziła własną walidację. Dzięki temu niezależnie od tego, które wyjście zostanie wywołane, zawsze najpierw sprawdzimy wejścia.

```scheme
(define (send-to-gui message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'terminal)
  (display message))
```

Zobacz, że usunęliśmy sprawdzanie poprawności z funkcji wysyłania wiadomości i przenieśliśmy odpowiedzialność na każdą indywidualną funkcję wyjściową. Ta zmiana zapewnia, że ​​każde miejsce docelowe (GUI, konsola komunikatów, terminal) obsługuje własną weryfikację, usprawniając funkcję wysyłania wiadomości i utrzymując logikę sprawdzania bliżej miejsca, w którym jest to potrzebne.

Takie podejście może uprościć funkcję wysyłania wiadomości, czyniąc ją _dyspozytorem_, zapewniając jednocześnie, że każda funkcja wysyłania do* poprawnie sprawdza poprawność wiadomości przed przetworzeniem.

Przenosząc weryfikację do każdej funkcji wysyłania do*, umożliwiliśmy ich ponowne użycie jako samodzielnych funkcji. Oznacza to, że możemy wywołać dowolną funkcję send-to-gui, send-to-error-console lub send-to-terminal bezpośrednio, bez polegania na funkcji wysyłającej wiadomość. Każda z tych funkcji obsługuje teraz w pełni swoją własną logikę i może być używana niezależnie w innych częściach kodu lub w innych wtyczkach, dzięki czemu Twój kod jest bardziej modułowy i elastyczny.

## Korzyści z refaktoryzacji

- **Wyraźne oddzielenie obaw**: Każda funkcja obsługuje teraz tylko jedną odpowiedzialność, dzięki czemu kod jest łatwiejszy do zrozumienia.
- **Rozszerzalność**: Dodawanie nowych typów wyników jest proste. Po prostu definiujesz nową funkcję, taką jak `send-to-file` lub `send-to-logger`, a następnie dodajesz wielkość liter w instrukcji `cond`.
- **Ponowne użycie**: Każdą z tych funkcji obsługi wyników można ponownie wykorzystać w innym miejscu projektu lub udostępnić wielu wtyczkom.
- **Spójność**: Ponowne użycie funkcji sprawdzania poprawności w każdej funkcji `send-to-*` zapewnia, że ​​wszystkie dane wyjściowe są prawidłowo sprawdzane, co czyni kod bardziej niezawodnym.

Zrefaktoryzowana wersja biblioteki:

```scheme
;; Cel: Wysyła wiadomość do okna dialogowego GUI
(define (send-to-gui message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Cel: Wysyła wiadomość do Message console
(define (send-to-error-console message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Cel: Wysyła wiadomość do okna terminal
(define (send-to-terminal message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'terminal)
  (display message))

;; Cel: Wysyła wiadomość do właściwego miejsca docelowego wyjścia
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))

;; Cel: Sprawdza, czy wiadomość jest niepustym ciągiem znaków i czy wyjście jest poprawne
(define (validate-message message output)
  ;; Sprawdź, czy wiadomość jest niepustym ciągiem znaków
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Sprawdź, czy wyjście jest jednym z oczekiwanych miejsc docelowych
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Czy to wszystko co możemy zrobić? NIE! jest jeszcze wiele do zrobienia, czytaj dalej.