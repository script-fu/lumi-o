---
title: "Refaktoryzacja ponownie"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
url: "hub/scripting/tutorials/First Step/refactor-again"
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
    ;; Wyślij do konsoli komunikatów
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

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
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

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
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

Zobacz, że usunęliśmy walidację z funkcji `send-message` i przenieśliśmy odpowiedzialność na każdą osobną funkcję wyjściową. Ta zmiana zapewnia, że każde miejsce docelowe (GUI, konsola komunikatów, terminal) obsługuje własną walidację, upraszczając `send-message` i trzymając logikę sprawdzania bliżej miejsca, w którym jest potrzebna.

Takie podejście może uprościć `send-message`, czyniąc ją _dyspozytorem_, przy jednoczesnym zapewnieniu, że każda funkcja `send-to-*` poprawnie waliduje wiadomość przed przetworzeniem.

Przenosząc walidację do każdej funkcji `send-to-*`, umożliwiliśmy ich ponowne użycie jako samodzielnych funkcji. Oznacza to, że możemy wywołać `send-to-gui`, `send-to-error-console` lub `send-to-terminal` bezpośrednio, bez polegania na dyspozytorze `send-message`. Każda z tych funkcji obsługuje teraz w pełni własną logikę i może być używana niezależnie w innych częściach kodu lub w innych wtyczkach, dzięki czemu kod jest bardziej modułowy i elastyczny.

## Korzyści z refaktoryzacji

- **Wyraźne oddzielenie obaw**: Każda funkcja obsługuje teraz tylko jedną odpowiedzialność, dzięki czemu kod jest łatwiejszy do zrozumienia.
- **Rozszerzalność**: Dodawanie nowych typów wyjść jest proste. Wystarczy zdefiniować nową funkcję, taką jak `send-to-file` lub `send-to-logger`, a następnie dodać gałąź w instrukcji `cond`.
- **Zapewniać ponowne użycie**: Każdą z tych funkcji obsługi wyników można ponownie wykorzystać w innym miejscu projektu lub udostępnić wielu wtyczkom.
- **Spójność**: Ponowne użycie funkcji sprawdzania poprawności w każdej funkcji `send-to-*` zapewnia, że wszystkie dane wyjściowe są prawidłowo sprawdzane, co czyni kod bardziej niezawodnym.

Zrefaktoryzowana wersja biblioteki:

```scheme
;; Cel: Wysyła wiadomość do okna dialogowego GUI
(define (send-to-gui message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Cel: Wysyła wiadomość do konsoli komunikatów
(define (send-to-error-console message)
  ;; Zweryfikuj wiadomość przed kontynuowaniem
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Cel: Wysyła wiadomość do okna terminala
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

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
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

Czy to już wszystko? Nie — jest jeszcze sporo do zrobienia, czytaj dalej.