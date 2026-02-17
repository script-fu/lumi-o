---
title: "Przeróbka"
type: docs
weight: 7
---
Ten krok rozwiązuje subtelne zachowanie w przykładzie wiadomości.

Jako wiadomość przekazywaliśmy ciąg „Hello world!\n”. „\n” to specjalny rodzaj znaku, znak „ucieczki”. Mówi wydrukowi wyjściowemu, aby rozpoczął nową linię. W schemacie wymusi to również wyświetlenie komunikatu wysłanego na pasek stanu jako okna GUI.

Pomocnik `send-to-gui` wysyła wiadomości do okna dialogowego Lumi.

Zaktualizuj treść wiadomości i miejsca docelowe, aby przykład działał spójnie.

Usunięcie znaku ucieczki i rozszerzenie funkcji:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

Zastąp liczby magiczne stałymi dostarczonymi przez Lumi (na przykład `MESSAGE-BOX` i `ERROR-CONSOLE`).

Następnie podziel walidację na dwie funkcje, aby można było ją ponownie wykorzystać w wielu witrynach połączeń.

- (is-valid-string?) Aby sprawdzić, czy ciąg znaków jest ciągiem, a nie pustym ciągiem, w ramach funkcji wysyłania do*.
- (is-valid-output-display?) Aby sprawdzić, czy dane miejsce docelowe wyjścia jest prawidłowe, w funkcji send-message.

Przerób bibliotekę:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Wniosek

Przeprojektowując naszą bibliotekę wiadomości, uczyniliśmy ją bardziej solidną i niezawodną. Naprawiliśmy ukryty problem ze znakiem nowej linii, wprowadziliśmy stałe dla lepszej przejrzystości i rozszerzyliśmy funkcjonalność, dodając obsługę wyników paska stanu i okien dialogowych. Dodatkowo rozdzielenie logiki walidacji na mniejsze, skupione funkcje sprawia, że ​​nasz kod będzie łatwiejszy w utrzymaniu i rozbudowie w przyszłości.

Ta przeróbka pokazuje, jak małe zmiany mogą ulepszyć ogólną strukturę i funkcjonalność naszej biblioteki, torując drogę do większej elastyczności i możliwości ponownego użycia w miarę rozwoju naszego projektu.