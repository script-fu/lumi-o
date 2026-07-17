---
title: "Walidacja"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
url: "hub/scripting/tutorials/First Step/validation"
---
Tworząc niezawodne wtyczki, ważne jest, aby nasze funkcje sprawnie obsługiwały błędy i działały zgodnie z oczekiwaniami, nawet w przypadku niewłaściwego użycia lub nieoczekiwanych danych wejściowych. Walidacja pomaga chronić integralność funkcji i zapobiega awariom lub niezamierzonym zachowaniom.

Zobaczmy, jak ulepszyć funkcję `send-message`, dodając kontrole walidacji, które zapewnią poprawne obsłużenie danych wejściowych.

### Sprawdź wprowadzone dane

Przed wysłaniem wiadomości upewnijmy się, że argument `output` przekazany do funkcji `send-message` jest prawidłowy. Możemy dodać sprawdzenie, czy miejsce docelowe wyjścia to jedna z oczekiwanych wartości (`gui`, `error-console` lub `terminal`).

Przykład:

```scheme
(define (send-message message output)
  ;; Sprawdza poprawność argumentu wyjścia
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
  (lumi-message-set-handler 2))
```

W tym przykładzie używamy `member`, aby sprawdzić, czy argument `output` jest prawidłowy. Jeśli nie, funkcja zgłasza błąd z jasnym komunikatem, zapobiegając problemom spowodowanym nieprawidłowymi wartościami.

### Obsługuj puste wiadomości

Warto też upewnić się, że argument `message` jest prawidłowy. Na przykład, jeśli jako wiadomość przekazano pusty ciąg lub `#f`, funkcja powinna obsłużyć to poprawnie.

Przykład obsługi pustej wiadomości:

```scheme
(define (send-message message output)
  ;; Sprawdź, czy wiadomość jest pusta
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

To podejście zapewnia, że funkcja zawsze otrzymuje prawidłowe dane wejściowe, co zwiększa jej niezawodność i zapobiega nieoczekiwanym zachowaniom.

### Przykład połączonej walidacji

```scheme
;; Funkcja obsługująca wysyłanie wiadomości do różnych miejsc docelowych
(define (send-message message output)

  ;; Zweryfikuj argumenty wiadomości i wyjścia
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
  (lumi-message-set-handler 2))
```

W tej wersji:
- Funkcja najpierw sprawdza, czy `message` jest puste lub nieprawidłowe. Jeśli wiadomość jest poprawna, przechodzi do sprawdzenia, czy `output` jest jedną z akceptowanych wartości (`gui`, `error-console` lub `terminal`).
- Jeżeli oba sprawdzenia przebiegną pomyślnie, wiadomość zostanie wysłana na odpowiednie wyjście. W przeciwnym razie zostanie wyświetlony komunikat o błędzie z jasnym wyjaśnieniem.
- Przeprowadzana jest dodatkowa kontrola, aby upewnić się, że wiadomość jest również ciągiem znaków.

Ta łączona funkcja sprawdzania poprawności sprawia, że kod jest czystszy i zapewnia, że oba dane wejściowe zostaną sprawdzone przed podjęciem jakichkolwiek działań, dzięki czemu funkcja jest bardziej niezawodna. Budujemy też system komunikatów debugowania: gdy kod zawiedzie, otrzymujemy powód, który sami napisaliśmy.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```
