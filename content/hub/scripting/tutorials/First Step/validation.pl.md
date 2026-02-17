---
title: "Walidacja"
type: docs
weight: 4
---
Tworząc niezawodne wtyczki, ważne jest, aby nasze funkcje sprawnie obsługiwały błędy i działały zgodnie z oczekiwaniami, nawet w przypadku niewłaściwego użycia lub nieoczekiwanych danych wejściowych. Walidacja pomaga chronić integralność funkcji i zapobiegać awariom lub niezamierzonym zachowaniom.

Przyjrzyjmy się, jak możemy ulepszyć funkcję `send-message`, dodając kontrole walidacyjne, aby upewnić się, że poprawnie obsługuje ona dane wejściowe.

### Sprawdź wprowadzone dane

Przed wysłaniem wiadomości powinniśmy upewnić się, że argument `output` przekazany do funkcji `send-message` jest poprawny. Możemy dodać kontrolę, aby potwierdzić, że miejsce docelowe wyjścia jest jedną z oczekiwanych wartości (gui, konsola błędów lub terminal).

Przykład:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Send to the Error Console
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Send to the GUI dialog box
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Send to the terminal window
      ((eq? output 'terminal)
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

W tym przykładzie używamy `member`, aby sprawdzić, czy argument `output` jest poprawny. Jeśli nie, funkcja zgłasza błąd z wyraźnym komunikatem, zapobiegając powodowaniu problemów przez nieprawidłowe wartości.

### Obsługuj puste wiadomości

Przydatne jest również upewnienie się, że argument `message` jest poprawny. Na przykład, jeśli jako wiadomość zostanie przekazany pusty ciąg znaków lub #f (fałsz), funkcja powinna obsłużyć to bezproblemowo.

Przykład obsługi pustej wiadomości:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
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

Takie podejście zapewnia, że funkcja zawsze otrzymuje prawidłowe dane wejściowe, co poprawia jej niezawodność i zapobiega nieoczekiwanym zachowaniom.

### Przykład połączonej walidacji

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Send to the Error Console
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Send to the GUI dialog box
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Send to the terminal window
        ((eq? output 'terminal)
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

W tej wersji:
- Funkcja sprawdza najpierw, czy `message` jest pusty lub nieprawidłowy. Jeśli wiadomość jest poprawna, przechodzi do sprawdzenia, czy `output` jest jedną z zaakceptowanych wartości (`gui`, `error-console`, lub `terminal`).
- Jeżeli obydwa sprawdzenia przebiegną pomyślnie, wiadomość zostanie wysłana na odpowiednie wyjście. W przeciwnym razie zostanie wyświetlony komunikat o błędzie z jasnym wyjaśnieniem.
- Przeprowadzana jest dodatkowa kontrola, aby upewnić się, że wiadomość jest również ciągiem znaków.

Ta łączona funkcja sprawdzania poprawności sprawia, że ​​kod jest czystszy i zapewnia, że ​​oba dane wejściowe zostaną sprawdzone przed podjęciem jakichkolwiek działań, dzięki czemu funkcja jest bardziej niezawodna. Zauważ, że budujemy także system przesyłania komunikatów debugowania. Kiedy
kod zawodzi, otrzymujemy powód, powód, który sami napisaliśmy.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```