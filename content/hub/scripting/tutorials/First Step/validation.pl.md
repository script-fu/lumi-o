---
title: "Walidacja"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d5d160ddb40b6a09f1d92ebf0287ce6912dcc703702b7701c564688226e92842
---
Tworząc niezawodne wtyczki, ważne jest, aby nasze funkcje sprawnie obsługiwały błędy i działały zgodnie z oczekiwaniami, nawet w przypadku niewłaściwego użycia lub nieoczekiwanych danych wejściowych. Validation helps protect the integrity of the function and prevent crashes or unintended behavior.

Let’s look at how we can improve the `send-message` function by adding validation checks to ensure it handles inputs correctly.

### Sprawdź wprowadzone dane

Before sending a message, we should ensure the `output` argument passed to the `send-message` function is valid. We can add a check to confirm that the output destination is one of the expected values (gui, error-console, or terminal).

Przykład:

```scheme
(define (send-message message output)
  ;; Sprawdza poprawność argumentu wyjścia
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))
```

In this example, we use `member` to check if the `output` argument is valid. If not, the function raises an error with a clear message, preventing invalid values from causing issues.

### Obsługuj puste wiadomości

It’s also useful to ensure that the `message` argument is valid. For example, if an empty string or #f (false) is passed as the message, the function should handle this gracefully.

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

This approach ensures that the function always receives valid input, improving its reliability and preventing unexpected behavior.

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
           (display message)))))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))
```

W tej wersji:
- The function checks if the `message` is empty or invalid first. Jeśli wiadomość jest poprawna, przechodzi do sprawdzenia, czy `output` jest jedną z akceptowanych wartości (`gui`, `error-console`, lub `terminal`).
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