---
title: "Ostatnie przemyślenia"
type: docs
weight: 10
---
Masz teraz działającą wtyczkę procedury i małą bibliotekę pomocniczą. W tej serii przedstawiono podstawowe wzorce, których będziesz używać w większości skryptów Lumi:

- Funkcje: Elementy składowe naszych wtyczek.
- Refaktoryzacja: Poprawa struktury kodu przy jednoczesnym zachowaniu funkcjonalności.
- Biblioteki kodu: centralizacja funkcji wielokrotnego użytku, aby nasz kod był czysty i modułowy.
- Techniki walidacji: Zapewnienie, że dane wejściowe są prawidłowe przed wykonaniem naszej podstawowej logiki.

Poznałeś także podstawy używania Gita do śledzenia zmian i utrzymywania przejrzystej struktury projektu. Ten przepływ pracy ułatwia iterację bez utraty działających wersji.

Oto ostateczna wersja naszego głównego kodu wtyczki:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Kod biblioteki:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Wniosek

Dzięki refaktoryzacji pomocników przesyłania wiadomości do małej biblioteki wtyczka pozostaje skupiona na celu, a biblioteka zawiera szczegóły implementacji. Walidacja i spójne kierowanie komunikatów zapewniają przewidywalność awarii.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Kolejne kroki:

- Przenieś pomocników wielokrotnego użytku do dedykowanego pliku biblioteki.
- Staraj się, aby wtyczki były małe i nazwij procedury odpowiadające ich działaniu.
- Dodaj weryfikację na granicach (wejścia, ścieżki plików, opcje menu).

Zachowaj końcowy wynik jako dwa pliki w repozytorium wtyczek:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`