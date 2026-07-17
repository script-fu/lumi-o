---
title: "Переробка"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: af1b2f3233ef50936b24aa195d3a7da50529a4fff3109b087be2f861e15496d1
translation_lock: true
url: "hub/scripting/tutorials/First Step/reworking"
---
Цей крок виправляє непомітну поведінку в прикладі обміну повідомленнями.

Ми передавали рядок `"Hello world!\n"`. `\n` — escape-символ нового рядка; він каже виводу почати новий рядок. У Scheme це також призводить до того, що повідомлення в рядку стану з’являється як вікно GUI.

Помічник `send-to-gui` надсилає повідомлення в діалогове вікно Lumi.

Оновіть текст повідомлення та пункти призначення, щоб приклад працював узгоджено.

Видалення escape-символу та розширення функцій:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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

Замініть «магічні числа» на константи Lumi (наприклад, `MESSAGE-BOX` і `ERROR-CONSOLE`).

Потім розділіть перевірку на дві функції для повторного використання:

- `(is-valid-string?)` — перевірити, що рядок не порожній, у функціях `send-to-*`.
- `(is-valid-output-display?)` — перевірити допустимий пункт призначення в `send-message`.

Перероблена бібліотека:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Додати новий рядок, щоб примусово показати вікно
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

;; Маршрутизація повідомлення на відповідний вихід
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Перевірка непорожнього рядка message
(define (is-valid-string? message)
  ;; Перевірити, що message — непорожній рядок
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Перевірка допустимого пункту призначення
(define (is-valid-output-display? output)
  ;; Перевірити, що output — один із очікуваних виходів
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Висновок

Переробивши бібліотеку повідomлень, ми зробили її надійнішою: виправили приховану проблему з `\n`, ввели константи для ясності та додали підтримку рядка стану і діалогового вікна. Розділення перевірки на менші функції полегшує підтримку та розширення.

Ця переробка показує, як невеликі зміни покращують структуру та функціональність бібліотеки, відкриваючи шлях до гнучкості та повторного використання.
