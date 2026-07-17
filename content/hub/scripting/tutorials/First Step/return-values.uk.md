---
title: "Значення, що повертаються"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
translation_lock: true
url: "hub/scripting/tutorials/First Step/return-values"
---
Значення, що повертаються, дозволяють керувати потоком без додаткового стану. У Scheme останній обчислений вираз стає результатом функції.

На цій сторінці помічники перевірки з прикладу обміну повідомленнями показують, як явні повернення спрощують код.

### Що таке значення, що повертається?

У Scheme результат функції визначає останній обчислений вираз. Якщо явного повернення немає, функція дає `#f` (хибність) або `undefined`.

Повернемося до `(is-valid-string?)`:

```scheme
;; Призначення: перевірити, що message — непорожній рядок
(define (is-valid-string? message)
  ;; Перевірити, що message — непорожній рядок
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Якщо повідомлення недійсне — `error`. Якщо дійсне — явного результату немає, і функція повертає `#f` за замовчуванням.

### Явні значення, що повертаються

Можна зробити результат явним — наприклад, повернути `#t` (істина), коли перевірка пройдена:

```scheme
;; Призначення: перевірити, що output — допустимий пункт призначення
(define (is-valid-output-display? output)
  ;; Перевірити, що output — один із очікуваних виходів
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

Тут функція повертає `#t` при успішній перевірці — зручніше використовувати її там, де потрібен логічний результат.

### Ефективне використання повернень

Явні `#t`, `#f` або конкретні результати роблять функції передбачуванішими. Повернене значення можна використати в коді, що викликає, або передати далі.

Приклад керування потоком:

```scheme
;; Призначення: направити повідомлення на відповідний вихід
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

`send-message` покладається на результат `is-valid-output-display?`. Якщо перша перевірка не пройде, `cond` не виконається. Код читається природно: «якщо вихід допустимий — надіслати».

## Логіка `if` у Scheme

Короткий огляд умовної логіки. Scheme використовує `if` для вибору між двома шляхами.

```scheme
(if (conditional test)
  do-if-true
  do-if-false)
```

Якщо умова істинна — перша гілка; якщо хибна — друга.

Для кількох дій у гілці використовуйте `begin`:

```scheme
(if (conditional test)
  (begin
    do-if-true)
  (begin
    do-if-false))
```

Ось бібліотека з явними поверненнями, що керують виконанням:

### Рефакторинг із поверненнями

```scheme
;; Призначення: надіслати в рядок стану; повертає #t при успіху
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Призначення: надіслати в діалогове вікно; повертає #t при успіху
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Призначення: надіслати в консоль помилок; повертає #t при успіху
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Призначення: надіслати в термінал; повертає #t при успіху
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Призначення: маршрутизація; повертає #t при успіху
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Призначення: перевірити непорожній рядок; повертає #t якщо валідно
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Призначення: перевірити допустимий output; повертає #t якщо валідно
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Висновок

Значення, що повертаються, роблять функції гнучкими та придатними для повторного використання. Свідомий вибір того, що повертає кожна функція, покращує взаємодію між частинами коду. `#t`, `#f` або конкретні результати дають контроль над потоком програми.
