---
title: "Підсумки"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
translation_lock: true
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
Тепер у вас є робочий procedure plug-in і невелика допоміжна бібліотека. У цій серії представлено основні шаблони для більшості скриптів Lumi:

- **Функції**: будівельні блоки плагінів.
- **Рефакторинг**: покращення структури коду без зміни поведінки.
- **Бібліотеки коду**: централізація багаторазових функцій для чистого модульного коду.
- **Перевірка**: валідація вхідних даних перед основною логікою.

Ви також ознайомилися з основами Git для відстеження змін і чіткої структури проєкту — це полегшує ітерації без втрати робочих версій.

Остаточна версія головного коду плагіна:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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

Код бібліотеки:

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

Рефакторинг помічників обміну повідомленнями в невелику бібліотеку залишає плагін зосередженим на намірі, а деталі реалізації — у бібліотеці. Перевірка та узгоджена маршрутизація роблять збої передбачуваними.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Наступні кроки:

- Перенесіть багаторазові помічники в окремий файл бібліотеки.
- Тримайте плагіни компактними; називайте процедури за тим, що вони роблять.
- Додавайте перевірку на межах (вхідні дані, шляхи до файлів, параметри меню).

Зберігайте результат як два файли в репозиторії плагінів:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`
