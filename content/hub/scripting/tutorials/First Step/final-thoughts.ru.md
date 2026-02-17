---
title: "Заключительные мысли"
type: docs
weight: 10
---
Теперь у вас есть плагин рабочей процедуры и небольшая вспомогательная библиотека. В этой серии представлены основные шаблоны, которые вы будете использовать в большинстве скриптов Lumi:

- Функции: строительные блоки наших плагинов.
- Рефакторинг: улучшение структуры кода при сохранении функциональности.
- Библиотеки кода: централизованное использование функций многократного использования для поддержания чистоты и модульности нашего кода.
- Методы проверки: проверка правильности входных данных перед выполнением нашей основной логики.

Вы также ознакомились с основами использования Git для отслеживания изменений и поддержания чистой структуры проекта. Такой рабочий процесс упрощает повторение без потери рабочих версий.

Вот окончательная версия нашего основного кода плагина:

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

Код библиотеки:

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

## Заключение

Благодаря рефакторингу помощников по обмену сообщениями в небольшую библиотеку подключаемый модуль остается сосредоточенным на намерении, а библиотека содержит детали реализации. Проверка и согласованная маршрутизация сообщений делают сбои предсказуемыми.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Следующие шаги:

— Переместите многоразовые помощники в отдельный файл библиотеки.
- Делайте плагины небольшими и называйте процедуры, соответствующие их функциям.
- Добавить проверку границ (входные данные, пути к файлам, параметры меню).

Сохраните окончательный результат в виде двух файлов в репозитории плагинов:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`