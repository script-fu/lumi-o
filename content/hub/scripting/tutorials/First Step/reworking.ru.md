---
title: "Переработка"
type: docs
weight: 7
---
Этот шаг исправляет тонкое поведение в примере обмена сообщениями.

В качестве сообщения мы передавали строку «Hello world!\n». «\n» — это особый тип символа, escape-символ. Он сообщает выходной печати о начале новой строки. В Scheme сообщение, отправленное в строку состояния, также будет отображаться в виде окна графического интерфейса.

Помощник `send-to-gui` отправляет сообщения в диалоговое окно Lumi.

Обновите содержимое сообщения и места назначения, чтобы пример работал последовательно.

Удаление escape-символа и расширение функций:
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

Замените магические числа константами, предоставленными Lumi (например, `MESSAGE-BOX` и `ERROR-CONSOLE`).

Затем разделите проверку на две функции, чтобы ее можно было повторно использовать с нескольких мест вызова.

- (is-valid-string?) Чтобы проверить, что строка является строкой, а не пустой строкой, в функции send-to*.
- (is-valid-output-display?) Чтобы проверить правильность данного места назначения вывода, в функции отправки сообщения.

Переработать библиотеку:

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

## Заключение

Переработав нашу библиотеку обмена сообщениями, мы сделали ее более надежной. Мы исправили скрытую проблему с символом новой строки, ввели константы для большей ясности и расширили функциональность, добавив поддержку вывода строки состояния и диалогового окна. Кроме того, разделение логики проверки на более мелкие, целенаправленные функции гарантирует, что наш код будет легче поддерживать и расширять в будущем.

Эта переработка демонстрирует, как небольшие изменения могут улучшить общую структуру и функциональность нашей библиотеки, открывая путь к большей гибкости и возможности повторного использования по мере роста нашего проекта.