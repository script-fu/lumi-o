---
title: "Возвращаемые значения"
type: docs
weight: 8
---
Возвращаемые значения имеют значение, поскольку они позволяют вам управлять потоком без дополнительных состояний. В Scheme возвращаемым значением становится последнее вычисленное выражение.

На этой странице используются помощники проверки из примера обмена сообщениями, чтобы показать, как явные возвращаемые значения упрощают компоновку кода.

### Что такое возвращаемое значение?

В Scheme возвращаемое значение функции определяется последним выражением, которое оценивает функция. Это означает, что все, что вычисляет последняя строка кода в функции, будет возвращено как результат функции. Если значение явно не возвращается, функция возвращает `#f` (false) или `undefined`.

Давайте вернемся к функции проверки (is-valid-string?).

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

В этой функции, если сообщение недействительно, выдается ошибка. Однако если сообщение действительно, явное возвращаемое значение не указывается, и функция по умолчанию возвращает `#f`.

### Явное выражение возвращаемых значений

Мы можем улучшить это, сделав возвращаемое значение более явным. Например, мы могли бы вернуть `#t` (true), если сообщение действительно:

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

В этой версии функция вернет `#t`, если сообщение действительно, обеспечивая четкий результат. Это позволяет более гибко использовать функцию в других контекстах, где требуется логический результат.

### Эффективное использование возвращаемых значений

Решая, что возвращают наши функции, мы можем сделать их более предсказуемыми и полезными. Возврат таких значений, как `#t`, `#f` или конкретного результата, дает нам больше контроля над тем, как функция взаимодействует с остальной частью кода. Например, вы можете использовать возвращаемое значение для принятия дальнейших решений в вызывающей функции или передать его в качестве аргумента другой функции.

Вот простой пример использования возвращаемого значения для управления потоком логики:

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

В этом случае (send-message) полагается на возвращаемое значение (is-valid-output-display?), чтобы решить, продолжать ли.
Условный оператор `cond` будет пропущен, если первый тест не пройден. Кроме того, обратите внимание, как это читается довольно естественно, если корректно отображать выходные данные?

## Логика оператора if в схеме

Прежде чем приступить к примеру с реорганизованной библиотекой, приведем краткий обзор условной логики. Схема использует `if` для выбора между двумя путями.

Вот простая форма оператора `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Эта структура проверяет условие и, если условие истинно, выполняет первое действие. Если условие ложно, выполняется второе действие.

В случаях, когда вам необходимо выполнить несколько действий, когда условие истинно или ложно, вы можете использовать `begin`, чтобы сгруппировать их вместе:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Это позволяет обрабатывать более сложные ситуации, когда в зависимости от результата условной проверки необходимо выполнить несколько выражений или операторов.

Хорошо, вот код библиотеки со встроенными возвращаемыми значениями, используемый для управления процессом выполнения.

### Рефакторинг с возвращаемыми значениями

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

Возвращаемые значения являются фундаментальной частью обеспечения гибкости и возможности повторного использования функций. Тщательно решая, что должна возвращать каждая функция, мы можем гарантировать, что наши функции хорошо взаимодействуют друг с другом и предоставляют полезную информацию для остальной части кода. Будь то возврат `#t` или `#f` или что-то более конкретное, возвращаемые значения дают нам возможность контролировать поток наших программ и обрабатывать различные результаты.