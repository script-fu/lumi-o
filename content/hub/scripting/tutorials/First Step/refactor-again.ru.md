---
title: "Снова рефакторинг"
type: docs
weight: 5
---
По мере роста вспомогательной библиотеки становится все труднее отслеживать ее с первого взгляда. Снова проведите рефакторинг, чтобы каждая функция была маленькой и одноцелевой.

### Разрушение сложности

Чтобы облегчить отслеживание и поддержку функции, разбейте ее на более мелкие и целенаправленные функции. Начните с отделения проверки от маршрутизации сообщений.

### Создайте функцию проверки

Мы можем взять часть функции, которая проверяет аргументы `message` и `output`, и переместить ее в отдельную функцию. Таким образом, основной функции `send-message` не нужно беспокоиться о проверке, что упрощает отслеживание.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Упростите отправку сообщений

Теперь, когда проверка вынесена в отдельную функцию, функция `send-message` может сосредоточиться только на отправке сообщения. Это будет намного проще, поскольку он решает только конкретную задачу — направить сообщение в правильный пункт назначения.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

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
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Далее: отдельный обработчик вывода

Каждый тип вывода сообщений (графический интерфейс, консоль ошибок, терминал) можно переместить в отдельную функцию. Это упрощает тестирование, модификацию и потенциальное расширение в будущем.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Повторное использование проверки в каждой функции отправки

Поскольку проверка является важной частью обеспечения правильности как сообщения, так и вывода, имеет смысл для каждой функции `send-*` выполнять собственную проверку. Это гарантирует, что независимо от того, какой выход вызывается, мы всегда сначала проверяем входы.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Обратите внимание, что мы удалили проверку из функции отправки сообщения и переложили ответственность на каждую отдельную функцию вывода. Это изменение гарантирует, что каждый пункт назначения (графический интерфейс пользователя, консоль ошибок, терминал) выполняет собственную проверку, оптимизируя функцию отправки сообщения и сохраняя логику проверки ближе к тому месту, где это необходимо.

Этот подход может упростить функцию отправки сообщения, сделав ее _диспетчером_, гарантируя при этом, что каждая функция отправки-* правильно проверяет сообщение перед обработкой.

Перенеся проверку в каждую функцию send-to-*, мы сделали их повторно используемыми как автономные функции. Это означает, что мы можем вызывать любую функцию отправки в графический интерфейс, отправку на консоль ошибок или отправку на терминал напрямую, не полагаясь на функцию диспетчера отправки сообщений. Каждая из этих функций теперь полностью обрабатывает свою собственную логику и может использоваться независимо в других частях кода или в других плагинах, что делает ваш код более модульным и гибким.

## Преимущества рефакторинга

- **Четкое разделение задач**: каждая функция теперь выполняет только одну ответственность, что упрощает понимание кода.
- **Расширяемость**: добавлять новые типы вывода очень просто. Вы просто определяете новую функцию, например `send-to-file` или `send-to-logger`, а затем добавляете регистр в оператор `cond`.
- **Повторное использование**: каждую из этих функций обработки вывода можно повторно использовать в другом месте вашего проекта или использовать в нескольких плагинах.
- **Последовательность**: повторно используя функцию проверки в каждой функции `send-to-*`, вы гарантируете, что все выходные данные проверены правильно, что делает код более надежным.

Переработанная версия библиотеки:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Это все, что мы можем сделать? Нет! еще многое предстоит сделать, пожалуйста, читайте дальше.