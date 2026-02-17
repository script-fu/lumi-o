---
title: "Валидация"
type: docs
weight: 4
---
При создании надежных плагинов важно убедиться, что наши функции корректно обрабатывают ошибки и работают должным образом, даже в случаях неправильного использования или неожиданных входных данных. Проверка помогает защитить целостность функции и предотвратить сбои или непреднамеренное поведение.

Давайте посмотрим, как мы можем улучшить функцию `send-message`, добавив проверки, чтобы убедиться, что она правильно обрабатывает входные данные.

### Проверка входных данных

Прежде чем отправлять сообщение, мы должны убедиться, что аргумент `output`, переданный функции `send-message`, действителен. Мы можем добавить проверку, чтобы подтвердить, что место назначения вывода является одним из ожидаемых значений (gui, консоль ошибок или терминал).

Пример:

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

В этом примере мы используем `member`, чтобы проверить допустимость аргумента `output`. В противном случае функция выдает ошибку с четким сообщением, предотвращая возникновение проблем с недопустимыми значениями.

### Обработка пустых сообщений

Также полезно убедиться, что аргумент `message` действителен. Например, если в качестве сообщения передается пустая строка или #f (false), функция должна обработать это корректно.

Пример обработки пустого сообщения:

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

Такой подход гарантирует, что функция всегда получает действительные входные данные, что повышает ее надежность и предотвращает непредвиденное поведение.

### Пример комбинированной проверки

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

В этой версии:
- Функция сначала проверяет, является ли `message` пустым или недействительным. Если сообщение действительно, происходит проверка, является ли `output` одним из принятых значений (`gui`, `error-console` или `terminal`).
- Если обе проверки пройдены, сообщение отправляется на соответствующий выход. В противном случае выдается сообщение об ошибке с четким объяснением.
- Выполняется дополнительная проверка, чтобы убедиться, что сообщение также является строкой.

Эта комбинированная функция проверки обеспечивает чистоту кода и гарантирует, что оба входных данных проверяются перед выполнением каких-либо действий, что делает функцию более надежной. Обратите внимание: мы также создаем систему обмена сообщениями отладки. Когда
код терпит неудачу, мы получаем причину, причину, которую мы написали сами.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```