---
title: "Перевірка"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
translation_lock: true
url: "hub/scripting/tutorials/First Step/validation"
---
Під час створення надійних плагінів важливо, щоб функції коректно обробляли помилки та працювали навіть при неправильному використанні або неочікуваних вхідних даних. Перевірка захищає цілісність функції та запобігає збоям і небажаній поведінці.

Розглянемо, як покращити функцію `send-message`, додавши перевірки вхідних даних.

### Перевірка аргументів

Перед надсиланням повідомлення переконайтеся, що аргумент `output` у `send-message` дійсний. Можна перевірити, що пункт призначення — один із очікуваних (`gui`, `error-console` або `terminal`).

Приклад:

```scheme
(define (send-message message output)
  ;; Перевірити аргумент output
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Консоль повідомлень
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Діалогове вікно GUI
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Вікно терміналу
      ((eq? output 'terminal)
         (display message))))

  ;; Повернути обробник за замовчуванням — консоль повідомлень
  (lumi-message-set-handler 2))
```

Тут `member` перевіряє, чи `output` допустимий. Якщо ні — функція викликає `error` із зрозумілим повідомленням.

### Обробка порожніх повідомлень

Також корисно перевірити аргумент `message`. Якщо передано порожній рядок або `#f`, функція має відреагувати акуратно.

Приклад:

```scheme
(define (send-message message output)
  ;; Перевірити, чи повідомлення не порожнє
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

Це гарантує валідні вхідні дані та підвищує надійність функції.

### Комбінований приклад перевірки

```scheme
;; Надсилання повідомлення на різні виходи
(define (send-message message output)

  ;; Перевірити message та output
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Консоль повідомлень
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Діалогове вікно GUI
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Вікно терміналу
        ((eq? output 'terminal)
           (display message)))))

  ;; Повернути обробник за замовчуванням — консоль повідомлень
  (lumi-message-set-handler 2))
```

У цій версії:

- спочатку перевіряється, чи `message` не порожній і є рядком;
- потім — чи `output` один із допустимих (`gui`, `error-console`, `terminal`);
- якщо обидві перевірки пройдені, повідомлення надсилається; інакше — `error` із поясненням.

Ця комбінована перевірка тримає код чистим і гарантує перевірку обох аргументів перед діями. Зверніть увагу: ми також закладаємо основу системи налагоджувальних повідомлень. Коли код падає, ми отримуємо причину — ту, яку написали самі.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```
