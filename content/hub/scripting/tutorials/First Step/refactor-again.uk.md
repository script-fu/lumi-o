---
title: "Рефакторинг знову"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactor-again"
---
У міру зростання допоміжної бібліотеки за нею важче стежити з першого погляду. Рефакторіть знову, щоб кожна функція була малою та одноцільовою.

### Подолання складності

Щоб функцію було легше відстежувати та підтримувати, розбийте її на менші цілеспрямовані частини. Почніть із відділення перевірки від маршрутизації повідомлень.

### Функція перевірки

Частину, що перевіряє аргументи `message` і `output`, можна винести в окрему функцію. Тоді `send-message` не займається перевіркою — її легше читати.

```scheme
(define (validate-message message output)
  ;; Перевірити, що message — непорожній рядок
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Перевірити, що output — один із очікуваних пунктів призначення
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Спрощення send-message

Після винесення перевірки `send-message` зосереджується лише на надсиланні повідомлення.

```scheme
(define (send-message message output)
  ;; Перевірити перед виконанням
  (validate-message message output)

  (cond
    ;; Консоль повідomлень
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Діалогове вікно GUI
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Вікно терміналу
    ((eq? output 'terminal)
       (display message)))

  ;; Повернути обробник за замовчуванням — консоль повідomлень
  (lumi-message-set-handler 2))
```

### Окремі обробники виходу

Кожен тип виводу (GUI, консоль повідomлень, термінал) можна винести в окрему функцію — простіше тестувати, змінювати та розширювати.

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
  ;; Надіслати на відповідний вихід
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Повернути обробник за замовчуванням — консоль повідomлень
  (lumi-message-set-handler 2))
```

### Перевірка в кожній send-* функції

Оскільки перевірка важлива, кожна `send-*` функція може виконувати власну валідацію — незалежно від того, який вихід викликається.

```scheme
(define (send-to-gui message)
  ;; Перевірити перед виконанням
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Перевірити перед виконанням
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Перевірити перед виконанням
  (validate-message message 'terminal)
  (display message))
```

Перевірку прибрано з `send-message` і перенесено ближче до кожного виходу. `send-message` стає _диспетчером_, а кожна `send-*` перевіряє вхідні дані самостійно.

Перемістивши перевірку в кожну `send-to-*`, функції стають автономними: `send-to-gui`, `send-to-error-console` або `send-to-terminal` можна викликати напряму, без диспетчера.

## Переваги рефакторингу

- **Чіткий розподіл завдань**: кожна функція має одну відповідальність.
- **Розширюваність**: новий вихід — нова функція (наприклад, `send-to-file`) і запис у `cond`.
- **Повторне використання**: обробники виходу можна використовувати в інших частинах проєкту або плагінах.
- **Узгодженість**: `validate-message` у кожній `send-to-*` гарантує перевірку всіх виходів.

Перероблена бібліотека:

```scheme
;; Надсилання повідомлення в діалогове вікно GUI
(define (send-to-gui message)
  ;; Перевірити перед виконанням
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Надсилання в консоль повідomлень
(define (send-to-error-console message)
  ;; Перевірити перед виконанням
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Надсилання у вікно терміналу
(define (send-to-terminal message)
  ;; Перевірити перед виконанням
  (validate-message message 'terminal)
  (display message))

;; Маршрутизація повідомлення на відповідний вихід
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Повернути обробник за замовчуванням — консоль повідomлень
  (lumi-message-set-handler 2))

;; Перевірка непорожнього рядка message та допустимого output
(define (validate-message message output)
  ;; Перевірити, що message — непорожній рядок
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Перевірити, що output — один із очікуваних пунктів призначення
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Це все? Ні — попереду ще кроки.
