---
title: "Завантаження"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
Як тільки допоміжна функція розростається, перенесіть її в окремий файл бібліотеки. Це зосереджує плагін і робить помічника придатним для повторного використання в кількох плагінах.

### Створення файлу бібліотеки

Візьміть функцію `send-message` і створіть новий файл з її вмістом. Збережіть його в репозиторії, а не в папці плагінів — наприклад, на верхньому рівні:

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: основний каталог для коду Scheme.
  - **library/**: тут живуть спільні функції, наприклад `send-message.scm`.
  - **plug-ins/**: тут зберігаються окремі плагіни.
    - **hello-world/**: папка для плагіна «Hello World!».
      - **hello-world.scm**: скрипт плагіна.

Приклад бібліотечної функції `send-message.scm`:

```scheme
;; Надсилання повідомлення на різні виходи
(define (send-message message output)
  (cond
    ;; Консоль повідомлень
    ((eq? output 'error-console)
       ;; Обробник — консоль повідомлень
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Діалогове вікно GUI
    ((eq? output 'gui)
       ;; Обробник — діалог GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Вікно терміналу
    ((eq? output 'terminal)
       ;; Вивід у термінал через display
       (display message)))

  ;; Повернути обробник за замовчуванням — консоль повідомлень
  (lumi-message-set-handler 2))
```

### Завантаження бібліотечної функції

Бібліотеку можна завантажити командою Scheme `load`:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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

«Hello world!» тепер стисліший і читається без коментарів — задовільний результат рефакторингу.
