---
title: "Загрузка"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
As soon as a helper function grows, move it into a small library file. That keeps the plug-in focused and makes the helper reusable across multiple plug-ins.

### Создаем библиотечную функцию

Мы можем взять функцию отправки сообщения и создать новый файл с ее содержимым. Save the file into your repo folder, not the plugins part, perhaps near the top level;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: это ваш основной каталог для хранения кода схемы.
  - **library/**: здесь живут общие функции, такие как `send-message.scm`.
  - **plug-ins/**: This is where your individual plug-ins are stored.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Пример библиотечной функции send-message.scm

```scheme
;; Функция для вывода сообщений в различные места назначения
(define (send-message message output)
  (cond
    ;; Отправить в Message console
    ((eq? output 'error-console)
       ;; Установить обработчик на Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Отправить в диалоговое окно GUI
    ((eq? output 'gui)
       ;; Установить обработчик на диалог GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Отправить в окно терминала
    ((eq? output 'terminal)
       ;; Вывод terminal обрабатывается с помощью display
       (display message)))

  ;; Восстановить обработчик сообщений по умолчанию для Message console
  (lumi-message-set-handler 2))
```

### Загрузите библиотечную функцию

Мы можем загрузить эту библиотечную функцию с помощью команды Scheme `load`;

Загрузка файла библиотеки:

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

Эй! Теперь у нас есть что-то более простое и короткое для чтения, которое описывает себя без комментариев. Это удовлетворительный вывод рефакторинга.