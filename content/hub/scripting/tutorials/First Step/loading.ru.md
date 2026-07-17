---
title: "Загрузка"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
Как только вспомогательная функция вырастет, переместите ее в небольшой библиотечный файл. Это сохраняет фокус плагина и позволяет повторно использовать помощник в нескольких плагинах.

### Создаем библиотечную функцию

Мы можем взять функцию отправки сообщения и создать новый файл с ее содержимым. Сохраните файл в папке репозитория, а не в части плагинов, возможно, на верхнем уровне;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: это ваш основной каталог для хранения кода Scheme.
  - **library/**: здесь живут общие функции, такие как `send-message.scm`.
  - **плагины/**: здесь хранятся ваши отдельные плагины.
    - **hello-world/**: Папка для plug-in «Hello World!».
      - **hello-world.scm**: Файл скрипта plug-in.

Пример библиотечной функции send-message.scm

```scheme
;; Функция для вывода сообщений в различные места назначения
(define (send-message message output)
  (cond
    ;; Отправить в консоль сообщений
    ((eq? output 'error-console)
       ;; Установить обработчик на консоль сообщений
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

  ;; Восстановить обработчик сообщений по умолчанию для консоль сообщений
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