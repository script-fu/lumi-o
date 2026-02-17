---
title: "Загрузка"
type: docs
weight: 3
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

- **scheme/**: это ваш основной каталог для хранения кода схемы.
  - **library/**: здесь живут общие функции, такие как `send-message.scm`.
  - **плагины/**: здесь хранятся ваши отдельные плагины.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Пример библиотечной функции send-message.scm

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Загрузите библиотечную функцию

Мы можем загрузить эту библиотечную функцию с помощью команды Scheme `load`;

Загрузка файла библиотеки:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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