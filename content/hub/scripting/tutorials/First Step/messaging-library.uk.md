---
title: "Бібліотека обміну повідомленнями"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
translation_lock: true
url: "hub/scripting/tutorials/First Step/messaging-library"
---
З часом єдина функція надсилання повідomлень перетворилася на набір пов’язаних функцій. Тепер вони формують **бібліотеку повідomлень** для виводу в GUI, консоль повідomлень і термінал ОС.

### Навіщо бібліотека повідomлень?

Зі зростанням потреб обробка кількох виходів потребує модульного підходу. Замість однієї функції «на все» процес розбито на багаторазові компоненти. Бібліотеку можна використовувати як загальний інструмент обміну повідомленнями в інших модулях і плагінах.

### Що входить до бібліотеки?

Наразі бібліотека містить:

- **send-to-gui**: надсилання в діалогове вікно Lumi.
- **send-to-error-console**: надсилання в консоль повідomлень Lumi.
- **send-to-terminal**: надсилання у вікно терміналу.
- **send-message**: диспетчер, що маршрутизує повідомлення на відповідний вихід.
- **validate-message**: перевірка валідності повідомлення та виходу перед надсиланням.

### Розширення бібліотеки

Бібліотеку легко розширити новими виходами, наприклад:

- **send-to-file**: запис повідomлень у файл журналу.
- **send-to-logger**: інтеграція із зовнішньою системою логування.
- **send-to-notification**: системні сповіщення.

Дотримуючись модульного дизайну, бібліотека може стати комплексним інструментом для всіх задач обміну повідомленнями.

## Переваги

- **Повторне використання**: функції працюють у різних плагінах і проєктах.
- **Модульність**: кожна функція виконує одне завдання — легше підтримувати та розширювати.
- **Узгодженість**: спільні перевірки та обробка дають передбачувану поведінку.

**Бібліотека повідomлень** — початок ширшої структури для керування повідомленнями в проєкті. Нові плагіни можуть підключатися до неї для надсилання повідomлень куди завгодно.

Можна налаштувати структуру файлів:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

І оновити `load` у головному плагіні:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

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
