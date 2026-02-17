---
title: "Плагин фильтра"
type: docs
weight: 2
---
Для урока [First Step](../../first-step/) мы использовали плагин _procedure_. Эти типы плагинов работают без необходимости изображения или рисования в качестве входных данных. Обычно мы используем плагин для изменения изображения и его элементов рисования. Подобные плагины называются плагинами _filter_.

### Что такое Drawable?

**Рисуемый** в Lumi относится к элементу изображения, на котором можно рисовать, например, к слою или каналу. Плагины фильтров обычно работают на этих элементах.

### Пример простого плагина фильтра

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Скопируйте текст и сохраните его как `simple-filter-plug-in.scm` в папке `simple-filter-plug-in` в одной из папок плагинов Lumi. Папка плагинов Lumi — это _любая_ папка, указанная в:
 **Lumi > Редактировать > Настройки > Папки > Плагины**

В Linux щелкните правой кнопкой мыши файл `simple-filter-plug-in.scm`, выберите **Свойства > Разрешения** и установите флажок **Разрешить выполнение файла как программы**. Как только файл окажется в нужном месте, станет исполняемым и не будет содержать синтаксических ошибок, при перезапуске Lumi он появится в верхней строке заголовка меню внутри меню под названием **Плагин**.

### Запуск плагина

1. Откройте изображение (для работы этого плагина фильтра требуется изображение).
2. Откройте **Окна > Закрепляемые диалоги > Консоль ошибок**, чтобы увидеть сообщение.
3. Выберите **Демонстрация подключаемого модуля простого фильтра** в меню **Плагин**.
4. Цвета одного из выбранных слоев будут инвертированы, и на консоли ошибок будет выведено сообщение.

### Редактирование плагина

Вы можете настроить плагин, отредактировав его файл `.scm`. Например, чтобы изменить отображаемое сообщение:

1. Откройте файл и найдите строку, определяющую `message`.
2. Замените `"hello, world"` своим собственным текстом.
3. Сохраните файл.

В Lumi версии 3 плагины не требуют обновления, чтобы сохраненные изменения вступили в силу. Просто перезапустите плагин, чтобы увидеть обновленное сообщение.

### Плагин «Экспертиза»

#### Линия Шебанг

Первая строка гарантирует, что скрипт работает как плагин в Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### Определение процедуры

Процедура принимает два аргумента: активное изображение и выбранные объекты рисования.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Основная логика

Оператор `let` определяет переменную и выполняет операции с объектом рисования.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### Регистрация плагина

Плагин зарегистрирован в Lumi как плагин фильтра:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### Меню Регистрация
Эта строка определяет расположение меню для плагина:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Устранение неполадок

Если плагин не отображается, проверьте его расположение, имя и свойство исполняемого файла.

Местоположение должно находиться в пути поиска подключаемого модуля.
Имя файла должно совпадать с именем содержащей его папки.
Файл должен быть установлен как исполняемый.


**Консоль ошибок** — ценный инструмент для устранения неполадок пользовательских плагинов. Если ваш плагин работает не так, как ожидалось, проверьте здесь сообщения об ошибках или журналы. Окно **Терминал** также может предоставлять отладочную информацию и сообщать о проблемах с загрузкой.