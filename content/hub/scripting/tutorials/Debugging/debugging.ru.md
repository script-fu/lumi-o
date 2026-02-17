---
title: "Отладка"
type: docs
weight: 5
---
В сценариях ни одна функция не является непогрешимой. Даже самые надежные команды могут дать сбой при столкновении с неожиданными входными данными или условиями. Чтобы защититься от этого, мы можем реализовать собственную систему отладки и применить методы защитного программирования. Обогащая стандартные функции механизмами обработки ошибок и предоставляя информативную обратную связь, мы можем сделать наши сценарии более надежными и упростить устранение неполадок.

Ключевой частью этой стратегии является использование глобального флага отладки для управления подробным выводом, что позволяет нам включать подробную отладочную информацию, когда это необходимо, сохраняя при этом вывод чистым во время обычного выполнения.

## Флаг глобальной отладки

Глобальный флаг отладки — это простой, но эффективный способ контролировать уровень вывода информации во время выполнения сценария. Когда он включен, он предоставляет подробные сообщения отладки, которые могут оказаться неоценимыми для отслеживания проблем. Если этот параметр отключен, выходные данные остаются краткими для использования в производстве.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

По умолчанию отладка отключена. Чтобы включить подробный вывод во время разработки, просто установите флаг `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

Мы также можем временно включить или отключить отладку для определенных разделов кода, используя вспомогательные функции.

### Локальный контроль отладки

Для более точного управления мы можем включать или отключать отладку в определенных частях сценария с помощью вспомогательных функций.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Это позволяет нам динамически управлять отладкой:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Система сообщений об отладке

Чтобы эффективно обрабатывать выходные данные отладки в Scheme, мы используем структурированный подход, включающий несколько вспомогательных функций. Эти функции гарантируют, что отладочные и предупреждающие сообщения будут четкими, читаемыми и удобными в обслуживании.

### Обзор системы сообщений об отладке

Наша система обмена сообщениями отладки состоит из следующих компонентов:

1. `debug-message` — отображает сообщения отладки, когда отладка включена.
2. `serialize-item` — преобразует различные типы данных схемы в строковое представление.
3. `concat` – объединяет несколько элементов в одну строку.
4. `list->string` — форматирует список в читаемую строку.
5. `message` — отображает вывод в консоли сообщений Lumi.
6. `warning-message` – отображает предупреждающие сообщения, когда предупреждения включены.

Каждая функция играет роль в форматировании и отображении структурированных сообщений.

---

### Функция сообщения отладки

Функция `debug-message` — это основной метод отображения выходных данных отладки. Это гарантирует, что сообщения будут отображаться только при включенной отладке.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- Условие `when debug` гарантирует, что сообщения будут появляться только при включенной отладке.
- Для ясности сообщения начинаются с префикса `"> "`.
- Функция использует `concat` для форматирования содержимого сообщения.
- Наконец, он вызывает `message`, чтобы отправить вывод на консоль сообщений Lumi.

Пример использования:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

При включенной отладке вывод может быть таким:

```scheme
> item: background-layer has tree position : 3
```

### Сериализация данных для отладочных сообщений

Сообщения могут содержать различные типы данных, такие как списки, векторы и числа. Чтобы убедиться, что они правильно отформатированы, мы используем `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Пример использования:

```scheme
(serialize-item '(1 2 3))
```

Выход:

```scheme
list:
1
2
3
```

### Объединение сообщений

Чтобы объединить несколько компонентов сообщения в одну строку, мы используем `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Пример использования:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Форматирование списков как строк

Функция `list->string` преобразует список в форматированную строку.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Предупреждающие сообщенияФункция `warning-message` работает аналогично `debug-message`, но отображает предупреждения, даже если отладка отключена.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Обеспечивает отображение сообщений только при включенных предупреждениях (флаг `warning` установлен в `common.scm` как `#t`).
- Вызывает `concat` для форматирования содержимого сообщения.
- Использует `message` для отправки вывода в Lumi.

## Улучшение стандартных функций

После создания системы отладки мы можем расширить нашу библиотеку функций, включив в нее подробные сообщения. Это дает представление о состояниях элементов, значениях переменных и вызовах функций.

Типичным примером является `item-is-valid?`, который оборачивает `lumi-item-id-is-valid` для возврата `#t` или `#f`. Если возвращается `#f`, мы можем вызвать `warning-message` в вызывающем коде. Если входные данные не являются числом, мы можем выдать предупреждение в функции.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Практическое использование

При разработке подключаемых модулей Scheme подобная упаковка функций значительно сокращает время отладки и обеспечивает надежный и удобный в сопровождении код. Имея нашу систему отладки, мы можем генерировать структурированный поток отладки в консоли ошибок одним щелчком переключателя.

В этом потоке отладки вызовы функций помечены звездочкой (*), что упрощает отслеживание выполнения скриптов и выявление сбоев, особенно в сложных плагинах. Эта видимость помогает нам понять поток операций и эффективно диагностировать непредвиденное поведение.

Обертка для нашей функции сообщения, использующая `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Пример использования `call` на практике:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Пример потока отладки при выполнении плагина:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Этот структурированный журнал обеспечивает четкую временную шкалу вызовов функций и изменений данных, что значительно упрощает отладку и анализ производительности.

## Заключение

Внедряя структурированную систему отладки, мы создаем более безопасные и удобные в обслуживании сценарии, которые позволяют в режиме реального времени получать информацию об их выполнении.

### Ключевые выводы

- **Управление подробностями** – используйте глобальный флаг отладки для управления уровнями вывода.
- **Обеспечьте четкую обратную связь** – оберните стандартные функции информативными сообщениями отладки.
- **Повышение надежности** – корректно обрабатывайте неожиданные входные данные, чтобы предотвратить ошибки.
- **Упрощение устранения неполадок**. Структурированные отладочные сообщения упрощают диагностику и устранение проблем.

Благодаря такому подходу наши сценарии эффективно «объясняют себя» при обработке данных, уменьшая разочарование и повышая эффективность рабочего процесса. Отладка становится упреждающим инструментом, а не реактивной рутинной работой, что делает процесс написания сценариев более плавным и более полезным.