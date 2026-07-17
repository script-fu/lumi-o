---
title: "Налагодження"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
url: "hub/scripting/tutorials/debugging"
translation_lock: true
---
У скриптах жодна функція не безпомилкова. Навіть надійні команди можуть зазнати невдачі через неочікувані вхідні дані чи умови. Захист — власна система налагодження та захисне програмування: обгортки з обробкою помилок і інформативний зворотний зв’язок роблять скрипти стійкішими та простішими для діагностики.

Ключова частина — глобальний прапорець налагодження для керування деталізацією виводу: увімкнути докладні повідомлення під час розробки і тримати вивід стислим під час звичайного виконання.

## Глобальний прапорець налагодження

Простий спосіб контролювати обсяг інформації під час виконання скрипта. Увімкнений — детальні повідомлення для пошуку проблем; вимкнений — лаконічний вивід.

```scheme
;; Призначення: глобальний прапорець для debug-виводу
(define debug #f)
```

За замовчуванням налагодження вимкнено. Для детального виводу під час розробки встановіть `#t`:

```scheme
;; Призначення: глобальний прапорець для debug-виводу
(define debug #t)
```

Можна тимчасово вмикати або вимикати налагодження в окремих ділянках коду.

### Локальне керування налагодженням

Для точнішого контролю — допоміжні функції:

```scheme
;; Призначення: вимкнути debug для ділянки коду
(define (debug-off)
  (set! debug #f))

;; Призначення: увімкнути debug для ділянки коду
(define (debug-on)
  (set! debug #t))
```

Динамічне керування:

```scheme
(debug-on)  ;; Увімкнути детальний вивід

;; Логіка скрипта

(debug-off) ;; Вимкнути детальний вивід
```

## Система налагоджувальних повідomлень

Для debug-виводу в Scheme — структурований підхід із кількома помічниками. Повідomлення мають бути зрозумілими та підтримуваними.

### Огляд системи

Компоненти:

1. `debug-message` – debug-повідомлення, коли налагодження увімкнено.
2. `serialize-item` – перетворення типів Scheme у рядок.
3. `concat` – об’єднання кількох елементів у один рядок.
4. `list->string` – форматування списку як читабельного рядка.
5. `message` – вивід у консоль повідomлень Lumi.
6. `warning-message` – попередження, коли увімкнено warnings.

---

### debug-message

Основний спосіб показати debug-вивід — лише коли налагодження увімкнено.

```scheme
;; Призначення: вивести debug-повідомлення
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug` — повідомлення лише при увімкненому debug.
- Префікс `"> "` для наочності.
- `concat` форматує вміст.
- `message` надсилає вивід у консоль повідomлень Lumi.

Приклад:

```scheme
;; Призначення: позиція елемента в дереві або #f якщо недійсний
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

При увімкненому debug:

```scheme
> item: background-layer has tree position : 3
```

### Серіалізація даних

Повідomлення можуть містити списки, вектори, числа. Для форматування — `serialize-item`:

```scheme
;; Призначення: перетворити типи Scheme (списки, вектори, пари тощо)
;;              у рядкове представлення
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Порожній список
    ((and (string? item) (string=? item "")) "\"\"")  ; Порожній рядок
    ((list? item) (list->string item))                ; Вкладений список
    ((vector? item)                                   ; Вектори
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Пари
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Числа
    ((symbol? item) (symbol->string item))            ; Символи
    ((boolean? item) (if item "#t" "#f"))             ; Логічні значення
    ((string? item) item)                             ; Рядки
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Приклад:

```scheme
(serialize-item '(1 2 3))
```

Вивід:

```scheme
list:
1
2
3
```

### concat

Об’єднання частин повідомлення:

```scheme
;; Призначення: об’єднати кілька елементів в один рядок
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Приклад:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### list->string

```scheme
;; Призначення: перетворити список у читабельний рядок
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### warning-message

Подібно до `debug-message`, але попередження показуються навіть при вимкненому debug:

```scheme
;; Призначення: вивести попередження
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Повідomлення лише коли увімкнено warnings (прапорець `warning` у `common.scm` як `#t`).
- `concat` форматує вміст.
- `message` надсилає вивід у Lumi.

## Покращення стандартних функцій

Після системи налагодження можна доповнити бібліотеку докладними повідомленнями — стан елементів, значення змінних, виклики функцій.

Приклад — `item-is-valid?`, обгортка навколо `lumi-item-id-is-valid`, що повертає `#t` або `#f`. При `#f` можна викликати `warning-message`; якщо вхід не число — попередження всередині функції.

```scheme
;; Призначення: перевірити валідність елемента; повертає #t або #f
;;              Попереджає, якщо item не число
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Практичне використання

При розробці плагінів Scheme такі обгортки скорочують час налагодження. З системою debug можна отримати структурований потік у консолі помилок одним перемикачем.

У потоці виклики функцій позначені зірочкою (*), що полегшує відстеження виконання в складних плагінах.

Обгортка для позначення викликів:

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Приклад:

```scheme
;; Призначення: застосувати текстуру до списку масок груп
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

Приклад debug-потоку під час виконання плагіна:

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

Структурований журнал дає часову шкалу викликів і змін даних — простіше налагодження та аналіз продуктивності.

## Висновок

Структурована система налагодження робить скрипти безпечнішими та зручнішими в підтримці з real-time уявленням про виконання.

### Ключові висновки

- **Контроль деталізації** — глобальний прапорець debug для рівнів виводу.
- **Зрозумілий зворотний зв’язок** — обгортки стандартних функцій із інформативними повідомленнями.
- **Стійкість** — акуратна обробка неочікуваних вхідних даних.
- **Простіша діагностика** — структуровані debug-повідомлення.

З таким підходом скрипти «пояснюють себе» під час роботи — менше розчарувань, ефективніший робочий процес. Налагодження стає проактивним інструментом, а не реактивною рутиною.
