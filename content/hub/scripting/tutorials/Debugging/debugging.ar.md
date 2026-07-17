---
title: "تصحيح الأخطاء"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
url: "hub/scripting/tutorials/debugging"
translation_lock: true
---
في البرمجة النصية، لا توجد وظيفة معصومة من الخطأ. حتى الأوامر الأكثر موثوقية قد تفشل أمام مدخلات أو شروط غير متوقعة. للحماية، يمكننا تنفيذ نظام تصحيح أخطاء مخصص واعتماد تقنيات برمجة دفاعية. بتغليف الوظائف القياسية بآليات معالجة أخطاء وتقديم تغذية راجعة مفيدة، نجعل سكربتاتنا أكثر متانة وأسهل في استكشاف الأخطاء وإصلاحها.

جزء أساسي من هذه الاستراتيجية هو علامة تصحيح أخطاء عامة للتحكم في المخرجات المطولة، فتمكّن معلومات تصحيح تفصيلية عند الحاجة مع إبقاء المخرجات نظيفة أثناء التشغيل العادي.

## علامة التصحيح العامة

علامة التصحيح العامة طريقة بسيطة وفعالة للتحكم في مستوى إخراج المعلومات أثناء تنفيذ السكربت. عند تمكينها، توفر رسائل تصحيح تفصيلية قيمة لتتبع المشكلات. عند تعطيلها، تبقي المخرجات موجزة للاستخدام في الإنتاج.

```scheme
;; الغرض: علامة عامة للتحكم في مخرجات التصحيح.
(define debug #f)
```

بشكل افتراضي، تصحيح الأخطاء متوقف. لتمكين المخرجات المطولة أثناء التطوير، عيّن العلامة على `#t`:

```scheme
;; الغرض: علامة عامة للتحكم في مخرجات التصحيح.
(define debug #t)
```

يمكننا أيضًا تمكين أو تعطيل التصحيح مؤقتًا لأجزاء معينة من الكود باستخدام وظائف مساعدة.

### التحكم المحلي في التصحيح

لتحكم أفضل، نشغّل أو نوقف التصحيح داخل أجزاء معينة من السكربت:

```scheme
;; الغرض: إيقاف وضع التصحيح لقسم من الكود.
(define (debug-off)
  (set! debug #f))

;; الغرض: تشغيل وضع التصحيح لقسم من الكود.
(define (debug-on)
  (set! debug #t))
```

يتيح ذلك التحكم في التصحيح ديناميكيًا:

```scheme
(debug-on)  ;; تمكين المخرجات المطولة

;; منطق السكربت هنا

(debug-off) ;; تعطيل المخرجات المطولة
```

## نظام رسائل التصحيح

للتعامل بكفاءة مع مخرجات تصحيح الأخطاء في Scheme، نستخدم أسلوبًا منظمًا يضم وظائف مساعدة متعددة. تضمن هذه الوظائف أن رسائل التصحيح والتحذير واضحة وقابلة للقراءة والصيانة.

### نظرة عامة على نظام رسائل التصحيح

يتكون نظام رسائل التصحيح من:

1. `debug-message` – يعرض رسائل التصحيح عند تمكين التصحيح.
2. `serialize-item` – يحوّل أنواع بيانات Scheme المختلفة إلى تمثيل سلسلة.
3. `concat` – يسلسل عناصر متعددة في سلسلة واحدة.
4. `list->string` – ينسّق القائمة في سلسلة قابلة للقراءة.
5. `message` – يعرض المخرجات في وحدة تحكم الرسائل في Lumi.
6. `warning-message` – يعرض رسائل التحذير عند تمكين التحذيرات.

تلعب كل وظيفة دورًا في تنسيق الرسائل المنظمة وعرضها.

---

### وظيفة debug-message

`debug-message` هي الطريقة الأساسية لعرض مخرجات التصحيح. تضمن عدم ظهور الرسائل إلا عند تمكين التصحيح.

```scheme
;; الغرض: عرض رسالة تصحيح.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- شرط `when debug` يضمن ظهور الرسائل فقط عند تمكين التصحيح.
- الرسائل مسبوقة بـ `"> "` للتوضيح.
- تستخدم `concat` لتنسيق محتوى الرسالة.
- أخيرًا، تستدعي `message` لإرسال المخرجات إلى وحدة تحكم الرسائل في Lumi.

مثال على الاستخدام:

```scheme
;; الغرض: إرجاع موضع العنصر في الشجرة أو #f إذا كان العنصر غير صالح
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

مع تمكين التصحيح، قد يكون المخرج:

```scheme
> item: background-layer has tree position : 3
```

### تسلسل البيانات لرسائل التصحيح

قد تحتوي الرسائل على أنواع بيانات مختلفة مثل القوائم والvectors والأرقام. لضمان تنسيقها بشكل صحيح، نستخدم `serialize-item`.

```scheme
;; الغرض: تحويل أنواع بيانات Scheme المختلفة (قوائم، vectors، أزواج، إلخ)
;;          إلى تمثيل سلسلة.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; قائمة فارغة
    ((and (string? item) (string=? item "")) "\"\"")  ; سلسلة فارغة
    ((list? item) (list->string item))                ; قائمة متداخلة
    ((vector? item)                                   ; معالجة vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; معالجة الأزواج
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; أرقام
    ((symbol? item) (symbol->string item))            ; رموز
    ((boolean? item) (if item "#t" "#f"))             ; قيم منطقية
    ((string? item) item)                             ; سلاسل
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

مثال على الاستخدام:

```scheme
(serialize-item '(1 2 3))
```

المخرج:

```scheme
list:
1
2
3
```

### concat للرسائل

لدمج مكونات رسائل متعددة في سلسلة واحدة، نستخدم `concat`.

```scheme
;; الغرض: دمج عناصر متعددة في سلسلة واحدة.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

مثال على الاستخدام:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### تنسيق القوائم كسلاسل

`list->string` تحوّل القائمة إلى سلسلة منسّقة.

```scheme
;; الغرض: تحويل قائمة عناصر إلى سلسلة قابلة للقراءة.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### رسائل التحذير

`warning-message` تعمل بشكل مشابه لـ `debug-message`، لكنها تعرض تحذيرات حتى عند تعطيل التصحيح.

```scheme
;; الغرض: عرض رسالة تحذير.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- تضمن عدم ظهور الرسائل إلا عند تمكين التحذيرات (تُعيَّن علامة `warning` في `common.scm` إلى `#t`).
- تستدعي `concat` لتنسيق محتوى الرسالة.
- تستخدم `message` لإرسال المخرجات إلى Lumi.

## تعزيز الوظائف القياسية

بعد إنشاء نظام التصحيح، يمكننا تعزيز مكتبة الوظائف بدمج رسائل تفصيلية. يوفر ذلك رؤية لحالات العناصر والقيم المتغيرة واستدعاءات الوظائف.

مثال شائع هو `item-is-valid?`، الذي يلف `lumi-item-id-is-valid` لإرجاع `#t` أو `#f`. إذا أُعيد `#f`، يمكن تشغيل `warning-message` في موضع الاستدعاء؛ وإذا لم يكن المدخل رقمًا، يمكن إصدار تحذير في الوظيفة.

```scheme
;; الغرض: التحقق من صحة عنصر، إرجاع #t أو #f.
;;          يُصدر تحذيرًا إذا لم يكن العنصر رقمًا.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## الاستخدام العملي

عند تطوير plug-ins Scheme، يقلّل تغليف الوظائف بهذه الطريقة وقت التصحيح بشكل كبير ويضمن كودًا متينًا وقابلًا للصيانة. مع نظام التصحيح، يمكننا إنشاء تدفق تصحيح منظم في وحدة تحكم الأخطاء بنقرة واحدة.

في هذا التدفق، تُميَّز استدعاءات الوظائف بعلامة النجمة (*)، فيسهل تتبع تنفيذ السكربت وتحديد حالات الفشل، خاصة في plug-ins المعقدة. تساعدنا هذه الرؤية على فهم تدفق العمليات وتشخيص السلوك غير المتوقع بكفاءة.

غلاف لوظيفة رسائلنا لاستخدام `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

مثال على استخدام `call` عمليًا:

```scheme
;; الغرض: تطبيق عملية النسيج على قائمة أقنعة المجموعة المعطاة
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

مثال على تدفق التصحيح أثناء تنفيذ plug-in:

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

يوفر هذا السجل المنظم جدولًا زمنيًا واضحًا لاستدعاءات الوظائف وتغييرات البيانات، فيسهل التصحيح وتحليل الأداء.

## الخلاصة

بتنفيذ نظام تصحيح أخطاء منظم، ننشئ سكربتات أكثر أمانًا وقابلية للصيانة تقدم رؤى في الوقت الفعلي عن تنفيذها.

### الوجبات الرئيسية

- **التحكم في الإسهاب** – استخدم علامة تصحيح عامة لإدارة مستويات المخرجات.
- **تقديم تغذية راجعة واضحة** – غلّف الوظائف القياسية برسائل تصحيح مفيدة.
- **تعزيز المتانة** – تعامل مع المدخلات غير المتوقعة بأمان لمنع الأخطاء.
- **تبسيط استكشاف الأخطاء** – رسائل التصحيح المنظمة تسهّل تشخيص المشكلات وإصلاحها.

بهذا النهج، «تشرح سكربتاتنا نفسها» أثناء معالجة البيانات، فيقل الإحباط ويتحسّن كفاءة سير العمل. يصبح التصحيح أداة استباقية وليس عملًا تفاعليًا روتينيًا، فتصبح عملية البرمجة النصية أكثر سلاسة وفائدة.
