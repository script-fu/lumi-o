---
title: "إعادة الهيكلة مجددًا"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactor-again"
---
مع نمو المكتبة المساعدة، يصعب متابعتها بلمحة واحدة. أعد الهيكلة مجددًا لإبقاء كل وظيفة صغيرة ذات غرض واحد.

### تفكيك التعقيد

لتسهيل متابعة الوظيفة وصيانتها، قسّمها إلى وظائف أصغر ومركزة. ابدأ بفصل التحقق عن توجيه الرسائل.

### إنشاء وظيفة تحقق

يمكننا أخذ الجزء الذي يتحقق من الوسيطتين `message` و`output` ونقله إلى وظيفة منفصلة. بهذا لا تحتاج `send-message` للقلق بشأن التحقق، فيسهل فهمها.

```scheme
(define (validate-message message output)
  ;; التحقق من أن الرسالة سلسلة غير فارغة
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; التحقق من أن الإخراج إحدى الوجهات المتوقعة
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### تبسيط send-message

بعد نقل التحقق إلى وظيفة منفصلة، يمكن لـ `send-message` التركيز على إرسال الرسالة فقط. تصبح أبسط بكثير لأنها تتولى فقط توجيه الرسالة إلى الوجهة الصحيحة.

```scheme
(define (send-message message output)
  ;; استدعاء وظيفة التحقق قبل المتابعة
  (validate-message message output)

  (cond
    ;; الإرسال إلى وحدة تحكم الأخطاء
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; الإرسال إلى مربع حوار GUI
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; الإرسال إلى نافذة الطرفية
    ((eq? output 'terminal)
       (display message)))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))
```

### المزيد من التفصيل: فصل كل معالج مخرج

يمكن نقل كل نوع مخرج (GUI ووحدة تحكم الأخطاء والطرفية) إلى وظيفته الخاصة. يسهل ذلك الاختبار والتعديل والتوسع مستقبلًا.

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
  ;; الإرسال إلى المخرج المناسب
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))
```

### إعادة استخدام التحقق في كل وظيفة send-*

بما أن التحقق جزء مهم من ضمان صحة الرسالة والمخرج، من المنطقي أن تنفّذ كل وظيفة `send-*` تحققها الخاص. يضمن ذلك أننا نتحقق من المدخلات أولًا بغض النظر عن المخرج المستدعى.

```scheme
(define (send-to-gui message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'terminal)
  (display message))
```

لاحظ أننا أزلنا التحقق من `send-message` ونقلنا المسؤولية إلى كل وظيفة مخرج. يضمن ذلك أن كل وجهة (GUI ووحدة تحكم الأخطاء والطرفية) تتولى تحققها، فيُبسَّط `send-message` ويبقى منطق التحقق أقرب إلى موضع الحاجة.

يمكن لهذا الأسلوب تبسيط `send-message` لتصبح _مرسلًا_، مع ضمان أن كل `send-to-*` تتحقق من الرسالة قبل معالجتها.

بنقل التحقق إلى كل `send-to-*`، جعلناها قابلة لإعادة الاستخدام كوظائف مستقلة. يمكننا استدعاء `send-to-gui` أو `send-to-error-console` أو `send-to-terminal` مباشرة دون الاعتماد على `send-message`. كل وظيفة تتولى منطقها بالكامل ويمكن استخدامها بشكل مستقل في أجزاء أخرى من الكود أو في plug-ins أخرى، فيصبح الكود أكثر مرونة.

## فوائد إعادة الهيكلة

- **فصل واضح للاهتمامات**: كل وظيفة تتولى مسؤولية واحدة فقط، فيسهل فهم الكود.
- **قابلية التوسع**: إضافة أنواع مخرج جديدة سهل. عرّف وظيفة جديدة مثل `send-to-file` أو `send-to-logger`، ثم أضف فرعًا في `cond`.
- **قابلية إعادة الاستخدام**: كل وظيفة معالجة مخرج قابلة لإعادة الاستخدام في مشروعك أو مشاركتها بين plug-ins متعددة.
- **الاتساق**: بإعادة استخدام `validate-message` في كل `send-to-*`، تضمن التحقق من جميع المخرجات بشكل صحيح.

نسخة المكتبة بعد إعادة الهيكلة:

```scheme
;; الغرض: إرسال رسالة إلى مربع حوار GUI
(define (send-to-gui message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; الغرض: إرسال رسالة إلى وحدة تحكم الأخطاء
(define (send-to-error-console message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; الغرض: إرسال رسالة إلى نافذة الطرفية
(define (send-to-terminal message)
  ;; التحقق من الرسالة قبل المتابعة
  (validate-message message 'terminal)
  (display message))

;; الغرض: توجيه رسالة إلى وجهة الإخراج المناسبة
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))

;; الغرض: التحقق من أن الرسالة سلسلة غير فارغة وأن الإخراج صالح
(define (validate-message message output)
  ;; التحقق من أن الرسالة سلسلة غير فارغة
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; التحقق من أن الإخراج إحدى الوجهات المتوقعة
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

هل هذا كل ما يمكننا فعله؟ لا! هناك المزيد، تابع القراءة.
